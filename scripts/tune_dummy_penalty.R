suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
})

seasons <- c(2007:2019, 2022:2025)
normalize_ratio <- function(x) {
  round(as.numeric(x), 6)
}

primary_grid_path <-
  "results/intermediate/broad_model_grid.csv"
primary_configuration_path <-
  "results/intermediate/broad_configuration_summary.csv"

if (
  !file.exists(primary_grid_path) ||
    !file.exists(primary_configuration_path)
) {
  stop(
    "Run scripts/run_model_grid.R before penalty tuning.",
    call. = FALSE
  )
}

primary_grid <- read.csv(primary_grid_path) %>%
  mutate(
    dummy_penalty_ratio =
      normalize_ratio(dummy_penalty_ratio)
  )
primary_configuration <- read.csv(
  primary_configuration_path
) %>%
  mutate(
    dummy_penalty_ratio =
      normalize_ratio(dummy_penalty_ratio)
  ) %>%
  arrange(
    mean_neutral_inner_RMSE,
    min_minutes,
    desc(dummy_penalty_ratio)
  )

if (
  n_distinct(primary_grid$season) != length(seasons) ||
    !setequal(unique(primary_grid$season), seasons)
) {
  stop(
    "The broad threshold grid does not contain all 17 seasons.",
    call. = FALSE
  )
}

selected_threshold <- primary_configuration %>%
  slice(1) %>%
  pull(min_minutes)

fixed_candidate_ratios <- normalize_ratio(
  seq(1, 2.5, by = 0.1)
)
primary_candidate_ratios <- normalize_ratio(c(1, 2))
fine_candidate_ratios <- normalize_ratio(setdiff(
  seq(1, 2, by = 0.1),
  primary_candidate_ratios
))
upper_verification_ratios <- normalize_ratio(
  seq(2.1, 2.5, by = 0.1)
)

experiment_directory <-
  ".rapm_penalty_extension/final"
dir.create(
  experiment_directory,
  showWarnings = FALSE,
  recursive = TRUE
)
dir.create(
  "results/intermediate",
  showWarnings = FALSE,
  recursive = TRUE
)

detected_cores <- suppressWarnings(
  parallel::detectCores(logical = TRUE)
)
if (is.na(detected_cores)) {
  detected_cores <- suppressWarnings(
    as.integer(
      tryCatch(
        system2(
          "getconf",
          "_NPROCESSORS_ONLN",
          stdout = TRUE
        ),
        error = function(error) NA_character_
      )
    )
  )
}
if (is.na(detected_cores)) {
  detected_cores <- 1L
}
experiment_workers <- max(1L, detected_cores - 4L)
worker_override <- Sys.getenv("RAPM_WORKERS")
if (nzchar(worker_override)) {
  experiment_workers <- max(
    1L,
    as.integer(worker_override)
  )
}
experiment_workers <- min(
  experiment_workers,
  length(seasons)
)

cat(
  "Selected broad-grid threshold:",
  selected_threshold,
  "\n"
)
cat(
  "Penalty tuning using",
  experiment_workers,
  "season workers.\n"
)

ratio_label <- function(ratio) {
  gsub("\\.", "_", format(ratio, trim = TRUE))
}

batch_paths <- function(season, ratios, batch_label) {
  namespace <- paste0(
    "penalty_",
    batch_label,
    "_season_",
    season
  )
  output_prefix <- file.path(
    experiment_directory,
    paste0(namespace, "_")
  )
  list(
    namespace = namespace,
    output_prefix = output_prefix,
    model_grid = paste0(
      output_prefix,
      "model_grid.csv"
    ),
    log = file.path(
      experiment_directory,
      paste0(namespace, ".log")
    ),
    ratios = normalize_ratio(ratios)
  )
}

valid_batch_output <- function(path, season, ratios) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  output <- tryCatch(
    read.csv(path),
    error = function(error) NULL
  )
  if (
    is.null(output) ||
      !"dummy_penalty_ratio" %in% names(output)
  ) {
    return(FALSE)
  }
  output_ratios <- normalize_ratio(
    output$dummy_penalty_ratio
  )
  all(output$season == season) &&
    all(output$min_minutes == selected_threshold) &&
    setequal(unique(output_ratios), normalize_ratio(ratios))
}

run_fit_batch <- function(season, ratios, batch_label) {
  paths <- batch_paths(season, ratios, batch_label)

  if (
    valid_batch_output(
      paths$model_grid,
      season,
      paths$ratios
    )
  ) {
    return(tibble(
      season = season,
      dummy_penalty_ratio = paths$ratios,
      checkpoint_namespace = paths$namespace,
      model_grid_path = paths$model_grid
    ))
  }

  environment <- c(
    paste0("RAPM_SEASONS=", season),
    paste0("RAPM_THRESHOLDS=", selected_threshold),
    paste0(
      "RAPM_DUMMY_PENALTY_RATIOS=",
      paste(paths$ratios, collapse = ",")
    ),
    "RAPM_WORKERS=1",
    paste0(
      "RAPM_RUN_NAMESPACE=",
      paths$namespace
    ),
    paste0(
      "RAPM_OUTPUT_PREFIX=",
      paths$output_prefix
    )
  )

  status <- system2(
    command = "Rscript",
    args = "scripts/run_model_grid.R",
    stdout = paths$log,
    stderr = paths$log,
    env = environment,
    wait = TRUE
  )

  if (
    status != 0L ||
      !valid_batch_output(
        paths$model_grid,
        season,
        paths$ratios
      )
  ) {
    stop(
      paste(
        "Penalty fitting failed for season",
        season,
        "- see",
        paths$log
      ),
      call. = FALSE
    )
  }

  tibble(
    season = season,
    dummy_penalty_ratio = paths$ratios,
    checkpoint_namespace = paths$namespace,
    model_grid_path = paths$model_grid
  )
}

run_batches_across_seasons <- function(ratios, batch_label) {
  runner <- function(season) {
    run_fit_batch(season, ratios, batch_label)
  }

  if (
    experiment_workers > 1L &&
      .Platform$OS.type != "windows"
  ) {
    batch_sources <- parallel::mclapply(
      seasons,
      runner,
      mc.cores = experiment_workers,
      mc.preschedule = FALSE,
      mc.set.seed = TRUE
    )
  } else {
    batch_sources <- lapply(seasons, runner)
  }

  source_table <- bind_rows(batch_sources)
  batch_results <- source_table %>%
    distinct(model_grid_path) %>%
    pull(model_grid_path) %>%
    map_dfr(read.csv) %>%
    mutate(
      dummy_penalty_ratio =
        normalize_ratio(dummy_penalty_ratio)
    ) %>%
    filter(
      min_minutes == selected_threshold,
      dummy_penalty_ratio %in%
        normalize_ratio(ratios)
    )

  list(
    results = batch_results,
    sources = source_table
  )
}

summarize_penalty_grid <- function(grid) {
  coverage <- grid %>%
    distinct(season, dummy_penalty_ratio) %>%
    count(dummy_penalty_ratio, name = "n_seasons")
  if (
    any(coverage$n_seasons != length(seasons)) ||
      n_distinct(grid$season) != length(seasons)
  ) {
    stop(
      "Every penalty ratio must contain all 17 seasons.",
      call. = FALSE
    )
  }

  grid %>%
    group_by(dummy_penalty_ratio) %>%
    summarise(
      n_seasons = n(),
      mean_neutral_inner_RMSE =
        mean(neutral_inner_RMSE),
      mean_inner_dummy_RMSE =
        mean(inner_dummy_RMSE),
      mean_inner_no_dummy_RMSE =
        mean(inner_no_dummy_RMSE),
      mean_outer_dummy_RMSE =
        mean(outer_dummy_RMSE),
      mean_outer_no_dummy_RMSE =
        mean(outer_no_dummy_RMSE),
      mean_outer_RMSE_gain = mean(
        outer_no_dummy_RMSE - outer_dummy_RMSE
      ),
      outer_RMSE_wins = sum(
        outer_dummy_RMSE < outer_no_dummy_RMSE
      ),
      .groups = "drop"
    ) %>%
    arrange(
      mean_neutral_inner_RMSE,
      desc(dummy_penalty_ratio)
    )
}

primary_results <- primary_grid %>%
  filter(
    min_minutes == selected_threshold,
    dummy_penalty_ratio %in%
      primary_candidate_ratios
  )
if (
  nrow(primary_results) !=
    length(seasons) * length(primary_candidate_ratios)
) {
  stop(
    "Primary grid is missing the 1.0 or 2.0 penalty rows.",
    call. = FALSE
  )
}
primary_sources <- tidyr::crossing(
  season = seasons,
  dummy_penalty_ratio = primary_candidate_ratios
) %>%
  mutate(
    checkpoint_namespace = "",
    model_grid_path = primary_grid_path
  )

fine_batch <- run_batches_across_seasons(
  fine_candidate_ratios,
  "fine_1_0_to_2_0"
)
upper_verification_batches <- map(
  upper_verification_ratios,
  function(ratio) {
    run_batches_across_seasons(
      ratio,
      paste0("boundary_", ratio_label(ratio))
    )
  }
)

combined_results <- bind_rows(
  primary_results,
  fine_batch$results,
  map_dfr(
    upper_verification_batches,
    "results"
  )
) %>%
  distinct(
    season,
    min_minutes,
    dummy_penalty_ratio,
    .keep_all = TRUE
  )
source_table <- bind_rows(
  primary_sources,
  fine_batch$sources,
  map_dfr(
    upper_verification_batches,
    "sources"
  )
) %>%
  distinct(
    season,
    dummy_penalty_ratio,
    .keep_all = TRUE
  )

checked_ratios <- fixed_candidate_ratios
penalty_summary <- summarize_penalty_grid(
  combined_results
)
if (
  !setequal(
    penalty_summary$dummy_penalty_ratio,
    checked_ratios
  )
) {
  stop(
    "The fixed 1.0-2.5 penalty grid is incomplete.",
    call. = FALSE
  )
}
selected_ratio <- penalty_summary %>%
  slice(1) %>%
  pull(dummy_penalty_ratio)

boundary_iterations <- 0L
while (
  selected_ratio == min(checked_ratios) ||
    selected_ratio == max(checked_ratios)
) {
  boundary_iterations <- boundary_iterations + 1L
  if (boundary_iterations > 100L) {
    stop(
      "Penalty boundary extension exceeded 100 increments.",
      call. = FALSE
    )
  }

  direction <- if (
    selected_ratio == min(checked_ratios)
  ) {
    -1
  } else {
    1
  }
  next_ratio <- normalize_ratio(
    selected_ratio + 0.1 * direction
  )
  if (next_ratio < 0) {
    stop(
      "A nonnegative penalty ratio cannot be bracketed below zero.",
      call. = FALSE
    )
  }

  cat(
    "Boundary winner",
    selected_ratio,
    "- extending grid to",
    next_ratio,
    "\n"
  )
  boundary_batch <- run_batches_across_seasons(
    next_ratio,
    paste0("boundary_", ratio_label(next_ratio))
  )
  combined_results <- bind_rows(
    combined_results,
    boundary_batch$results
  ) %>%
    distinct(
      season,
      min_minutes,
      dummy_penalty_ratio,
      .keep_all = TRUE
    )
  source_table <- bind_rows(
    source_table,
    boundary_batch$sources
  ) %>%
    distinct(
      season,
      dummy_penalty_ratio,
      .keep_all = TRUE
    )
  checked_ratios <- sort(unique(c(
    checked_ratios,
    next_ratio
  )))
  penalty_summary <- summarize_penalty_grid(
    combined_results
  )
  selected_ratio <- penalty_summary %>%
    slice(1) %>%
    pull(dummy_penalty_ratio)
}

selected_results <- combined_results %>%
  filter(dummy_penalty_ratio == selected_ratio) %>%
  mutate(
    outer_RMSE_gain =
      outer_no_dummy_RMSE - outer_dummy_RMSE,
    outer_R2_gain =
      outer_dummy_R2 - outer_no_dummy_R2
  ) %>%
  arrange(season)

if (
  nrow(selected_results) != length(seasons) ||
    anyNA(selected_results)
) {
  stop(
    "Selected penalty results are incomplete.",
    call. = FALSE
  )
}

rmse_test <- t.test(selected_results$outer_RMSE_gain)
r2_test <- t.test(selected_results$outer_R2_gain)
wilcoxon_test <- wilcox.test(
  selected_results$outer_RMSE_gain,
  alternative = "two.sided",
  exact = FALSE
)
sign_test <- binom.test(
  sum(selected_results$outer_RMSE_gain > 0),
  nrow(selected_results),
  alternative = "two.sided"
)

selected_summary <- selected_results %>%
  summarise(
    selected_threshold = selected_threshold,
    selected_dummy_penalty_ratio = selected_ratio,
    checked_ratio_min = min(checked_ratios),
    checked_ratio_max = max(checked_ratios),
    boundary_iterations = boundary_iterations,
    winner_is_interior = (
      selected_ratio > min(checked_ratios) &&
        selected_ratio < max(checked_ratios)
    ),
    n_seasons = n(),
    mean_dummy_RMSE = mean(outer_dummy_RMSE),
    mean_no_dummy_RMSE = mean(outer_no_dummy_RMSE),
    mean_RMSE_gain = mean(outer_RMSE_gain),
    mean_relative_RMSE_gain = mean(
      outer_RMSE_gain / outer_no_dummy_RMSE
    ),
    RMSE_gain_CI_low = rmse_test$conf.int[1],
    RMSE_gain_CI_high = rmse_test$conf.int[2],
    paired_t_RMSE_statistic =
      unname(rmse_test$statistic),
    paired_t_RMSE_df = unname(rmse_test$parameter),
    paired_t_RMSE_p = rmse_test$p.value,
    wilcoxon_RMSE_statistic =
      unname(wilcoxon_test$statistic),
    wilcoxon_RMSE_p = wilcoxon_test$p.value,
    sign_test_RMSE_successes =
      sum(outer_RMSE_gain > 0),
    sign_test_RMSE_p = sign_test$p.value,
    RMSE_wins = sum(outer_RMSE_gain > 0),
    mean_dummy_R2 = mean(outer_dummy_R2),
    mean_no_dummy_R2 = mean(outer_no_dummy_R2),
    mean_R2_gain = mean(outer_R2_gain),
    R2_gain_CI_low = r2_test$conf.int[1],
    R2_gain_CI_high = r2_test$conf.int[2]
  )

if (!isTRUE(selected_summary$winner_is_interior)) {
  stop(
    "Selected penalty remains on an unchecked boundary.",
    call. = FALSE
  )
}

checkpoint_path <- function(season, namespace) {
  if (nzchar(namespace)) {
    file.path(
      ".rapm_checkpoints/final",
      namespace,
      paste0("rapm_season_", season, ".rds")
    )
  } else {
    file.path(
      ".rapm_checkpoints/final",
      paste0("rapm_season_", season, ".rds")
    )
  }
}

selected_source_table <- source_table %>%
  filter(dummy_penalty_ratio == selected_ratio)
if (nrow(selected_source_table) != length(seasons)) {
  stop(
    "Could not resolve one selected checkpoint per season.",
    call. = FALSE
  )
}

selected_dummy_coefficients <- map_dfr(
  seq_len(nrow(selected_source_table)),
  function(i) {
    source_row <- selected_source_table[i, ]
    path <- checkpoint_path(
      source_row$season,
      source_row$checkpoint_namespace
    )
    if (!file.exists(path)) {
      stop(
        paste("Missing selected checkpoint:", path),
        call. = FALSE
      )
    }
    readRDS(path)$coefficients %>%
      filter(
        season == source_row$season,
        min_minutes == selected_threshold,
        model == "dummy",
        normalize_ratio(dummy_penalty_ratio) ==
          selected_ratio
      )
  }
)

primary_checkpoints <- map(
  seasons,
  function(season) {
    path <- checkpoint_path(season, "")
    if (!file.exists(path)) {
      stop(
        paste("Missing primary checkpoint:", path),
        call. = FALSE
      )
    }
    readRDS(path)
  }
)

selected_no_dummy_coefficients <- map_dfr(
  primary_checkpoints,
  function(checkpoint) {
    checkpoint$coefficients %>%
      filter(
        min_minutes == selected_threshold,
        model == "no_dummy"
      )
  }
)
selected_coefficients <- bind_rows(
  selected_dummy_coefficients,
  selected_no_dummy_coefficients
) %>%
  arrange(model, season, term)

selected_category_exposure <- map_dfr(
  primary_checkpoints,
  function(checkpoint) {
    checkpoint$category_exposure %>%
      filter(min_minutes == selected_threshold)
  }
) %>%
  arrange(season, side, excluded_players)

sample_summary <- map_dfr(
  primary_checkpoints,
  "sample_summary"
) %>%
  arrange(season)

write.csv(
  combined_results %>%
    arrange(dummy_penalty_ratio, season),
  "results/intermediate/penalty_grid.csv",
  row.names = FALSE
)
write.csv(
  penalty_summary,
  "results/intermediate/penalty_summary.csv",
  row.names = FALSE
)
write.csv(
  selected_results,
  "results/intermediate/penalty_selected_results.csv",
  row.names = FALSE
)
write.csv(
  selected_summary,
  "results/intermediate/penalty_selected_summary.csv",
  row.names = FALSE
)
write.csv(
  selected_coefficients,
  "results/intermediate/penalty_selected_coefficients.csv",
  row.names = FALSE
)
write.csv(
  selected_category_exposure,
  "results/intermediate/penalty_selected_category_exposure.csv",
  row.names = FALSE
)
write.csv(
  sample_summary,
  "results/intermediate/penalty_sample_summary.csv",
  row.names = FALSE
)

print(penalty_summary, n = Inf)
print(as_tibble(selected_summary), n = Inf)

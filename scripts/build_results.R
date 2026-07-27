suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(purrr)
  library(tidyr)
})

seasons <- c(2007:2019, 2022:2025)

results_directory <- "results"
figures_directory <- "figures"
manuscript_directory <- "manuscript"

dir.create(results_directory, showWarnings = FALSE, recursive = TRUE)
dir.create(figures_directory, showWarnings = FALSE, recursive = TRUE)
dir.create(manuscript_directory, showWarnings = FALSE, recursive = TRUE)

primary_grid_path <-
  "results/intermediate/broad_model_grid.csv"
penalty_grid_path <-
  "results/intermediate/penalty_grid.csv"
penalty_summary_path <-
  "results/intermediate/penalty_summary.csv"
selected_results_path <-
  "results/intermediate/penalty_selected_results.csv"
selected_summary_path <-
  "results/intermediate/penalty_selected_summary.csv"
selected_coefficients_path <-
  "results/intermediate/penalty_selected_coefficients.csv"
selected_category_exposure_path <-
  paste0(
    "results/intermediate/",
    "penalty_selected_category_exposure.csv"
  )
sample_summary_path <-
  "results/intermediate/penalty_sample_summary.csv"
starter_exclusions_path <-
  "results/intermediate/broad_starter_exclusions.csv"

required_inputs <- c(
  primary_grid_path,
  penalty_grid_path,
  penalty_summary_path,
  selected_results_path,
  selected_summary_path,
  selected_coefficients_path,
  selected_category_exposure_path,
  sample_summary_path,
  starter_exclusions_path
)
missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs) > 0L) {
  stop(
    paste(
      "Missing required input files:",
      paste(missing_inputs, collapse = ", ")
    ),
    call. = FALSE
  )
}

primary_grid <- read.csv(primary_grid_path)
penalty_grid <- read.csv(penalty_grid_path) %>%
  mutate(
    dummy_penalty_ratio =
      round(dummy_penalty_ratio, 6)
  )
penalty_summary <- read.csv(penalty_summary_path) %>%
  mutate(
    dummy_penalty_ratio =
      round(dummy_penalty_ratio, 6)
  )
selection_summary <- read.csv(selected_summary_path)
selected_threshold <- selection_summary$selected_threshold[[1]]
selected_dummy_penalty_ratio <- round(
  selection_summary$selected_dummy_penalty_ratio[[1]],
  6
)

if (
  nrow(selection_summary) != 1L ||
    !isTRUE(selection_summary$winner_is_interior[[1]]) ||
    selection_summary$checked_ratio_max[[1]] < 2.5
) {
  stop(
    paste(
      "Penalty selection is missing, remains on a boundary,",
      "or does not include the fixed grid through 2.5."
    ),
    call. = FALSE
  )
}

required_penalty_ratios <- round(seq(1, 2.5, by = 0.1), 6)
if (
  !all(
    required_penalty_ratios %in%
      penalty_summary$dummy_penalty_ratio
  )
) {
  stop(
    "Penalty results do not contain every ratio from 1.0 through 2.5.",
    call. = FALSE
  )
}

selected_by_inner <- penalty_summary %>%
  arrange(
    mean_neutral_inner_RMSE,
    desc(dummy_penalty_ratio)
  ) %>%
  slice(1) %>%
  pull(dummy_penalty_ratio)
if (
  !isTRUE(all.equal(
    selected_by_inner,
    selected_dummy_penalty_ratio
  ))
) {
  stop(
    "Selected ratio does not match the inner-validation winner.",
    call. = FALSE
  )
}

selected_rows <- read.csv(selected_results_path) %>%
  mutate(
    dummy_penalty_ratio =
      round(dummy_penalty_ratio, 6)
  ) %>%
  arrange(season)

if (
  nrow(selected_rows) != length(seasons) ||
    !setequal(selected_rows$season, seasons) ||
    anyNA(
      selected_rows[
        c(
          "outer_dummy_RMSE",
          "outer_no_dummy_RMSE",
          "outer_dummy_R2",
          "outer_no_dummy_R2"
        )
      ]
    )
) {
  stop(
    "Selected penalty results do not contain all 17 seasons.",
    call. = FALSE
  )
}

season_results <- bind_rows(
  selected_rows %>%
    transmute(
      season,
      min_minutes,
      dummy_penalty_ratio,
      model_type = "dummy",
      RMSE = outer_dummy_RMSE,
      R2 = outer_dummy_R2
    ),
  selected_rows %>%
    transmute(
      season,
      min_minutes,
      dummy_penalty_ratio,
      model_type = "no_dummy",
      RMSE = outer_no_dummy_RMSE,
      R2 = outer_no_dummy_R2
    )
) %>%
  arrange(model_type, season)

paired_results <- season_results %>%
  select(season, model_type, RMSE, R2) %>%
  pivot_wider(
    names_from = model_type,
    values_from = c(RMSE, R2)
  ) %>%
  mutate(
    RMSE_gain = RMSE_no_dummy - RMSE_dummy,
    relative_RMSE_gain = RMSE_gain / RMSE_no_dummy,
    R2_gain = R2_dummy - R2_no_dummy
  ) %>%
  arrange(season)

rmse_test <- t.test(paired_results$RMSE_gain)
r2_test <- t.test(paired_results$R2_gain)
wilcoxon_test <- wilcox.test(
  paired_results$RMSE_gain,
  alternative = "two.sided",
  exact = FALSE
)
sign_test <- binom.test(
  sum(paired_results$RMSE_gain > 0),
  nrow(paired_results),
  alternative = "two.sided"
)

model_summary <- season_results %>%
  group_by(model_type) %>%
  summarise(
    mean_RMSE = mean(RMSE),
    sd_RMSE = sd(RMSE),
    mean_R2 = mean(R2),
    sd_R2 = sd(R2),
    n_seasons = n(),
    .groups = "drop"
  )

paired_summary <- paired_results %>%
  summarise(
    n_seasons = n(),
    dummy_RMSE_wins = sum(RMSE_gain > 0),
    mean_RMSE_gain = mean(RMSE_gain),
    median_RMSE_gain = median(RMSE_gain),
    mean_relative_RMSE_gain = mean(
      relative_RMSE_gain
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
      sum(RMSE_gain > 0),
    sign_test_RMSE_p = sign_test$p.value,
    mean_R2_gain = mean(R2_gain),
    R2_gain_CI_low = r2_test$conf.int[1],
    R2_gain_CI_high = r2_test$conf.int[2],
    paired_t_R2_p = r2_test$p.value
  )

zero_threshold_results <- primary_grid %>%
  filter(min_minutes == 0) %>%
  arrange(season, dummy_penalty_ratio) %>%
  group_by(season) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(
    season,
    zero_threshold_no_dummy_RMSE = outer_no_dummy_RMSE
  )

if (
  nrow(zero_threshold_results) != length(seasons) ||
    !setequal(zero_threshold_results$season, seasons)
) {
  stop(
    "Zero-threshold baseline does not contain all 17 seasons.",
    call. = FALSE
  )
}

zero_threshold_identity <- primary_grid %>%
  filter(min_minutes == 0) %>%
  transmute(
    inner_lambda =
      inner_dummy_lambda == inner_no_dummy_lambda,
    outer_lambda =
      outer_dummy_lambda == outer_no_dummy_lambda,
    inner_rmse =
      inner_dummy_RMSE == inner_no_dummy_RMSE,
    outer_rmse =
      outer_dummy_RMSE == outer_no_dummy_RMSE,
    inner_r2 =
      inner_dummy_R2 == inner_no_dummy_R2,
    outer_r2 =
      outer_dummy_R2 == outer_no_dummy_R2
  )
if (!all(as.matrix(zero_threshold_identity))) {
  stop(
    "Threshold-zero results are not exactly identical.",
    call. = FALSE
  )
}

summarize_rmse_comparison <- function(
  comparison,
  comparison_data
) {
  comparison_test <- t.test(comparison_data$RMSE_gain)

  tibble(
    comparison = comparison,
    n_seasons = nrow(comparison_data),
    mean_RMSE_gain = mean(comparison_data$RMSE_gain),
    RMSE_gain_CI_low = comparison_test$conf.int[1],
    RMSE_gain_CI_high = comparison_test$conf.int[2],
    paired_t_p = comparison_test$p.value,
    RMSE_wins = sum(comparison_data$RMSE_gain > 0)
  )
}

baseline_comparisons <- selected_rows %>%
  select(
    season,
    selected_dummy_RMSE = outer_dummy_RMSE,
    selected_no_dummy_RMSE = outer_no_dummy_RMSE
  ) %>%
  left_join(zero_threshold_results, by = "season")

sensitivity_summary <- bind_rows(
  summarize_rmse_comparison(
    "dummy_vs_no_dummy",
    paired_results %>% select(season, RMSE_gain)
  ),
  summarize_rmse_comparison(
    "selected_no_dummy_vs_zero_threshold_no_dummy",
    baseline_comparisons %>%
      transmute(
        season,
        RMSE_gain =
          zero_threshold_no_dummy_RMSE -
          selected_no_dummy_RMSE
      )
  ),
  summarize_rmse_comparison(
    "selected_dummy_vs_zero_threshold_no_dummy",
    baseline_comparisons %>%
      transmute(
        season,
        RMSE_gain =
          zero_threshold_no_dummy_RMSE -
          selected_dummy_RMSE
      )
  ),
  summarize_rmse_comparison(
    "dummy_vs_no_dummy_excluding_2019",
    paired_results %>%
      filter(season != 2019) %>%
      select(season, RMSE_gain)
  )
)

selected_coefficients <- read.csv(
  selected_coefficients_path
) %>%
  mutate(
    dummy_penalty_ratio =
      round(dummy_penalty_ratio, 6)
  )

dummy_coefficients <- selected_coefficients %>%
  filter(
    model == "dummy",
    min_minutes == selected_threshold,
    dummy_penalty_ratio ==
      selected_dummy_penalty_ratio
  )

no_dummy_coefficients <- selected_coefficients %>%
  filter(
    model == "no_dummy",
    min_minutes == selected_threshold
  )

selected_coefficients <- bind_rows(
  dummy_coefficients,
  no_dummy_coefficients
  ) %>%
  arrange(model, season, term)

if (
  n_distinct(dummy_coefficients$season) != length(seasons) ||
    n_distinct(no_dummy_coefficients$season) != length(seasons)
) {
  stop(
    "Selected coefficients do not contain all 17 seasons.",
    call. = FALSE
  )
}

dummy_term_check <- dummy_coefficients %>%
  filter(
    term_type %in% c("home_dummy", "away_dummy")
  ) %>%
  count(season, name = "n_dummy_terms")
if (
  nrow(dummy_term_check) != length(seasons) ||
    any(dummy_term_check$n_dummy_terms != 10L)
) {
  stop(
    "Each selected season must contain ten dummy coefficients.",
    call. = FALSE
  )
}

selected_category_exposure <- read.csv(
  selected_category_exposure_path
) %>%
  arrange(season, side, excluded_players)
sample_summary <- read.csv(sample_summary_path) %>%
  arrange(season)
if (
  nrow(sample_summary) != length(seasons) ||
    !setequal(sample_summary$season, seasons)
) {
  stop(
    "Sample summary does not contain all 17 seasons.",
    call. = FALSE
  )
}

collapse_season_ranges <- function(x) {
  x <- sort(unique(as.integer(x)))
  if (length(x) == 0L) {
    return("None")
  }
  groups <- split(
    x,
    cumsum(c(TRUE, diff(x) != 1L))
  )
  paste(
    vapply(
      groups,
      function(group) {
        if (length(group) >= 3L) {
          sprintf("%d--%d", group[[1]], tail(group, 1))
        } else {
          paste(group, collapse = ", ")
        }
      },
      character(1)
    ),
    collapse = ", "
  )
}

category_exposure_summary <- selected_category_exposure %>%
  group_by(side, excluded_players) %>%
  summarise(
    n_seasons_present = sum(n_stints > 0),
    seasons_present = paste(
      season[n_stints > 0],
      collapse = ", "
    ),
    seasons_present_compact = collapse_season_ranges(
      season[n_stints > 0]
    ),
    n_stints = sum(n_stints),
    possession_exposure = sum(possession_exposure),
    .groups = "drop"
  ) %>%
  arrange(match(side, c("Home", "Away")), excluded_players)

if (
  nrow(category_exposure_summary) != 12L ||
    any(
      category_exposure_summary$n_seasons_present !=
        lengths(
          strsplit(
            category_exposure_summary$seasons_present,
            ", ",
            fixed = TRUE
          )
        )
    ) ||
    !setequal(
      category_exposure_summary$excluded_players,
      0:5
    )
) {
  stop(
    "Category exposure summary must contain counts 0-5 by side.",
    call. = FALSE
  )
}

dummy_coefficient_summary <- dummy_coefficients %>%
  filter(
    term_type %in% c("home_dummy", "away_dummy")
  ) %>%
  mutate(
    side = if_else(
      term_type == "home_dummy",
      "Home",
      "Away"
    ),
    excluded_players =
      as.integer(sub(".*_", "", term))
  ) %>%
  group_by(side, excluded_players) %>%
  summarise(
    n_seasons = n(),
    mean_coefficient = mean(coefficient),
    sd_coefficient = sd(coefficient),
    median_coefficient = median(coefficient),
    negative_seasons = sum(coefficient < -1e-12),
    positive_seasons = sum(coefficient > 1e-12),
    zero_seasons = sum(abs(coefficient) <= 1e-12),
    se_coefficient = sd_coefficient / sqrt(n_seasons),
    ci_half_width =
      qt(0.975, df = n_seasons - 1) * se_coefficient,
    .groups = "drop"
  )

starter_exclusions <- read.csv(starter_exclusions_path)

direction_summary <- function(
  negative_seasons,
  positive_seasons,
  zero_seasons
) {
  parts <- character()
  if (negative_seasons > 0L) {
    parts <- c(parts, paste(negative_seasons, "negative"))
  }
  if (positive_seasons > 0L) {
    parts <- c(parts, paste(positive_seasons, "positive"))
  }
  if (zero_seasons > 0L) {
    parts <- c(parts, paste(zero_seasons, "zero"))
  }
  paste(parts, collapse = ", ")
}

dummy_coefficient_table <- dummy_coefficient_summary %>%
  mutate(
    side_order = match(side, c("Home", "Away")),
    directional_seasons = pmap_chr(
      list(
        negative_seasons,
        positive_seasons,
        zero_seasons
      ),
      direction_summary
    )
  ) %>%
  arrange(side_order, excluded_players)

dummy_coefficient_rows <- sprintf(
  "%s & %d & %.2f & %s \\\\",
  dummy_coefficient_table$side,
  dummy_coefficient_table$excluded_players,
  dummy_coefficient_table$mean_coefficient,
  dummy_coefficient_table$directional_seasons
)

writeLines(
  c(
    "\\begin{tabular}{lrrr}",
    "\\toprule",
    paste(
      "Lineup side & Low-minute players &",
      "Mean coefficient & Directional seasons \\\\"
    ),
    "\\midrule",
    dummy_coefficient_rows,
    "\\bottomrule",
    "\\end{tabular}"
  ),
  file.path(
    manuscript_directory,
    "dummy_coefficient_table.tex"
  )
)

sample_summary_rows <- sprintf(
  paste0(
    "%d & %d & %d & %d & %d & %d & %d & %d & ",
    "%s & %d \\\\"
  ),
  sample_summary$season,
  sample_summary$total_regular_season_games,
  sample_summary$inner_train_games,
  sample_summary$inner_validation_games,
  sample_summary$outer_train_games,
  sample_summary$outer_test_games,
  sample_summary$out_of_window_games,
  sample_summary$starter_excluded_games,
  format(
    sample_summary$modeled_stints,
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  ),
  sample_summary$distinct_players
)
writeLines(
  c(
    "\\resizebox{\\linewidth}{!}{%",
    "\\begin{tabular}{rrrrrrrrrr}",
    "\\toprule",
    paste(
      "Season & All & Oct--Dec & Jan--Feb & Oct--Feb &",
      "Mar--Apr & Out & Starter excl. & Stints & Players \\\\"
    ),
    "\\midrule",
    sample_summary_rows,
    "\\bottomrule",
    "\\end{tabular}%",
    "}"
  ),
  file.path(
    manuscript_directory,
    "sample_summary_table.tex"
  )
)

category_exposure_rows <- sprintf(
  "%s & %d & %s & %s & %d & %s \\\\",
  category_exposure_summary$side,
  category_exposure_summary$excluded_players,
  format(
    category_exposure_summary$n_stints,
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  ),
  format(
    round(
      category_exposure_summary$possession_exposure,
      1
    ),
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE,
    nsmall = 1
  ),
  category_exposure_summary$n_seasons_present,
  category_exposure_summary$seasons_present_compact
)
writeLines(
  c(
    "\\begin{tabular}{lrrrrp{5.0cm}}",
    "\\toprule",
    paste(
      "Side & Count & Stints & Exposure &",
      "Seasons & Seasons present \\\\"
    ),
    "\\midrule",
    category_exposure_rows,
    "\\bottomrule",
    "\\end{tabular}"
  ),
  file.path(
    manuscript_directory,
    "category_exposure_table.tex"
  )
)

write.csv(
  primary_grid,
  file.path(results_directory, "model_grid.csv"),
  row.names = FALSE
)
write.csv(
  penalty_summary,
  file.path(results_directory, "penalty_tuning.csv"),
  row.names = FALSE
)
write.csv(
  selection_summary,
  file.path(results_directory, "selected_configuration.csv"),
  row.names = FALSE
)
write.csv(
  season_results,
  file.path(results_directory, "season_results.csv"),
  row.names = FALSE
)
write.csv(
  model_summary,
  file.path(results_directory, "model_summary.csv"),
  row.names = FALSE
)
write.csv(
  paired_results,
  file.path(results_directory, "paired_results.csv"),
  row.names = FALSE
)
write.csv(
  paired_summary,
  file.path(results_directory, "paired_summary.csv"),
  row.names = FALSE
)
write.csv(
  selected_coefficients,
  file.path(results_directory, "selected_coefficients.csv"),
  row.names = FALSE
)
write.csv(
  dummy_coefficient_summary,
  file.path(
    results_directory,
    "dummy_coefficient_summary.csv"
  ),
  row.names = FALSE
)
write.csv(
  starter_exclusions,
  file.path(results_directory, "starter_exclusions.csv"),
  row.names = FALSE
)
write.csv(
  sensitivity_summary,
  file.path(results_directory, "sensitivity_summary.csv"),
  row.names = FALSE
)
write.csv(
  sample_summary,
  file.path(results_directory, "sample_summary.csv"),
  row.names = FALSE
)
write.csv(
  selected_category_exposure,
  file.path(
    results_directory,
    "selected_category_exposure_by_season.csv"
  ),
  row.names = FALSE
)
write.csv(
  category_exposure_summary,
  file.path(
    results_directory,
    "dummy_category_exposure.csv"
  ),
  row.names = FALSE
)

rmse_by_season_plot <- ggplot(
  season_results,
  aes(
    x = season,
    y = RMSE,
    color = model_type,
    group = model_type
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(
      dummy = "#D55E00",
      no_dummy = "#0072B2"
    ),
    labels = c(
      dummy = "Dummy RAPM",
      no_dummy = "Filtered RAPM"
    )
  ) +
  labs(
    x = "Season",
    y = "Game-margin RMSE",
    color = "Model"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

rmse_gain_plot <- ggplot(
  paired_results,
  aes(x = factor(season), y = RMSE_gain)
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey45"
  ) +
  geom_col(
    aes(fill = RMSE_gain > 0),
    width = 0.8
  ) +
  scale_fill_manual(
    values = c(
      `TRUE` = "#0072B2",
      `FALSE` = "#D55E00"
    ),
    guide = "none"
  ) +
  labs(
    x = "Season",
    y = "Filtered RAPM RMSE minus Dummy RAPM RMSE"
  ) +
  theme_minimal(base_size = 12)

dummy_coefficient_plot <- ggplot(
  dummy_coefficient_summary,
  aes(
    x = excluded_players,
    y = mean_coefficient,
    color = side
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey45"
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  geom_errorbar(
    aes(
      ymin = mean_coefficient - ci_half_width,
      ymax = mean_coefficient + ci_half_width
    ),
    width = 0.15
  ) +
  scale_x_continuous(breaks = 1:5) +
  scale_color_manual(
    values = c(Away = "#D55E00", Home = "#0072B2")
  ) +
  labs(
    x = "Low-minute players in lineup",
    y = "Points per 100 team possessions",
    color = "Lineup side"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

penalty_tuning_plot <- ggplot(
  penalty_summary,
  aes(
    x = dummy_penalty_ratio,
    y = mean_neutral_inner_RMSE
  )
) +
  geom_line(color = "#0072B2", linewidth = 0.8) +
  geom_point(color = "#0072B2", size = 2) +
  geom_vline(
    xintercept = selected_dummy_penalty_ratio,
    linetype = "dashed",
    color = "#D55E00"
  ) +
  scale_x_continuous(
    breaks = seq(
      min(penalty_summary$dummy_penalty_ratio),
      max(penalty_summary$dummy_penalty_ratio),
      by = 0.1
    )
  ) +
  labs(
    x = expression(lambda[D] / lambda[P]),
    y = "Mean inner-validation game RMSE"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(figures_directory, "rmse_by_season.png"),
  rmse_by_season_plot,
  width = 7.5,
  height = 4.8,
  dpi = 300
)
ggsave(
  file.path(figures_directory, "rmse_gain_by_season.png"),
  rmse_gain_plot,
  width = 7.5,
  height = 4.8,
  dpi = 300
)
ggsave(
  file.path(figures_directory, "dummy_coefficients.png"),
  dummy_coefficient_plot,
  width = 7.5,
  height = 4.8,
  dpi = 300
)
ggsave(
  file.path(figures_directory, "penalty_tuning.png"),
  penalty_tuning_plot,
  width = 7.5,
  height = 4.8,
  dpi = 300
)

dummy_model_summary <- model_summary %>%
  filter(model_type == "dummy")
no_dummy_model_summary <- model_summary %>%
  filter(model_type == "no_dummy")
home_one <- dummy_coefficient_summary %>%
  filter(side == "Home", excluded_players == 1)
away_one <- dummy_coefficient_summary %>%
  filter(side == "Away", excluded_players == 1)
no_dummy_baseline_comparison <- sensitivity_summary %>%
  filter(
    comparison ==
      "selected_no_dummy_vs_zero_threshold_no_dummy"
  )
dummy_baseline_comparison <- sensitivity_summary %>%
  filter(
    comparison ==
      "selected_dummy_vs_zero_threshold_no_dummy"
  )
excluding_2019_comparison <- sensitivity_summary %>%
  filter(
    comparison == "dummy_vs_no_dummy_excluding_2019"
  )
pre_2020_results <- paired_results %>%
  filter(season <= 2019)
post_covid_results <- paired_results %>%
  filter(season >= 2022)
starter_excluded_games <- n_distinct(starter_exclusions$game_id)
starter_excluded_2019_games <- starter_exclusions %>%
  filter(season == 2019) %>%
  summarise(n = n_distinct(game_id)) %>%
  pull(n)

format_p <- function(x) {
  if (x < 0.001) {
    return("<0.001")
  }
  sprintf("%.4f", x)
}

result_macros <- c(
  paste0(
    "\\newcommand{\\SelectedThreshold}{",
    selected_threshold,
    "}"
  ),
  paste0(
    "\\newcommand{\\SelectedPenaltyRatio}{",
    selected_dummy_penalty_ratio,
    "}"
  ),
  paste0(
    "\\newcommand{\\CheckedPenaltyMinimum}{",
    selection_summary$checked_ratio_min,
    "}"
  ),
  paste0(
    "\\newcommand{\\CheckedPenaltyMaximum}{",
    selection_summary$checked_ratio_max,
    "}"
  ),
  paste0(
    "\\newcommand{\\PenaltyBoundaryIterations}{",
    selection_summary$boundary_iterations,
    "}"
  ),
  paste0(
    "\\newcommand{\\NumberSeasons}{",
    length(seasons),
    "}"
  ),
  paste0(
    "\\newcommand{\\NumberSeasonsExcludingTwentyNineteen}{",
    length(seasons) - 1L,
    "}"
  ),
  paste0(
    "\\newcommand{\\MeanDummyRMSE}{",
    sprintf("%.3f", dummy_model_summary$mean_RMSE),
    "}"
  ),
  paste0(
    "\\newcommand{\\MeanNoDummyRMSE}{",
    sprintf("%.3f", no_dummy_model_summary$mean_RMSE),
    "}"
  ),
  paste0(
    "\\newcommand{\\MeanRMSEGain}{",
    sprintf("%.3f", paired_summary$mean_RMSE_gain),
    "}"
  ),
  paste0(
    "\\newcommand{\\RelativeRMSEGain}{",
    sprintf(
      "%.2f",
      100 * paired_summary$mean_relative_RMSE_gain
    ),
    "\\%}"
  ),
  paste0(
    "\\newcommand{\\RMSEGainCILow}{",
    sprintf("%.3f", paired_summary$RMSE_gain_CI_low),
    "}"
  ),
  paste0(
    "\\newcommand{\\RMSEGainCIHigh}{",
    sprintf("%.3f", paired_summary$RMSE_gain_CI_high),
    "}"
  ),
  paste0(
    "\\newcommand{\\RMSEWins}{",
    paired_summary$dummy_RMSE_wins,
    "}"
  ),
  paste0(
    "\\newcommand{\\NoDummyRMSEWins}{",
    length(seasons) - paired_summary$dummy_RMSE_wins,
    "}"
  ),
  paste0(
    "\\newcommand{\\PairedTPValue}{",
    format_p(paired_summary$paired_t_RMSE_p),
    "}"
  ),
  paste0(
    "\\newcommand{\\PairedTStatistic}{",
    sprintf(
      "%.3f",
      paired_summary$paired_t_RMSE_statistic
    ),
    "}"
  ),
  paste0(
    "\\newcommand{\\PairedTDegreesFreedom}{",
    sprintf(
      "%.0f",
      paired_summary$paired_t_RMSE_df
    ),
    "}"
  ),
  paste0(
    "\\newcommand{\\WilcoxonPValue}{",
    format_p(paired_summary$wilcoxon_RMSE_p),
    "}"
  ),
  paste0(
    "\\newcommand{\\WilcoxonStatistic}{",
    sprintf(
      "%.1f",
      paired_summary$wilcoxon_RMSE_statistic
    ),
    "}"
  ),
  paste0(
    "\\newcommand{\\SignTestPValue}{",
    format_p(paired_summary$sign_test_RMSE_p),
    "}"
  ),
  paste0(
    "\\newcommand{\\SignTestSuccesses}{",
    paired_summary$sign_test_RMSE_successes,
    "}"
  ),
  paste0(
    "\\newcommand{\\MeanDummyRSquared}{",
    sprintf("%.3f", dummy_model_summary$mean_R2),
    "}"
  ),
  paste0(
    "\\newcommand{\\MeanNoDummyRSquared}{",
    sprintf("%.3f", no_dummy_model_summary$mean_R2),
    "}"
  ),
  paste0(
    "\\newcommand{\\MeanRSquaredGain}{",
    sprintf("%.3f", paired_summary$mean_R2_gain),
    "}"
  ),
  paste0(
    "\\newcommand{\\HomeOneCoefficient}{",
    sprintf("%.2f", home_one$mean_coefficient),
    "}"
  ),
  paste0(
    "\\newcommand{\\AwayOneCoefficient}{",
    sprintf("%.2f", away_one$mean_coefficient),
    "}"
  ),
  paste0(
    "\\newcommand{\\HomeOneNegativeSeasons}{",
    home_one$negative_seasons,
    "}"
  ),
  paste0(
    "\\newcommand{\\AwayOnePositiveSeasons}{",
    away_one$positive_seasons,
    "}"
  ),
  paste0(
    "\\newcommand{\\StarterExcludedGames}{",
    starter_excluded_games,
    "}"
  ),
  paste0(
    "\\newcommand{\\StarterExcludedTwentyNineteenGames}{",
    starter_excluded_2019_games,
    "}"
  ),
  paste0(
    "\\newcommand{\\TotalModeledGames}{",
    sum(sample_summary$modeled_games),
    "}"
  ),
  paste0(
    "\\newcommand{\\TotalModeledStints}{",
    format(
      sum(sample_summary$modeled_stints),
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    ),
    "}"
  ),
  paste0(
    "\\newcommand{\\PreTwentyTwentyWins}{",
    sum(pre_2020_results$RMSE_gain > 0),
    "}"
  ),
  paste0(
    "\\newcommand{\\PreTwentyTwentySeasons}{",
    nrow(pre_2020_results),
    "}"
  ),
  paste0(
    "\\newcommand{\\PostCovidWins}{",
    sum(post_covid_results$RMSE_gain > 0),
    "}"
  ),
  paste0(
    "\\newcommand{\\PostCovidSeasons}{",
    nrow(post_covid_results),
    "}"
  ),
  paste0(
    "\\newcommand{\\ExcludingTwentyNineteenRMSEGain}{",
    sprintf(
      "%.3f",
      excluding_2019_comparison$mean_RMSE_gain
    ),
    "}"
  ),
  paste0(
    "\\newcommand{\\ExcludingTwentyNineteenPValue}{",
    format_p(excluding_2019_comparison$paired_t_p),
    "}"
  ),
  paste0(
    "\\newcommand{\\NoDummyBaselineRMSELoss}{",
    sprintf(
      "%.3f",
      -no_dummy_baseline_comparison$mean_RMSE_gain
    ),
    "}"
  ),
  paste0(
    "\\newcommand{\\NoDummyBaselinePValue}{",
    format_p(no_dummy_baseline_comparison$paired_t_p),
    "}"
  ),
  paste0(
    "\\newcommand{\\DummyBaselineRMSEGain}{",
    sprintf(
      "%.3f",
      dummy_baseline_comparison$mean_RMSE_gain
    ),
    "}"
  ),
  paste0(
    "\\newcommand{\\DummyBaselinePValue}{",
    format_p(dummy_baseline_comparison$paired_t_p),
    "}"
  )
)

writeLines(
  result_macros,
  file.path(manuscript_directory, "results.tex")
)

print(as_tibble(penalty_summary), n = Inf)
print(as_tibble(model_summary), n = Inf)
print(as_tibble(paired_summary), n = Inf)
print(as_tibble(sensitivity_summary), n = Inf)
print(as_tibble(dummy_coefficient_summary), n = Inf)

suppressPackageStartupMessages({
  library(dplyr)
  library(glmnet)
  library(Matrix)
})

split_lineup_key <- function(key) {
  if (is.na(key) || !nzchar(key)) {
    return(character())
  }

  players <- strsplit(key, "\\|", fixed = FALSE)[[1]]
  players[nzchar(players)]
}

find_incomplete_starter_team_games <- function(
  player_box_starters,
  play_by_play
) {
  expected_team_games <- bind_rows(
    play_by_play %>%
      distinct(game_id, team_id = home_team_id),
    play_by_play %>%
      distinct(game_id, team_id = away_team_id)
  ) %>%
    filter(!is.na(team_id)) %>%
    distinct(game_id, team_id)

  starter_counts <- player_box_starters %>%
    filter(
      !is.na(game_id),
      !is.na(team_id),
      !is.na(athlete_id),
      nzchar(as.character(athlete_id))
    ) %>%
    group_by(game_id, team_id) %>%
    summarise(
      n_starters = n_distinct(as.character(athlete_id)),
      .groups = "drop"
    )

  expected_team_games %>%
    left_join(
      starter_counts,
      by = c("game_id", "team_id")
    ) %>%
    mutate(n_starters = coalesce(n_starters, 0L)) %>%
    filter(n_starters != 5L) %>%
    arrange(game_id, team_id)
}

lineup_key_diagnostics <- function(base_stints) {
  bind_rows(
    tibble(
      game_id = base_stints$game_id,
      stint_id = base_stints$stint_id,
      side = "home",
      lineup = base_stints$home_lineup_key
    ),
    tibble(
      game_id = base_stints$game_id,
      stint_id = base_stints$stint_id,
      side = "away",
      lineup = base_stints$away_lineup_key
    )
  ) %>%
    rowwise() %>%
    mutate(
      n_players = length(split_lineup_key(lineup)),
      n_unique_players = n_distinct(split_lineup_key(lineup))
    ) %>%
    ungroup() %>%
    filter(n_players != 5L | n_unique_players != 5L)
}

validate_lineup_keys <- function(base_stints) {
  lineup_diagnostics <- lineup_key_diagnostics(base_stints)

  if (nrow(lineup_diagnostics) > 0L) {
    diagnostic_preview <- paste(
      capture.output(print(head(lineup_diagnostics, 10))),
      collapse = "\n"
    )
    stop(
      paste0(
        "Invalid five-player lineups detected in ",
        n_distinct(lineup_diagnostics$game_id),
        " games.\n",
        diagnostic_preview
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

collapse_zero_exposure_stints <- function(game_stints) {
  game_stints <- arrange(game_stints, stint_id)
  keep <- game_stints$exposure > 0

  if (!any(keep)) {
    stop("A game has no positive-exposure stints.", call. = FALSE)
  }

  positive_rows <- which(keep)
  assigned_points <- game_stints$home_net_points[positive_rows]
  pending_points <- 0
  positive_index <- 0L

  for (i in seq_len(nrow(game_stints))) {
    if (game_stints$exposure[i] <= 0) {
      pending_points <- pending_points + game_stints$home_net_points[i]
    } else {
      positive_index <- positive_index + 1L
      assigned_points[positive_index] <-
        assigned_points[positive_index] + pending_points
      pending_points <- 0
    }
  }

  if (pending_points != 0) {
    assigned_points[length(assigned_points)] <-
      assigned_points[length(assigned_points)] + pending_points
  }

  game_stints[positive_rows, ] %>%
    mutate(home_net_points = assigned_points)
}

build_stints <- function(base_play_by_play) {
  base_play_by_play %>%
    group_by(game_id) %>%
    arrange(game_play_number, .by_group = TRUE) %>%
    mutate(
      lineup_change = row_number() == 1L |
        lineup_pair != lag(lineup_pair),
      stint_id = cumsum(lineup_change),
      score_margin = home_score - away_score,
      signed_score_value = score_margin - lag(
        score_margin,
        default = 0
      )
    ) %>%
    group_by(game_id, stint_id) %>%
    summarise(
      home_lineup_key = first(home_lineup_key),
      away_lineup_key = first(away_lineup_key),
      n_possession_events = sum(possession_end, na.rm = TRUE),
      exposure = n_possession_events / 2,
      home_net_points = sum(signed_score_value, na.rm = TRUE),
      game_date = first(game_date),
      .groups = "drop"
    ) %>%
    group_by(game_id) %>%
    group_modify(~ collapse_zero_exposure_stints(.x)) %>%
    ungroup() %>%
    arrange(game_date, game_id, stint_id)
}

validate_stints <- function(base_stints, play_by_play_data_small) {
  model_stints <- base_stints %>%
    filter(exposure > 0)

  reconstructed <- model_stints %>%
    group_by(game_id) %>%
    summarise(
      reconstructed_margin = sum(home_net_points),
      .groups = "drop"
    )

  official <- play_by_play_data_small %>%
    group_by(game_id) %>%
    arrange(game_play_number, .by_group = TRUE) %>%
    summarise(
      official_margin = last(home_score) - last(away_score),
      .groups = "drop"
    )

  margin_check <- reconstructed %>%
    inner_join(official, by = "game_id") %>%
    mutate(error = reconstructed_margin - official_margin) %>%
    filter(error != 0)

  if (nrow(margin_check) > 0L) {
    diagnostic_preview <- paste(
      capture.output(print(head(margin_check, 10))),
      collapse = "\n"
    )
    stop(
      paste0(
        "Reconstructed stint margins do not equal official game margins in ",
        nrow(margin_check),
        " games.\n",
        diagnostic_preview
      ),
      call. = FALSE
    )
  }

  validate_lineup_keys(model_stints)
  model_stints
}

qualified_players_from_games <- function(
  player_game_seconds,
  training_game_ids,
  min_minutes
) {
  player_game_seconds %>%
    filter(game_id %in% training_game_ids) %>%
    group_by(all_players) %>%
    summarise(
      avg_minutes_per_game = mean(total_minutes, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(avg_minutes_per_game > min_minutes) %>%
    pull(all_players) %>%
    as.character()
}

calculate_player_game_minutes <- function(
  play_by_play_data_small
) {
  required_columns <- c(
    "game_id",
    "period_number",
    "game_play_number",
    "start_quarter_seconds_remaining",
    "possession_end",
    "home_lineup",
    "away_lineup"
  )
  missing_columns <- setdiff(
    required_columns,
    names(play_by_play_data_small)
  )
  if (length(missing_columns) > 0L) {
    stop(
      paste(
        "Missing player-minute columns:",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  possession_rows <- play_by_play_data_small %>%
    filter(possession_end) %>%
    mutate(
      period_length = if_else(
        period_number <= 4L,
        720,
        300
      )
    ) %>%
    group_by(game_id, period_number) %>%
    arrange(game_play_number, .by_group = TRUE) %>%
    mutate(
      prev_seconds_remaining = lag(
        start_quarter_seconds_remaining,
        default = first(period_length)
      ),
      possession_duration =
        prev_seconds_remaining -
        start_quarter_seconds_remaining
    ) %>%
    ungroup()

  invalid_durations <- possession_rows %>%
    filter(
      is.na(possession_duration) |
        possession_duration < 0 |
        possession_duration > period_length
    )
  if (nrow(invalid_durations) > 0L) {
    stop(
      "Invalid possession durations detected within a period.",
      call. = FALSE
    )
  }

  possession_rows %>%
    mutate(
      home_lineup = lapply(home_lineup, as.character),
      away_lineup = lapply(away_lineup, as.character)
    ) %>%
    rowwise() %>%
    mutate(
      all_players = list(
        c(home_lineup, away_lineup)
      )
    ) %>%
    ungroup() %>%
    tidyr::unnest(all_players) %>%
    group_by(game_id, all_players) %>%
    summarise(
      total_seconds = sum(possession_duration),
      .groups = "drop"
    ) %>%
    mutate(total_minutes = total_seconds / 60)
}

filter_noncontiguous_periods <- function(play_by_play) {
  required_columns <- c("game_id", "period_number")
  missing_columns <- setdiff(
    required_columns,
    names(play_by_play)
  )
  if (length(missing_columns) > 0L) {
    stop(
      paste(
        "Missing period-filter columns:",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  valid_period_limits <- play_by_play %>%
    filter(
      !is.na(game_id),
      !is.na(period_number),
      period_number > 0L
    ) %>%
    distinct(game_id, period_number) %>%
    group_by(game_id) %>%
    summarise(
      max_contiguous_period = {
        observed_periods <- sort(unique(period_number))
        candidate_periods <- seq_len(max(observed_periods))
        first_missing <- setdiff(
          candidate_periods,
          observed_periods
        )
        if (length(first_missing) == 0L) {
          max(observed_periods)
        } else {
          min(first_missing) - 1L
        }
      },
      .groups = "drop"
    )

  play_by_play %>%
    left_join(valid_period_limits, by = "game_id") %>%
    filter(
      is.na(period_number) |
        period_number <= max_contiguous_period
    ) %>%
    select(-max_contiguous_period)
}

build_design <- function(base_stints, qualified_players) {
  qualified_players <- sort(unique(as.character(qualified_players)))
  player_columns <- paste0("player__", qualified_players)
  home_dummy_columns <- paste0("home_dummy_", 1:5)
  away_dummy_columns <- paste0("away_dummy_", 1:5)
  dummy_columns <- c(home_dummy_columns, away_dummy_columns)

  player_matrix <- matrix(
    0,
    nrow = nrow(base_stints),
    ncol = length(player_columns),
    dimnames = list(NULL, player_columns)
  )
  dummy_matrix <- matrix(
    0,
    nrow = nrow(base_stints),
    ncol = length(dummy_columns),
    dimnames = list(NULL, dummy_columns)
  )

  for (i in seq_len(nrow(base_stints))) {
    home <- split_lineup_key(base_stints$home_lineup_key[i])
    away <- split_lineup_key(base_stints$away_lineup_key[i])

    retained_home <- intersect(home, qualified_players)
    retained_away <- intersect(away, qualified_players)

    if (length(retained_home) > 0L) {
      player_matrix[i, paste0("player__", retained_home)] <- 1
    }
    if (length(retained_away) > 0L) {
      player_matrix[i, paste0("player__", retained_away)] <- -1
    }

    n_home_dummies <- sum(!home %in% qualified_players)
    n_away_dummies <- sum(!away %in% qualified_players)

    if (n_home_dummies > 0L) {
      dummy_matrix[i, paste0("home_dummy_", n_home_dummies)] <- 1
    }
    if (n_away_dummies > 0L) {
      dummy_matrix[i, paste0("away_dummy_", n_away_dummies)] <- 1
    }
  }

  if (any(grepl("^player__.*dummy_", player_columns))) {
    stop("Player and dummy namespaces overlap.", call. = FALSE)
  }

  list(
    X_no_dummy = Matrix(player_matrix, sparse = TRUE),
    X_dummy = Matrix(cbind(player_matrix, dummy_matrix), sparse = TRUE),
    player_columns = player_columns,
    dummy_columns = dummy_columns
  )
}

make_game_folds <- function(game_ids, nfolds = 5L, seed = 1L) {
  unique_games <- unique(game_ids)
  nfolds <- min(nfolds, length(unique_games))

  if (nfolds < 3L) {
    stop("At least three games are required for grouped cross-validation.", call. = FALSE)
  }

  set.seed(seed)
  shuffled_games <- sample(unique_games)
  game_fold <- rep(seq_len(nfolds), length.out = length(shuffled_games))
  names(game_fold) <- as.character(shuffled_games)
  unname(game_fold[as.character(game_ids)])
}

analysis_window_seed <- function(season, window) {
  window_code <- switch(
    window,
    inner_train = 101L,
    outer_train = 211L,
    stop("Unknown training window.", call. = FALSE)
  )
  as.integer(season * 1000L + window_code)
}

assign_analysis_windows <- function(dated_games) {
  required_columns <- c("game_id", "game_date")
  missing_columns <- setdiff(required_columns, names(dated_games))
  if (length(missing_columns) > 0L) {
    stop(
      paste(
        "Missing dated-game columns:",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  game_dates <- dated_games %>%
    transmute(
      game_id = as.character(game_id),
      game_date = as.Date(game_date)
    ) %>%
    distinct()

  conflicting_dates <- game_dates %>%
    count(game_id, name = "n_dates") %>%
    filter(n_dates != 1L)
  if (nrow(conflicting_dates) > 0L) {
    stop("A game is associated with multiple dates.", call. = FALSE)
  }
  if (anyNA(game_dates$game_date)) {
    stop("Game dates cannot be missing.", call. = FALSE)
  }

  windows <- game_dates %>%
    mutate(
      month = as.integer(format(game_date, "%m")),
      analysis_window = case_when(
        month %in% c(10L, 11L, 12L) ~ "inner_train",
        month %in% c(1L, 2L) ~ "inner_validation",
        month %in% c(3L, 4L) ~ "outer_test",
        TRUE ~ "out_of_window"
      )
    ) %>%
    arrange(game_date, game_id)

  if (anyDuplicated(windows$game_id)) {
    stop("A game belongs to multiple analysis windows.", call. = FALSE)
  }

  inner_train_dates <- windows %>%
    filter(analysis_window == "inner_train") %>%
    pull(game_date)
  inner_validation_dates <- windows %>%
    filter(analysis_window == "inner_validation") %>%
    pull(game_date)
  outer_train_dates <- windows %>%
    filter(
      analysis_window %in% c(
        "inner_train",
        "inner_validation"
      )
    ) %>%
    pull(game_date)
  outer_test_dates <- windows %>%
    filter(analysis_window == "outer_test") %>%
    pull(game_date)

  if (
    length(inner_train_dates) > 0L &&
      length(inner_validation_dates) > 0L &&
      max(inner_train_dates) >= min(inner_validation_dates)
  ) {
    stop(
      "Inner training does not strictly precede inner validation.",
      call. = FALSE
    )
  }
  if (
    length(outer_train_dates) > 0L &&
      length(outer_test_dates) > 0L &&
      max(outer_train_dates) >= min(outer_test_dates)
  ) {
    stop(
      "Outer training does not strictly precede outer testing.",
      call. = FALSE
    )
  }

  windows
}

validate_foldid <- function(game_ids, foldid) {
  if (length(foldid) != length(game_ids)) {
    stop("foldid must have one value per observation.", call. = FALSE)
  }
  if (anyNA(foldid)) {
    stop("foldid cannot contain missing values.", call. = FALSE)
  }

  fold_diagnostics <- tibble(
    game_id = as.character(game_ids),
    foldid = as.integer(foldid)
  ) %>%
    distinct() %>%
    count(game_id, name = "n_folds") %>%
    filter(n_folds != 1L)

  if (nrow(fold_diagnostics) > 0L) {
    stop("All stints from a game must share one fold.", call. = FALSE)
  }
  if (n_distinct(foldid) < 3L) {
    stop("At least three folds are required.", call. = FALSE)
  }

  invisible(TRUE)
}

game_margin_rmse_by_lambda <- function(
  fit_preval,
  y,
  exposure,
  game_ids
) {
  if (is.null(dim(fit_preval))) {
    fit_preval <- matrix(fit_preval, ncol = 1L)
  }

  actual_margin <- rowsum(y * exposure / 100, game_ids, reorder = FALSE)
  predicted_margin <- apply(
    fit_preval,
    2,
    function(prediction) {
      rowsum(prediction * exposure / 100, game_ids, reorder = FALSE)[, 1]
    }
  )

  if (is.null(dim(predicted_margin))) {
    predicted_margin <- matrix(predicted_margin, ncol = 1L)
  }

  sqrt(colMeans((predicted_margin - actual_margin[, 1])^2, na.rm = TRUE))
}

fit_grouped_ridge <- function(
  X,
  y,
  exposure,
  game_ids,
  penalty_factor,
  foldid
) {
  validate_foldid(game_ids, foldid)

  cv_fit <- cv.glmnet(
    x = X,
    y = y,
    alpha = 0,
    weights = exposure,
    foldid = foldid,
    keep = TRUE,
    intercept = TRUE,
    standardize = FALSE,
    penalty.factor = penalty_factor
  )

  game_rmse <- game_margin_rmse_by_lambda(
    fit_preval = cv_fit$fit.preval,
    y = y,
    exposure = exposure,
    game_ids = game_ids
  )
  best_index <- which.min(game_rmse)

  list(
    fit = cv_fit$glmnet.fit,
    lambda = cv_fit$lambda[best_index],
    cv_game_rmse = game_rmse[best_index]
  )
}

evaluate_game_margins <- function(
  model,
  X,
  y,
  exposure,
  game_ids
) {
  predictions <- as.vector(
    predict(model$fit, newx = X, s = model$lambda)
  )

  tibble(
    game_id = game_ids,
    actual_y = y,
    predicted_y = predictions,
    exposure = exposure
  ) %>%
    group_by(game_id) %>%
    summarise(
      actual_margin = sum(actual_y * exposure / 100),
      predicted_margin = sum(predicted_y * exposure / 100),
      .groups = "drop"
    )
}

r_squared <- function(actual, predicted) {
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  1 - ss_res / ss_tot
}

model_metrics <- function(game_results) {
  tibble(
    RMSE = sqrt(mean(
      (game_results$actual_margin - game_results$predicted_margin)^2
    )),
    R2 = r_squared(
      game_results$actual_margin,
      game_results$predicted_margin
    )
  )
}

extract_model_coefficients <- function(
  model,
  season,
  min_minutes,
  model_type,
  dummy_penalty_ratio
) {
  coefficient_matrix <- as.matrix(
    coef(model$fit, s = model$lambda)
  )

  tibble(
    season = season,
    min_minutes = min_minutes,
    model = model_type,
    dummy_penalty_ratio = dummy_penalty_ratio,
    term = rownames(coefficient_matrix),
    coefficient = coefficient_matrix[, 1]
  ) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      term_type = case_when(
        grepl("^player__", term) ~ "player",
        grepl("^home_dummy_", term) ~ "home_dummy",
        grepl("^away_dummy_", term) ~ "away_dummy",
        TRUE ~ "other"
      ),
      player_id = if_else(
        term_type == "player",
        sub("^player__", "", term),
        NA_character_
      )
    )
}

append_zero_dummy_coefficients <- function(
  no_dummy_coefficients,
  dummy_penalty_ratio
) {
  required_columns <- c(
    "season",
    "min_minutes",
    "model",
    "dummy_penalty_ratio",
    "term",
    "coefficient",
    "term_type",
    "player_id"
  )
  missing_columns <- setdiff(
    required_columns,
    names(no_dummy_coefficients)
  )
  if (length(missing_columns) > 0L) {
    stop(
      paste(
        "Missing coefficient columns:",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (nrow(no_dummy_coefficients) == 0L) {
    stop("No no-dummy coefficients were supplied.", call. = FALSE)
  }

  dummy_terms <- c(
    paste0("home_dummy_", 1:5),
    paste0("away_dummy_", 1:5)
  )
  metadata <- no_dummy_coefficients[1, , drop = FALSE]
  zero_rows <- metadata[rep(1L, length(dummy_terms)), , drop = FALSE]
  zero_rows$term <- dummy_terms
  zero_rows$coefficient <- 0
  zero_rows$term_type <- c(
    rep("home_dummy", 5L),
    rep("away_dummy", 5L)
  )
  zero_rows$player_id <- NA_character_

  bind_rows(
    no_dummy_coefficients %>%
      mutate(
        model = "dummy",
        dummy_penalty_ratio = dummy_penalty_ratio
      ),
    zero_rows %>%
      mutate(
        model = "dummy",
        dummy_penalty_ratio = dummy_penalty_ratio
      )
  )
}

reuse_no_dummy_at_threshold_zero <- function(
  no_dummy_model,
  no_dummy_game_results,
  no_dummy_metrics,
  no_dummy_coefficients = NULL,
  dummy_penalty_ratio = NA_real_
) {
  zero_coefficients <- if (
    is.null(no_dummy_coefficients)
  ) {
    NULL
  } else {
    append_zero_dummy_coefficients(
      no_dummy_coefficients,
      dummy_penalty_ratio
    )
  }

  list(
    model = no_dummy_model,
    game_results = no_dummy_game_results,
    metrics = no_dummy_metrics,
    coefficients = zero_coefficients
  )
}

summarize_dummy_categories <- function(
  base_stints,
  qualified_players,
  stint_indices
) {
  selected_stints <- base_stints[stint_indices, , drop = FALSE]
  qualified_players <- as.character(qualified_players)

  category_rows <- bind_rows(
    tibble(
      side = "Home",
      excluded_players = vapply(
        selected_stints$home_lineup_key,
        function(lineup) {
          sum(!split_lineup_key(lineup) %in% qualified_players)
        },
        integer(1)
      ),
      exposure = selected_stints$exposure
    ),
    tibble(
      side = "Away",
      excluded_players = vapply(
        selected_stints$away_lineup_key,
        function(lineup) {
          sum(!split_lineup_key(lineup) %in% qualified_players)
        },
        integer(1)
      ),
      exposure = selected_stints$exposure
    )
  )

  category_rows %>%
    group_by(side, excluded_players) %>%
    summarise(
      n_stints = n(),
      possession_exposure = sum(exposure),
      .groups = "drop"
    ) %>%
    tidyr::complete(
      side = c("Home", "Away"),
      excluded_players = 0:5,
      fill = list(
        n_stints = 0L,
        possession_exposure = 0
      )
    ) %>%
    arrange(match(side, c("Home", "Away")), excluded_players)
}

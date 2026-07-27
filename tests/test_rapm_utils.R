source("R/rapm_utils.R")

starter_test_pbp <- tibble(
  game_id = c("complete", "incomplete", "missing"),
  home_team_id = c(1L, 3L, 5L),
  away_team_id = c(2L, 4L, 6L)
)
starter_test_box <- tibble(
  game_id = c(
    rep("complete", 10),
    rep("incomplete", 9),
    rep("missing", 5)
  ),
  team_id = c(
    rep(1L, 5),
    rep(2L, 5),
    rep(3L, 4),
    rep(4L, 5),
    rep(6L, 5)
  ),
  athlete_id = as.character(seq_len(24))
)
incomplete_starter_test <- find_incomplete_starter_team_games(
  starter_test_box,
  starter_test_pbp
)

stopifnot(nrow(incomplete_starter_test) == 2L)
stopifnot(
  all(
    incomplete_starter_test$game_id ==
      c("incomplete", "missing")
  )
)
stopifnot(all(incomplete_starter_test$team_id == c(3L, 5L)))
stopifnot(all(incomplete_starter_test$n_starters == c(4L, 0L)))

window_test_games <- tibble(
  game_id = paste0("window_", 1:8),
  game_date = as.Date(c(
    "2024-10-15",
    "2024-12-31",
    "2025-01-01",
    "2025-02-28",
    "2025-03-01",
    "2025-04-30",
    "2025-05-01",
    "2025-09-30"
  ))
)
window_assignments <- assign_analysis_windows(
  window_test_games
)

stopifnot(
  identical(
    window_assignments$analysis_window,
    c(
      "inner_train",
      "inner_train",
      "inner_validation",
      "inner_validation",
      "outer_test",
      "outer_test",
      "out_of_window",
      "out_of_window"
    )
  )
)
outer_train_test_games <- window_assignments %>%
  filter(
    analysis_window %in% c(
      "inner_train",
      "inner_validation"
    )
  ) %>%
  pull(game_id)
outer_test_test_games <- window_assignments %>%
  filter(analysis_window == "outer_test") %>%
  pull(game_id)

stopifnot(!"window_7" %in% outer_train_test_games)
stopifnot(!"window_7" %in% outer_test_test_games)
stopifnot(
  length(
    intersect(
      outer_train_test_games,
      outer_test_test_games
    )
  ) == 0L
)
stopifnot(
  max(
    window_assignments$game_date[
      window_assignments$game_id %in%
        outer_train_test_games
    ]
  ) <
    min(
      window_assignments$game_date[
        window_assignments$game_id %in%
          outer_test_test_games
      ]
    )
)

fold_game_ids <- rep(
  paste0("fold_game_", 1:10),
  each = 3
)
inner_seed <- analysis_window_seed(2025, "inner_train")
foldid_model_a <- make_game_folds(
  fold_game_ids,
  nfolds = 5L,
  seed = inner_seed
)
foldid_model_b <- make_game_folds(
  fold_game_ids,
  nfolds = 5L,
  seed = inner_seed
)

stopifnot(identical(foldid_model_a, foldid_model_b))
stopifnot(
  analysis_window_seed(2025, "inner_train") ==
    analysis_window_seed(2025, "inner_train")
)
stopifnot(
  analysis_window_seed(2025, "inner_train") !=
    analysis_window_seed(2025, "outer_train")
)
validate_foldid(fold_game_ids, foldid_model_a)
fold_group_check <- tibble(
  game_id = fold_game_ids,
  foldid = foldid_model_a
) %>%
  distinct() %>%
  count(game_id, name = "n_folds")
stopifnot(all(fold_group_check$n_folds == 1L))
stopifnot("foldid" %in% names(formals(fit_grouped_ridge)))
stopifnot(!"seed" %in% names(formals(fit_grouped_ridge)))

period_clock_test <- tibble(
  game_id = "clock_game",
  period_number = c(
    1L,
    1L,
    2L,
    2L,
    3L,
    3L,
    4L,
    4L,
    5L,
    5L
  ),
  game_play_number = 1:10,
  start_quarter_seconds_remaining = c(
    700,
    0,
    710,
    0,
    700,
    0,
    710,
    0,
    280,
    0
  ),
  possession_end = TRUE,
  home_lineup = rep(
    list(as.character(1:5)),
    10
  ),
  away_lineup = rep(
    list(as.character(6:10)),
    10
  )
)
period_clock_minutes <- calculate_player_game_minutes(
  period_clock_test
)

stopifnot(nrow(period_clock_minutes) == 10L)
stopifnot(
  all(
    period_clock_minutes$total_seconds ==
      720 + 720 + 720 + 720 + 300
  )
)
stopifnot(
  all(period_clock_minutes$total_minutes == 53)
)

period_filter_test <- tibble(
  game_id = c(
    rep("valid_double_ot", 6),
    rep("corrupt_period", 5)
  ),
  period_number = c(1:6, 1:4, 29L),
  event = seq_len(11)
)
filtered_period_test <- filter_noncontiguous_periods(
  period_filter_test
)

stopifnot(
  identical(
    filtered_period_test %>%
      filter(game_id == "valid_double_ot") %>%
      pull(period_number),
    1:6
  )
)
stopifnot(
  identical(
    filtered_period_test %>%
      filter(game_id == "corrupt_period") %>%
      pull(period_number),
    1:4
  )
)
stopifnot(
  !29L %in% filtered_period_test$period_number
)

test_stints <- tibble(
  game_id = c("g1", "g1", "g2"),
  stint_id = c(1L, 2L, 1L),
  home_lineup_key = c(
    "3|10|11|12|99",
    "3|10|11|50|51",
    "3|10|11|50|51"
  ),
  away_lineup_key = c(
    "20|21|22|23|24",
    "20|21|60|61|62",
    "20|21|60|61|62"
  )
)

qualified <- c("3", "10", "11", "12", "20", "21", "22", "23")
design <- build_design(test_stints, qualified)

stopifnot("player__3" %in% colnames(design$X_dummy))
stopifnot("home_dummy_1" %in% colnames(design$X_dummy))
stopifnot("away_dummy_1" %in% colnames(design$X_dummy))
stopifnot(design$X_dummy[1, "player__3"] == 1)
stopifnot(design$X_dummy[1, "home_dummy_1"] == 1)
stopifnot(design$X_dummy[1, "away_dummy_1"] == 1)
stopifnot(design$X_dummy[2, "home_dummy_2"] == 1)
stopifnot(design$X_dummy[2, "away_dummy_3"] == 1)

same_count_stint <- tibble(
  game_id = "g3",
  stint_id = 1L,
  home_lineup_key = "3|10|11|50|51",
  away_lineup_key = "20|21|22|60|61"
)
same_count_design <- build_design(same_count_stint, qualified)

stopifnot(same_count_design$X_dummy[1, "home_dummy_2"] == 1)
stopifnot(same_count_design$X_dummy[1, "away_dummy_2"] == 1)

category_stints <- test_stints %>%
  mutate(exposure = c(2, 3, 4))
category_summary <- summarize_dummy_categories(
  category_stints,
  qualified,
  seq_len(nrow(category_stints))
)
stopifnot(nrow(category_summary) == 12L)
stopifnot(
  setequal(category_summary$excluded_players, 0:5)
)
stopifnot(
  sum(
    category_summary$n_stints[
      category_summary$side == "Home"
    ]
  ) == nrow(category_stints)
)
stopifnot(
  sum(
    category_summary$possession_exposure[
      category_summary$side == "Away"
    ]
  ) == sum(category_stints$exposure)
)

fake_no_dummy_model <- list(
  lambda = 3.25,
  fit = list(identifier = "same_fit")
)
fake_no_dummy_game_results <- tibble(
  game_id = c("a", "b"),
  actual_margin = c(5, -2),
  predicted_margin = c(4.5, -1.5)
)
fake_no_dummy_metrics <- model_metrics(
  fake_no_dummy_game_results
)
fake_no_dummy_coefficients <- tibble(
  season = 2025,
  min_minutes = 0,
  model = "no_dummy",
  dummy_penalty_ratio = NA_real_,
  term = c("player__1", "player__2"),
  coefficient = c(0.5, -0.2),
  term_type = "player",
  player_id = c("1", "2")
)
threshold_zero_reuse <- reuse_no_dummy_at_threshold_zero(
  fake_no_dummy_model,
  fake_no_dummy_game_results,
  fake_no_dummy_metrics,
  fake_no_dummy_coefficients,
  dummy_penalty_ratio = 1.7
)

stopifnot(
  identical(
    threshold_zero_reuse$model,
    fake_no_dummy_model
  )
)
stopifnot(
  identical(
    threshold_zero_reuse$model$lambda,
    fake_no_dummy_model$lambda
  )
)
stopifnot(
  identical(
    threshold_zero_reuse$game_results$predicted_margin,
    fake_no_dummy_game_results$predicted_margin
  )
)
stopifnot(
  identical(
    threshold_zero_reuse$metrics,
    fake_no_dummy_metrics
  )
)
zero_dummy_rows <- threshold_zero_reuse$coefficients %>%
  filter(
    term_type %in% c("home_dummy", "away_dummy")
  )
stopifnot(nrow(zero_dummy_rows) == 10L)
stopifnot(all(zero_dummy_rows$coefficient == 0))
stopifnot(
  setequal(
    zero_dummy_rows$term,
    c(
      paste0("home_dummy_", 1:5),
      paste0("away_dummy_", 1:5)
    )
  )
)

scoring_plays <- tibble(
  game_id = c("g4", "g4", "g4"),
  game_play_number = 1:3,
  lineup_pair = c("a", "a", "b"),
  home_lineup_key = c(
    "1|2|3|4|5",
    "1|2|3|4|5",
    "1|2|3|4|6"
  ),
  away_lineup_key = rep("7|8|9|10|11", 3),
  score_value = c(3L, 2L, 2L),
  team_id = c(1L, 2L, 1L),
  home_team_id = rep(1L, 3),
  away_team_id = rep(2L, 3),
  home_score = c(3L, 3L, 5L),
  away_score = c(0L, 2L, 2L),
  possession_end = rep(TRUE, 3),
  game_date = as.Date(rep("2025-01-01", 3))
)
scoring_stints <- build_stints(scoring_plays)

stopifnot(scoring_stints$home_net_points[1] == 1)
stopifnot(scoring_stints$home_net_points[2] == 2)
stopifnot(sum(scoring_stints$home_net_points) == 3)

zero_exposure_stints <- tibble(
  game_id = "g5",
  stint_id = 1:4,
  exposure = c(1, 0, 0, 1),
  home_net_points = c(2, 1, -1, 3)
)
collapsed_stints <- collapse_zero_exposure_stints(zero_exposure_stints)

stopifnot(nrow(collapsed_stints) == 2)
stopifnot(sum(collapsed_stints$home_net_points) == 5)

message("All RAPM utility tests passed.")

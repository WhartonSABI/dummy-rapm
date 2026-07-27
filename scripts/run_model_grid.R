###############################################################
### GRID SEARCH MINIMUM AVERAGE TRAINING MINUTES BY SEASON ###
###############################################################

library(dplyr)
library(glmnet)
library(tidyr)
library(hoopR)
library(purrr)
library(zoo)
library(Matrix)
library(ggplot2)
library(stringr)

source("R/rapm_utils.R")

set.seed(1)
options(timeout = max(600, getOption("timeout", 60)))

data_cache_directory <- ".rapm_cache"
checkpoint_root_directory <- ".rapm_checkpoints/final"
run_namespace <- Sys.getenv("RAPM_RUN_NAMESPACE")
output_prefix <- Sys.getenv("RAPM_OUTPUT_PREFIX")
if (!nzchar(output_prefix)) {
  output_prefix <- "results/intermediate/broad_"
}
checkpoint_directory <- if (nzchar(run_namespace)) {
  file.path(checkpoint_root_directory, run_namespace)
} else {
  checkpoint_root_directory
}
checkpoint_version <- 5L

output_path <- function(filename) {
  path <- paste0(output_prefix, filename)
  dir.create(
    dirname(path),
    showWarnings = FALSE,
    recursive = TRUE
  )
  path
}

dir.create(
  data_cache_directory,
  showWarnings = FALSE,
  recursive = TRUE
)
dir.create(
  checkpoint_directory,
  showWarnings = FALSE,
  recursive = TRUE
)

load_hoopr_season_cached <- function(
  season,
  data_name,
  loader,
  required_columns,
  attempts = 4L
) {
  cache_path <- file.path(
    data_cache_directory,
    paste0(data_name, "_", season, ".rds")
  )

  is_valid <- function(data) {
    is.data.frame(data) &&
      nrow(data) > 0L &&
      all(required_columns %in% names(data))
  }

  if (file.exists(cache_path)) {
    cached_data <- tryCatch(
      readRDS(cache_path),
      error = function(error) NULL
    )

    if (is_valid(cached_data)) {
      cat("Loaded", data_name, season, "from local cache.\n")
      return(cached_data)
    }

    warning(
      paste("Ignoring invalid cache file", cache_path),
      call. = FALSE
    )
  }

  errors <- character()

  for (attempt in seq_len(attempts)) {
    loaded_data <- tryCatch(
      loader(seasons = season),
      error = function(error) {
        errors <<- c(errors, conditionMessage(error))
        NULL
      }
    )

    if (is_valid(loaded_data)) {
      temporary_cache <- tempfile(
        pattern = paste0(data_name, "_", season, "_"),
        tmpdir = data_cache_directory,
        fileext = ".rds"
      )
      saveRDS(loaded_data, temporary_cache)
      if (!file.rename(temporary_cache, cache_path)) {
        warning(
          paste("Could not finalize cache file", cache_path),
          call. = FALSE
        )
      }
      return(loaded_data)
    }

    missing_columns <- if (is.data.frame(loaded_data)) {
      setdiff(required_columns, names(loaded_data))
    } else {
      required_columns
    }
    errors <- c(
      errors,
      paste(
        "attempt",
        attempt,
        "returned invalid data; missing:",
        paste(missing_columns, collapse = ", ")
      )
    )

    if (attempt < attempts) {
      retry_delay <- min(15, 2^(attempt - 1))
      cat(
        "Retrying",
        data_name,
        season,
        "after",
        retry_delay,
        "seconds.\n"
      )
      Sys.sleep(retry_delay)
    }
  }

  stop(
    paste0(
      "Failed to load valid ",
      data_name,
      " data for season ",
      season,
      " after ",
      attempts,
      " attempts.\n",
      paste(unique(errors), collapse = "\n")
    ),
    call. = FALSE
  )
}

#################################################
### SEASONS TO RUN ##############################
#################################################

# Full seasons with sufficiently complete starter data and the same
# October-April calendar. The disrupted 2019-20 season and the
# COVID-shortened 2020-21 season are excluded.

seasons_to_run <- c(
  2007:2019,
  2022:2025
)

minutes_thresholds <- seq(0, 30, by = 5)
dummy_penalty_ratios <- c(0, 0.1, 0.25, 0.5, 1, 2)

season_override <- Sys.getenv("RAPM_SEASONS")
threshold_override <- Sys.getenv("RAPM_THRESHOLDS")
penalty_override <- Sys.getenv("RAPM_DUMMY_PENALTY_RATIOS")
worker_override <- Sys.getenv("RAPM_WORKERS")

if (nzchar(season_override)) {
  seasons_to_run <- as.integer(strsplit(season_override, ",")[[1]])
}
if (nzchar(threshold_override)) {
  minutes_thresholds <- as.numeric(
    strsplit(threshold_override, ",")[[1]]
  )
}
if (nzchar(penalty_override)) {
  dummy_penalty_ratios <- as.numeric(
    strsplit(penalty_override, ",")[[1]]
  )
}

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
rapm_workers <- max(1L, detected_cores - 4L)
if (nzchar(worker_override)) {
  rapm_workers <- max(1L, as.integer(worker_override))
}

cat(
  "Detected cores:", detected_cores,
  "| configured workers:", rapm_workers,
  "\n"
)

#################################################
### STORAGE #####################################
#################################################

all_season_results <- list()
all_season_coefs   <- list()
all_starter_exclusions <- list()
all_sample_summaries <- list()
all_category_exposure <- list()

checkpoint_specification <- list(
  version = checkpoint_version,
  minutes_thresholds = minutes_thresholds,
  dummy_penalty_ratios = dummy_penalty_ratios,
  split_scheme = list(
    inner_train_months = 10:12,
    inner_validation_months = 1:2,
    outer_train_months = c(10:12, 1:2),
    outer_test_months = 3:4,
    excluded_months = 5:9
  ),
  fold_scheme = list(
    grouped_by = "game_id",
    nfolds = 5L,
    seed_basis = "season_and_training_window_only"
  ),
  threshold_zero_scheme =
    "reuse_no_dummy_fit_and_append_ten_zero_coefficients"
)

#################################################
### R-SQUARED FUNCTION ##########################
#################################################

r_squared <- function(actual, predicted) {
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  1 - (ss_res / ss_tot)
}

#################################################
### LOOP THROUGH SEASONS ########################
#################################################

for (season in seasons_to_run) {
  checkpoint_path <- file.path(
    checkpoint_directory,
    paste0("rapm_season_", season, ".rds")
  )
  saved_checkpoint <- if (file.exists(checkpoint_path)) {
    tryCatch(
      readRDS(checkpoint_path),
      error = function(error) NULL
    )
  } else {
    NULL
  }

  if (
    is.list(saved_checkpoint) &&
      identical(
        saved_checkpoint$specification,
        checkpoint_specification
      )
  ) {
    all_season_results[[as.character(season)]] <-
      saved_checkpoint$results
    all_season_coefs[[as.character(season)]] <-
      saved_checkpoint$coefficients
    all_starter_exclusions[[as.character(season)]] <-
      saved_checkpoint$starter_exclusions
    all_sample_summaries[[as.character(season)]] <-
      saved_checkpoint$sample_summary
    all_category_exposure[[as.character(season)]] <-
      saved_checkpoint$category_exposure

    write.csv(
      bind_rows(all_starter_exclusions),
      output_path(
        "starter_exclusions.csv"
      ),
      row.names = FALSE
    )
    write.csv(
      bind_rows(all_sample_summaries),
      output_path("sample_summary.csv"),
      row.names = FALSE
    )
    write.csv(
      bind_rows(all_category_exposure),
      output_path("category_exposure.csv"),
      row.names = FALSE
    )

    cat(
      "\nLoaded completed season",
      season,
      "from checkpoint.\n"
    )
    next
  }
  
  cat("\n=====================================\n")
  cat("RUNNING SEASON:", season, "\n")
  cat("=====================================\n")
  
  ########################################
  ### LOAD PLAY BY PLAY ##################
  ########################################
  
  play_by_play_data <- load_hoopr_season_cached(
    season = season,
    data_name = "play_by_play",
    loader = load_nba_pbp,
    required_columns = c(
      "season_type",
      "team_id",
      "game_id",
      "home_team_id",
      "away_team_id",
      "home_score",
      "away_score",
      "game_date",
      "start_quarter_seconds_remaining"
    )
  )
  player_box_data <- load_hoopr_season_cached(
    season = season,
    data_name = "player_box",
    loader = load_nba_player_box,
    required_columns = c(
      "season_type",
      "team_id",
      "game_id",
      "athlete_id",
      "starter",
      "did_not_play"
    )
  ) %>%
    filter(
      season_type == 2,
      team_id %in% 1:30,
      starter,
      !did_not_play
    ) %>%
    mutate(
      athlete_id = trimws(as.character(athlete_id))
    )
  
  # Keep only regular season plays
  play_by_play_data <- play_by_play_data %>%
    filter(season_type == 2)

  play_by_play_data <- filter_noncontiguous_periods(
    play_by_play_data
  )
  
  # Keep only actual nba teams
  play_by_play_data <- play_by_play_data %>%
    filter(team_id %in% (1:30))

  regular_season_games <- play_by_play_data %>%
    transmute(
      game_id = as.character(game_id),
      game_date = as.Date(game_date)
    ) %>%
    distinct()
  regular_season_windows <- assign_analysis_windows(
    regular_season_games
  )

  incomplete_starters <- find_incomplete_starter_team_games(
    player_box_data,
    play_by_play_data
  ) %>%
    mutate(season = season, .before = 1)

  all_starter_exclusions[[as.character(season)]] <-
    incomplete_starters

  write.csv(
    bind_rows(all_starter_exclusions),
    output_path(
      "starter_exclusions.csv"
    ),
    row.names = FALSE
  )

  if (nrow(incomplete_starters) > 0L) {
    excluded_starter_games <- unique(
      incomplete_starters$game_id
    )

    cat(
      "Excluding",
      length(excluded_starter_games),
      "games with incomplete box-score starter data.\n"
    )
    print(
      incomplete_starters %>%
        count(n_starters, name = "n_team_games"),
      n = Inf
    )
    cat(
      "Excluded game IDs:",
      paste(
        head(excluded_starter_games, 20L),
        collapse = ", "
      ),
      if (length(excluded_starter_games) > 20L) "..." else "",
      "\n"
    )

    play_by_play_data <- play_by_play_data %>%
      filter(!game_id %in% excluded_starter_games)
    player_box_data <- player_box_data %>%
      filter(!game_id %in% excluded_starter_games)
  }
  
  # Sort by game date
  play_by_play_data <- play_by_play_data %>%
    arrange(game_date)
  
  ########################################
  ### FILTERING COLUMNS ##################
  ########################################
  
  play_by_play_data_small <- play_by_play_data %>%
    select(
      game_play_number,
      type_text,
      text,
      score_value,
      team_id,
      game_id,
      athlete_id_1,
      athlete_id_2,
      athlete_id_3,
      home_team_id,
      away_team_id,
      start_game_seconds_remaining,
      start_quarter_seconds_remaining,
      away_score,
      home_score,
      game_date,
      period_number
    )
  
  play_by_play_data_small <- play_by_play_data_small %>%
    mutate(
      athlete_id_1 = trimws(as.character(athlete_id_1)),
      athlete_id_2 = trimws(as.character(athlete_id_2)),
      athlete_id_3 = trimws(as.character(athlete_id_3))
    )
  
  ############################
  ### DEFINING POSSESSIONS ###
  ############################
  
  play_by_play_data_small <- play_by_play_data_small %>%
    group_by(game_id, period_number) %>%
    mutate(last_in_period = row_number() == n()) %>%
    ungroup() %>%
    mutate(
      possession_end = case_when(
        
        # End of period
        last_in_period ~ TRUE,
        
        # Made field goals
        score_value > 0 &
          grepl("Shot|Dunk|Layup", type_text) ~ TRUE,
        
        # Defensive rebound
        type_text == "Defensive Rebound" ~ TRUE,
        
        # Last free throw
        type_text %in% c(
          "Free Throw - 1 of 1",
          "Free Throw - 2 of 2",
          "Free Throw - 3 of 3",
          "Free Throw - Flagrant 1 of 1",
          "Free Throw - Flagrant 2 of 2",
          "Free Throw - Flagrant 3 of 3",
          "Free Throw - Clear Path 2 of 2"
        ) & score_value > 0 ~ TRUE,
        
        # Travel
        type_text == "Traveling" ~ TRUE,
        
        # Turnovers
        grepl("Turnover", type_text) &
          type_text != "No Turnover" ~ TRUE,
        
        TRUE ~ FALSE
      )
    ) %>%
    group_by(game_id) %>%
    mutate(
      possession_id = lag(cumsum(possession_end),
                          default = 0) + 1
    ) %>%
    ungroup()
  
  ################################################
  ### CREATE TEAM-SPECIFIC PLAYER EVENTS #########
  ################################################
  
  team_player_events <- play_by_play_data_small %>%
    mutate(
      athlete_id_1 = trimws(as.character(athlete_id_1)),
      athlete_id_2_team_player = case_when(
        grepl("assists", text) ~ athlete_id_2,
        TRUE ~ NA_character_
      )
    ) %>%
    pivot_longer(
      cols = c(
        athlete_id_1,
        athlete_id_2_team_player
      ),
      names_to = "id_source",
      values_to = "player_id"
    ) %>%
    filter(!is.na(player_id)) %>%
    transmute(
      game_id,
      period_number,
      game_play_number,
      team_id,
      player_id
    )
  
  ###########################################
  ### ADD STEALERS TO OPPOSING TEAM ROWS ###
  ###########################################
  
  game_teams <- play_by_play_data_small %>%
    filter(!is.na(game_id),
           !is.na(team_id)) %>%
    distinct(game_id, team_id)
  
  steal_events <- play_by_play_data_small %>%
    filter(grepl("steals", text)) %>%
    filter(!is.na(athlete_id_2)) %>%
    rename(turnover_team_id = team_id) %>%
    left_join(
      game_teams,
      by = "game_id",
      relationship = "many-to-many"
    ) %>%
    filter(team_id != turnover_team_id) %>%
    transmute(
      game_id,
      period_number,
      game_play_number,
      team_id = team_id,
      player_id = athlete_id_2
    )
  
  ###########################################
  ### COMBINE PLAYER APPEARANCE ROWS #######
  ###########################################
  
  all_player_events <- bind_rows(
    team_player_events,
    steal_events
  ) %>%
    mutate(player_id = trimws(as.character(player_id)))
  
  ###########################################
  ### BUILD INFERRED LINEUPS ###############
  ###########################################
  
  start_five <- player_box_data %>%
    group_by(game_id, team_id) %>%
    summarise(
      starting_five = list(
        sort(unique(athlete_id))
      ),
      .groups = "drop"
    ) %>%
    filter(lengths(starting_five) == 5L)
  
  play_by_play_data_small <- play_by_play_data_small %>%
    left_join(
      start_five,
      by = c(
        "game_id",
        "team_id"
      )
    ) %>%
    group_by(game_id, team_id) %>%
    mutate(
      starting_five = if_else(
        row_number() == 1,
        starting_five,
        NA
      )
    ) %>%
    ungroup()
  
  ###########################################
  ### HANDLE SUBSTITUTIONS ##################
  ###########################################
  
  play_by_play_data_small <- play_by_play_data_small %>%
    mutate(
      starting_five = map(
        starting_five,
        ~ if (is.null(.x)) NA else .x
      )
    )
  
  lineup_changes <- play_by_play_data_small %>%
    filter(
      !map_lgl(starting_five, is.null) |
        type_text == "Substitution"
    ) %>%
    arrange(
      game_id,
      team_id,
      game_play_number
    ) %>%
    group_by(game_id, team_id) %>%
    group_split()
  
  updated_lineup_list <- list()
  
  for (grp in lineup_changes) {
    
    n <- nrow(grp)
    
    updated_lineups <- vector("list", n)
    
    for (i in seq_len(n)) {
      
      if (
        !is.null(grp$starting_five[[i]]) &&
        length(grp$starting_five[[i]]) == 5
      ) {
        
        updated_lineups[[i]] <- trimws(
          as.character(grp$starting_five[[i]])
        )
        
      } else if (
        i > 1 &&
        !is.null(updated_lineups[[i - 1]])
      ) {
        
        updated_lineups[[i]] <- trimws(
          as.character(updated_lineups[[i - 1]])
        )
        
      } else {
        
        updated_lineups[[i]] <- NA
        
      }
      
      if (
        grp$type_text[i] == "Substitution" &&
        !is.na(grp$athlete_id_1[i]) &&
        !is.na(grp$athlete_id_2[i]) &&
        !all(is.na(updated_lineups[[i]]))
      ) {
        
        out_player <- trimws(
          as.character(grp$athlete_id_2[i])
        )
        
        in_player <- trimws(
          as.character(grp$athlete_id_1[i])
        )
        
        lineup <- trimws(
          as.character(updated_lineups[[i]])
        )
        
        in_already <- in_player %in% lineup
        out_present <- out_player %in% lineup
        
        if (in_already && out_present) {
          # Duplicate or repeated substitution record. Removing only the
          # outgoing player would create an impossible four-player lineup.
          updated_lineups[[i]] <- lineup
          
        } else if (!in_already && out_present) {
          
          idx <- which(lineup == out_player)
          lineup[idx] <- in_player
          updated_lineups[[i]] <- lineup
        }
      }
    }
    
    grp$updated_lineup <- updated_lineups
    
    updated_lineup_list <- append(
      updated_lineup_list,
      list(grp)
    )
  }
  
  lineup_changes_updated <- bind_rows(
    updated_lineup_list
  )
  
  play_by_play_data_small <- play_by_play_data_small %>%
    left_join(
      lineup_changes_updated %>%
        select(
          game_id,
          team_id,
          game_play_number,
          updated_lineup
        ),
      by = c(
        "game_id",
        "team_id",
        "game_play_number"
      )
    ) %>%
    group_by(game_id, team_id) %>%
    fill(updated_lineup,
         .direction = "down") %>%
    ungroup()
  
  ###########################################
  ### HOME / AWAY LINEUPS ##################
  ###########################################
  
  play_by_play_data_small <- play_by_play_data_small %>%
    mutate(
      home_lineup = case_when(
        team_id == home_team_id ~ updated_lineup,
        TRUE ~ NA
      ),
      away_lineup = case_when(
        team_id == away_team_id ~ updated_lineup,
        TRUE ~ NA
      )
    ) %>%
    group_by(game_id) %>%
    fill(home_lineup,
         away_lineup,
         .direction = "down") %>%
    fill(home_lineup,
         away_lineup,
         .direction = "up") %>%
    ungroup()
  
  ############################################
  ### MID-FREE-THROW SUBSTITUTIONS ###########
  ############################################
  
  ft_info <- play_by_play_data_small %>%
    filter(
      grepl("Free Throw", type_text),
      type_text != "Free Throw - Technical"
    ) %>%
    mutate(
      ft_attempt = as.integer(
        gsub(
          ".*(\\d) of (\\d).*",
          "\\1",
          type_text
        )
      ),
      ft_total = as.integer(
        gsub(
          ".*(\\d) of (\\d).*",
          "\\2",
          type_text
        )
      )
    )
  
  ft_first <- ft_info %>%
    filter(ft_attempt == 1L,
           ft_total >= 2L) %>%
    group_by(game_id) %>%
    mutate(ft_trip_id = row_number()) %>%
    ungroup() %>%
    select(
      game_id,
      ft_trip_id,
      lineup_at_ft1 = updated_lineup,
      home_lineup_at_ft1 = home_lineup,
      away_lineup_at_ft1 = away_lineup
    )
  
  ft_last <- ft_info %>%
    filter(ft_attempt == ft_total,
           ft_total >= 2L) %>%
    group_by(game_id) %>%
    mutate(ft_trip_id = row_number()) %>%
    ungroup() %>%
    select(
      game_id,
      ft_trip_id,
      game_play_number
    )
  
  ft_fix <- ft_last %>%
    left_join(
      ft_first,
      by = c(
        "game_id",
        "ft_trip_id"
      )
    )
  
  play_by_play_data_small <- play_by_play_data_small %>%
    left_join(
      ft_fix %>%
        select(
          game_id,
          game_play_number,
          lineup_at_ft1,
          home_lineup_at_ft1,
          away_lineup_at_ft1
        ),
      by = c(
        "game_id",
        "game_play_number"
      )
    )
  
  fix_rows <- which(
    !sapply(
      play_by_play_data_small$lineup_at_ft1,
      is.null
    )
  )
  
  play_by_play_data_small$updated_lineup[fix_rows] <-
    play_by_play_data_small$lineup_at_ft1[fix_rows]
  
  play_by_play_data_small$home_lineup[fix_rows] <-
    play_by_play_data_small$home_lineup_at_ft1[fix_rows]
  
  play_by_play_data_small$away_lineup[fix_rows] <-
    play_by_play_data_small$away_lineup_at_ft1[fix_rows]
  
  play_by_play_data_small <- play_by_play_data_small %>%
    select(
      -lineup_at_ft1,
      -home_lineup_at_ft1,
      -away_lineup_at_ft1
    )
  
  ###########################################
  ### FLAG GARBAGE TIME ####################
  ###########################################
  
  play_by_play_data_small <- play_by_play_data_small %>%
    mutate(
      score_difference = abs(
        home_score - away_score
      ),
      garbage_time = (
        score_difference > 15 &
          start_game_seconds_remaining < 120
      )
    )
  
  ###########################################
  ### PLAYER GAME MINUTES ###################
  ###########################################
  
  player_game_seconds <- calculate_player_game_minutes(
    play_by_play_data_small
  )
  
  ###########################################
  ### BUILD STINTS ##########################
  ###########################################
  
  base_play_by_play <- play_by_play_data_small %>%
    mutate(
      home_lineup_key = sapply(
        home_lineup,
        function(x) {
          paste(
            sort(
              trimws(
                as.character(x[!is.na(x)])
              )
            ),
            collapse = "|"
          )
        }
      ),
      away_lineup_key = sapply(
        away_lineup,
        function(x) {
          paste(
            sort(
              trimws(
                as.character(x[!is.na(x)])
              )
            ),
            collapse = "|"
          )
        }
      ),
      lineup_pair = paste(
        home_lineup_key,
        away_lineup_key,
        sep = "||"
      )
    )
  
  base_stints_raw <- build_stints(base_play_by_play)
  base_stints <- validate_stints(
    base_stints_raw,
    play_by_play_data_small
  )
  
  ###############################################
  ### CHRONOLOGICAL INNER / OUTER SPLITS ########
  ###############################################
  
  dated_games <- assign_analysis_windows(
    base_stints %>%
      select(game_id, game_date)
  )

  inner_train_games <- dated_games %>%
    filter(analysis_window == "inner_train") %>%
    pull(game_id)
  
  inner_validation_games <- dated_games %>%
    filter(analysis_window == "inner_validation") %>%
    pull(game_id)
  
  outer_test_games <- dated_games %>%
    filter(analysis_window == "outer_test") %>%
    pull(game_id)
  
  outer_train_games <- dated_games %>%
    filter(
      analysis_window %in% c(
        "inner_train",
        "inner_validation"
      )
    ) %>%
    pull(game_id)
  
  inner_train_idx <- which(
    base_stints$game_id %in% inner_train_games
  )
  inner_validation_idx <- which(
    base_stints$game_id %in% inner_validation_games
  )
  outer_train_idx <- which(
    base_stints$game_id %in% outer_train_games
  )
  outer_test_idx <- which(
    base_stints$game_id %in% outer_test_games
  )
  
  if (
    length(inner_train_idx) == 0L ||
      length(inner_validation_idx) == 0L ||
      length(outer_train_idx) == 0L ||
      length(outer_test_idx) == 0L
  ) {
    stop(
      paste("Incomplete chronological split for season", season),
      call. = FALSE
    )
  }

  if (
    length(intersect(inner_train_games, inner_validation_games)) > 0L ||
      length(intersect(outer_train_games, outer_test_games)) > 0L
  ) {
    stop(
      paste("Overlapping chronological windows for season", season),
      call. = FALSE
    )
  }

  inner_foldid <- make_game_folds(
    base_stints$game_id[inner_train_idx],
    nfolds = 5L,
    seed = analysis_window_seed(season, "inner_train")
  )
  outer_foldid <- make_game_folds(
    base_stints$game_id[outer_train_idx],
    nfolds = 5L,
    seed = analysis_window_seed(season, "outer_train")
  )
  validate_foldid(
    base_stints$game_id[inner_train_idx],
    inner_foldid
  )
  validate_foldid(
    base_stints$game_id[outer_train_idx],
    outer_foldid
  )

  may_games_excluded <- regular_season_windows %>%
    filter(month == 5L) %>%
    summarise(n = n_distinct(game_id)) %>%
    pull(n)
  analysis_game_ids <- c(
    outer_train_games,
    outer_test_games
  )
  analysis_stints <- base_stints %>%
    filter(game_id %in% analysis_game_ids)
  distinct_modeled_players <- unique(c(
    unlist(lapply(analysis_stints$home_lineup_key, split_lineup_key)),
    unlist(lapply(analysis_stints$away_lineup_key, split_lineup_key))
  ))
  starter_excluded_game_ids <- unique(
    as.character(incomplete_starters$game_id)
  )
  starter_excluded_in_analysis_window <-
    regular_season_windows %>%
    filter(
      game_id %in% starter_excluded_game_ids,
      analysis_window != "out_of_window"
    ) %>%
    summarise(n = n_distinct(game_id)) %>%
    pull(n)

  all_sample_summaries[[as.character(season)]] <- tibble(
    season = season,
    total_regular_season_games =
      n_distinct(regular_season_windows$game_id),
    inner_train_games = length(inner_train_games),
    inner_validation_games = length(inner_validation_games),
    outer_train_games = length(outer_train_games),
    outer_test_games = length(outer_test_games),
    out_of_window_games = regular_season_windows %>%
      filter(analysis_window == "out_of_window") %>%
      summarise(n = n_distinct(game_id)) %>%
      pull(n),
    may_games_excluded = may_games_excluded,
    starter_excluded_games = length(starter_excluded_game_ids),
    starter_excluded_in_analysis_window =
      starter_excluded_in_analysis_window,
    modeled_games = n_distinct(analysis_stints$game_id),
    modeled_stints = nrow(analysis_stints),
    distinct_players = length(distinct_modeled_players)
  )
  
  y <- 100 *
    base_stints$home_net_points /
    base_stints$exposure
  
  exposure <- base_stints$exposure
  
  ###########################################
  ### PLAYER NAME LOOKUP ####################
  ###########################################
  
  player_names <- play_by_play_data_small %>%
    filter(
      !is.na(athlete_id_1),
      !is.na(text),
      str_detect(type_text,
                 "Free Throw")
    ) %>%
    mutate(
      player_name = word(text, 1, 2)
    ) %>%
    distinct(
      athlete_id_1,
      .keep_all = TRUE
    ) %>%
    select(
      athlete_id_1,
      player_name
    ) %>%
    mutate(
      athlete_id_1 = as.character(
        athlete_id_1
      )
    )

  ##################################################
  ### NESTED CONFIGURATION SEARCH ##################
  ##################################################

  run_threshold <- function(min_minutes) {
    threshold_results <- list()
    threshold_coefficients <- list()

    qualified_inner <- qualified_players_from_games(
      player_game_seconds,
      inner_train_games,
      min_minutes
    )
    qualified_outer <- qualified_players_from_games(
      player_game_seconds,
      outer_train_games,
      min_minutes
    )

    inner_design <- build_design(
      base_stints,
      qualified_inner
    )
    outer_design <- build_design(
      base_stints,
      qualified_outer
    )
    threshold_category_exposure <- summarize_dummy_categories(
      base_stints = base_stints,
      qualified_players = qualified_outer,
      stint_indices = outer_train_idx
    ) %>%
      mutate(
        season = season,
        min_minutes = min_minutes,
        .before = 1
      )

    inner_no_dummy_model <- fit_grouped_ridge(
      X = inner_design$X_no_dummy[inner_train_idx, , drop = FALSE],
      y = y[inner_train_idx],
      exposure = exposure[inner_train_idx],
      game_ids = base_stints$game_id[inner_train_idx],
      penalty_factor = rep(1, ncol(inner_design$X_no_dummy)),
      foldid = inner_foldid
    )
    inner_no_dummy_game_results <- evaluate_game_margins(
      inner_no_dummy_model,
      inner_design$X_no_dummy[
        inner_validation_idx,
        ,
        drop = FALSE
      ],
      y[inner_validation_idx],
      exposure[inner_validation_idx],
      base_stints$game_id[inner_validation_idx]
    )
    inner_no_dummy_metrics <- model_metrics(
      inner_no_dummy_game_results
    )

    outer_no_dummy_model <- fit_grouped_ridge(
      X = outer_design$X_no_dummy[outer_train_idx, , drop = FALSE],
      y = y[outer_train_idx],
      exposure = exposure[outer_train_idx],
      game_ids = base_stints$game_id[outer_train_idx],
      penalty_factor = rep(1, ncol(outer_design$X_no_dummy)),
      foldid = outer_foldid
    )
    outer_no_dummy_game_results <- evaluate_game_margins(
      outer_no_dummy_model,
      outer_design$X_no_dummy[outer_test_idx, , drop = FALSE],
      y[outer_test_idx],
      exposure[outer_test_idx],
      base_stints$game_id[outer_test_idx]
    )
    outer_no_dummy_metrics <- model_metrics(
      outer_no_dummy_game_results
    )

    outer_no_dummy_coefficients <- extract_model_coefficients(
      outer_no_dummy_model,
      season,
      min_minutes,
      "no_dummy",
      NA_real_
    )
    threshold_coefficients[[paste(
      season,
      min_minutes,
      "no_dummy",
      sep = "_"
    )]] <- outer_no_dummy_coefficients

    for (dummy_penalty_ratio in dummy_penalty_ratios) {
      inner_penalty_factor <- c(
        rep(1, length(inner_design$player_columns)),
        rep(dummy_penalty_ratio, length(inner_design$dummy_columns))
      )
      outer_penalty_factor <- c(
        rep(1, length(outer_design$player_columns)),
        rep(dummy_penalty_ratio, length(outer_design$dummy_columns))
      )

      if (isTRUE(all.equal(min_minutes, 0))) {
        inner_zero <- reuse_no_dummy_at_threshold_zero(
          inner_no_dummy_model,
          inner_no_dummy_game_results,
          inner_no_dummy_metrics
        )
        outer_zero <- reuse_no_dummy_at_threshold_zero(
          outer_no_dummy_model,
          outer_no_dummy_game_results,
          outer_no_dummy_metrics,
          outer_no_dummy_coefficients,
          dummy_penalty_ratio
        )
        inner_dummy_model <- inner_zero$model
        outer_dummy_model <- outer_zero$model
        inner_dummy_game_results <- inner_zero$game_results
        outer_dummy_game_results <- outer_zero$game_results
        inner_dummy_metrics <- inner_zero$metrics
        outer_dummy_metrics <- outer_zero$metrics
        outer_dummy_coefficients <- outer_zero$coefficients

        if (
          !identical(
            inner_dummy_model$lambda,
            inner_no_dummy_model$lambda
          ) ||
            !identical(
              outer_dummy_model$lambda,
              outer_no_dummy_model$lambda
            ) ||
            !identical(
              inner_dummy_game_results,
              inner_no_dummy_game_results
            ) ||
            !identical(
              outer_dummy_game_results,
              outer_no_dummy_game_results
            ) ||
            !identical(
              inner_dummy_metrics,
              inner_no_dummy_metrics
            ) ||
            !identical(
              outer_dummy_metrics,
              outer_no_dummy_metrics
            )
        ) {
          stop(
            "Threshold-zero dummy and no-dummy results differ.",
            call. = FALSE
          )
        }
      } else {
        inner_dummy_model <- fit_grouped_ridge(
          X = inner_design$X_dummy[
            inner_train_idx,
            ,
            drop = FALSE
          ],
          y = y[inner_train_idx],
          exposure = exposure[inner_train_idx],
          game_ids = base_stints$game_id[inner_train_idx],
          penalty_factor = inner_penalty_factor,
          foldid = inner_foldid
        )
        inner_dummy_game_results <- evaluate_game_margins(
          inner_dummy_model,
          inner_design$X_dummy[
            inner_validation_idx,
            ,
            drop = FALSE
          ],
          y[inner_validation_idx],
          exposure[inner_validation_idx],
          base_stints$game_id[inner_validation_idx]
        )
        inner_dummy_metrics <- model_metrics(
          inner_dummy_game_results
        )

        outer_dummy_model <- fit_grouped_ridge(
          X = outer_design$X_dummy[
            outer_train_idx,
            ,
            drop = FALSE
          ],
          y = y[outer_train_idx],
          exposure = exposure[outer_train_idx],
          game_ids = base_stints$game_id[outer_train_idx],
          penalty_factor = outer_penalty_factor,
          foldid = outer_foldid
        )
        outer_dummy_game_results <- evaluate_game_margins(
          outer_dummy_model,
          outer_design$X_dummy[outer_test_idx, , drop = FALSE],
          y[outer_test_idx],
          exposure[outer_test_idx],
          base_stints$game_id[outer_test_idx]
        )
        outer_dummy_metrics <- model_metrics(
          outer_dummy_game_results
        )
        outer_dummy_coefficients <- extract_model_coefficients(
          outer_dummy_model,
          season,
          min_minutes,
          "dummy",
          dummy_penalty_ratio
        )
      }

      result_key <- paste(
        season,
        min_minutes,
        dummy_penalty_ratio,
        sep = "_"
      )

      threshold_results[[result_key]] <- tibble(
        season = season,
        min_minutes = min_minutes,
        dummy_penalty_ratio = dummy_penalty_ratio,
        n_players_inner = length(qualified_inner),
        n_players_outer = length(qualified_outer),
        inner_dummy_lambda = inner_dummy_model$lambda,
        inner_no_dummy_lambda = inner_no_dummy_model$lambda,
        inner_dummy_RMSE = inner_dummy_metrics$RMSE,
        inner_dummy_R2 = inner_dummy_metrics$R2,
        inner_no_dummy_RMSE = inner_no_dummy_metrics$RMSE,
        inner_no_dummy_R2 = inner_no_dummy_metrics$R2,
        neutral_inner_RMSE = (
          inner_dummy_metrics$RMSE +
            inner_no_dummy_metrics$RMSE
        ) / 2,
        outer_dummy_lambda = outer_dummy_model$lambda,
        outer_no_dummy_lambda = outer_no_dummy_model$lambda,
        outer_dummy_RMSE = outer_dummy_metrics$RMSE,
        outer_dummy_R2 = outer_dummy_metrics$R2,
        outer_no_dummy_RMSE = outer_no_dummy_metrics$RMSE,
        outer_no_dummy_R2 = outer_no_dummy_metrics$R2
      )

      threshold_coefficients[[paste(
        season,
        min_minutes,
        dummy_penalty_ratio,
        "dummy",
        sep = "_"
      )]] <- outer_dummy_coefficients

      cat(
        "Season:", season,
        "| threshold:", min_minutes,
        "| dummy penalty ratio:", dummy_penalty_ratio,
        "| inner neutral RMSE:",
        round(threshold_results[[result_key]]$neutral_inner_RMSE, 3),
        "| outer dummy RMSE:",
        round(threshold_results[[result_key]]$outer_dummy_RMSE, 3),
        "\n"
      )
    }

    list(
      results = threshold_results,
      coefficients = threshold_coefficients,
      category_exposure = threshold_category_exposure
    )
  }

  threshold_worker_count <- min(
    rapm_workers,
    length(minutes_thresholds)
  )

  if (
    threshold_worker_count > 1L &&
      .Platform$OS.type != "windows"
  ) {
    threshold_runs <- parallel::mclapply(
      minutes_thresholds,
      run_threshold,
      mc.cores = threshold_worker_count,
      mc.preschedule = FALSE,
      mc.set.seed = TRUE
    )
  } else {
    threshold_runs <- lapply(
      minutes_thresholds,
      run_threshold
    )
  }

  all_season_results[[as.character(season)]] <- bind_rows(
    unlist(
      lapply(threshold_runs, `[[`, "results"),
      recursive = FALSE
    )
  )
  all_season_coefs[[as.character(season)]] <- bind_rows(
    unlist(
      lapply(threshold_runs, `[[`, "coefficients"),
      recursive = FALSE
    )
  )
  all_category_exposure[[as.character(season)]] <- bind_rows(
    lapply(threshold_runs, `[[`, "category_exposure")
  )

  season_checkpoint <- list(
    specification = checkpoint_specification,
    results = all_season_results[[as.character(season)]],
    coefficients = all_season_coefs[[as.character(season)]],
    starter_exclusions =
      all_starter_exclusions[[as.character(season)]],
    sample_summary =
      all_sample_summaries[[as.character(season)]],
    category_exposure =
      all_category_exposure[[as.character(season)]]
  )
  temporary_checkpoint <- tempfile(
    pattern = paste0("season_", season, "_"),
    tmpdir = checkpoint_directory,
    fileext = ".rds"
  )
  saveRDS(season_checkpoint, temporary_checkpoint)
  if (!file.rename(temporary_checkpoint, checkpoint_path)) {
    stop(
      paste("Could not finalize checkpoint", checkpoint_path),
      call. = FALSE
    )
  }

  cat("\nFinished season:", season, "\n")
}

########################################################
### SELECT CONFIGURATION USING INNER VALIDATION ONLY ###
########################################################

all_configuration_results <- bind_rows(all_season_results)
all_coefficients <- bind_rows(all_season_coefs)

configuration_summary <- all_configuration_results %>%
  group_by(min_minutes, dummy_penalty_ratio) %>%
  summarise(
    mean_neutral_inner_RMSE = mean(neutral_inner_RMSE),
    mean_inner_dummy_RMSE = mean(inner_dummy_RMSE),
    mean_inner_no_dummy_RMSE = mean(inner_no_dummy_RMSE),
    n_seasons = n(),
    .groups = "drop"
  ) %>%
  arrange(
    mean_neutral_inner_RMSE,
    min_minutes,
    desc(dummy_penalty_ratio)
  )

selected_configuration <- configuration_summary %>%
  slice(1)

selected_outer_results <- all_configuration_results %>%
  semi_join(
    selected_configuration,
    by = c("min_minutes", "dummy_penalty_ratio")
  ) %>%
  arrange(season)

selected_coefficients <- all_coefficients %>%
  filter(
    min_minutes == selected_configuration$min_minutes,
    model == "no_dummy" |
      dummy_penalty_ratio ==
        selected_configuration$dummy_penalty_ratio
  )

selected_model_results <- bind_rows(
  selected_outer_results %>%
    transmute(
      season,
      min_minutes,
      dummy_penalty_ratio,
      model_type = "dummy",
      model_name = "selected_dummy",
      RMSE = outer_dummy_RMSE,
      R2 = outer_dummy_R2
    ),
  selected_outer_results %>%
    transmute(
      season,
      min_minutes,
      dummy_penalty_ratio,
      model_type = "no_dummy",
      model_name = "selected_no_dummy",
      RMSE = outer_no_dummy_RMSE,
      R2 = outer_no_dummy_R2
    )
)

write.csv(
  all_configuration_results,
  output_path(
    "model_grid.csv"
  ),
  row.names = FALSE
)
write.csv(
  configuration_summary,
  output_path(
    "configuration_summary.csv"
  ),
  row.names = FALSE
)
write.csv(
  selected_model_results,
  output_path("selected_models.csv"),
  row.names = FALSE
)
write.csv(
  selected_coefficients,
  output_path("selected_coefficients.csv"),
  row.names = FALSE
)
write.csv(
  bind_rows(all_sample_summaries),
  output_path("sample_summary.csv"),
  row.names = FALSE
)
write.csv(
  bind_rows(all_category_exposure),
  output_path("category_exposure.csv"),
  row.names = FALSE
)

threshold_zero_check <- all_configuration_results %>%
  filter(min_minutes == 0) %>%
  transmute(
    lambda_inner_equal =
      inner_dummy_lambda == inner_no_dummy_lambda,
    lambda_outer_equal =
      outer_dummy_lambda == outer_no_dummy_lambda,
    rmse_inner_equal =
      inner_dummy_RMSE == inner_no_dummy_RMSE,
    rmse_outer_equal =
      outer_dummy_RMSE == outer_no_dummy_RMSE,
    r2_inner_equal =
      inner_dummy_R2 == inner_no_dummy_R2,
    r2_outer_equal =
      outer_dummy_R2 == outer_no_dummy_R2
  )
if (
  nrow(threshold_zero_check) > 0L &&
    !all(as.matrix(threshold_zero_check))
) {
  stop(
    "Threshold-zero equality failed in generated results.",
    call. = FALSE
  )
}

print(selected_configuration)
print(selected_model_results)

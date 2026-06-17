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
library(brms)
library(ggplot2)
library(stringr)

set.seed(1)

#################################################
### SEASONS TO RUN ##############################
#################################################

# Last full seasons excluding COVID 2020 season

seasons_to_run <- c(2015, 2016, 2017, 2018, 2019)

minutes_thresholds <- seq(0, 30, by = 5)

#################################################
### STORAGE #####################################
#################################################

all_season_results <- list()
all_season_coefs   <- list()

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
  
  cat("\n=====================================\n")
  cat("RUNNING SEASON:", season, "\n")
  cat("=====================================\n")
  
  ########################################
  ### LOAD PLAY BY PLAY ##################
  ########################################
  
  play_by_play_data <- load_nba_pbp(seasons = season)
  
  # Keep only regular season plays
  play_by_play_data <- play_by_play_data %>%
    filter(season_type == 2)
  
  # Keep only actual nba teams
  play_by_play_data <- play_by_play_data %>%
    filter(team_id %in% (1:30))
  
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
    left_join(game_teams, by = "game_id") %>%
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
  
  start_five <- all_player_events %>%
    filter(!if_any(c(team_id, game_id),
                   ~ is.na(.))) %>%
    group_by(game_id,
             team_id,
             period_number) %>%
    arrange(game_play_number,
            .by_group = TRUE) %>%
    distinct(player_id,
             .keep_all = TRUE) %>%
    slice_head(n = 5) %>%
    summarise(
      starting_five = list(
        trimws(as.character(player_id))
      ),
      .groups = "drop"
    )
  
  play_by_play_data_small <- play_by_play_data_small %>%
    left_join(
      start_five,
      by = c(
        "game_id",
        "team_id",
        "period_number"
      )
    ) %>%
    group_by(game_id,
             team_id,
             period_number) %>%
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
          
          lineup <- lineup[lineup != out_player]
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
    group_by(game_id, period_number) %>%
    fill(home_lineup,
         away_lineup,
         .direction = "down") %>%
    fill(home_lineup,
         away_lineup,
         .direction = "up") %>%
    ungroup()
  
  #################################################
  ### CORRECT MID-FREE-THROW SUBSTITUTIONS ########
  #################################################
  
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
  ### FILTER GARBAGE TIME ##################
  ###########################################
  
  play_by_play_data_small <- play_by_play_data_small %>%
    mutate(
      score_difference = abs(
        home_score - away_score
      )
    ) %>%
    filter(
      !(
        score_difference > 15 &
          start_game_seconds_remaining < 120
      )
    )
  
  ###########################################
  ### PLAYER GAME MINUTES ###################
  ###########################################
  
  player_game_seconds <- play_by_play_data_small %>%
    
    filter(possession_end == TRUE) %>%
    
    group_by(game_id) %>%
    arrange(game_play_number) %>%
    
    mutate(
      prev_seconds_remaining = lag(
        start_game_seconds_remaining,
        default = 2880
      ),
      possession_duration =
        prev_seconds_remaining -
        start_game_seconds_remaining
    ) %>%
    
    ungroup() %>%
    
    mutate(
      home_lineup = lapply(
        home_lineup,
        as.character
      ),
      away_lineup = lapply(
        away_lineup,
        as.character
      )
    ) %>%
    
    rowwise() %>%
    
    mutate(
      all_players = list(
        c(home_lineup, away_lineup)
      )
    ) %>%
    
    ungroup() %>%
    
    unnest(all_players) %>%
    
    group_by(game_id, all_players) %>%
    
    summarise(
      total_seconds = sum(
        possession_duration,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    
    mutate(total_minutes = total_seconds / 60)
  
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
    ) %>%
    mutate(
      lineup_change =
        lineup_pair != lag(
          lineup_pair,
          default = first(lineup_pair)
        ),
      stint_id = cumsum(lineup_change)
    )
  
  base_stints_raw <- base_play_by_play %>%
    
    filter(type_text != "Substitution") %>%
    
    group_by(game_id, stint_id) %>%
    
    summarise(
      home_lineup_key = first(home_lineup_key),
      away_lineup_key = first(away_lineup_key),
      n_possessions = sum(
        possession_end,
        na.rm = TRUE
      ),
      start_points_home = first(home_score),
      end_points_home = last(home_score),
      start_points_away = first(away_score),
      end_points_away = last(away_score),
      game_date = first(game_date),
      .groups = "drop"
    ) %>%
    
    mutate(
      home_net_points =
        (end_points_home - start_points_home) -
        (end_points_away - start_points_away)
    ) %>%
    
    arrange(game_date, stint_id)
  
  base_stints <- base_stints_raw %>%
    
    group_by(game_id) %>%
    
    mutate(
      home_net_points =
        home_net_points +
        if_else(
          lag(n_possessions,
              default = 1) == 0,
          lag(home_net_points,
              default = 0),
          0L
        )
    ) %>%
    
    filter(n_possessions > 0) %>%
    
    ungroup()
  
  ###########################################
  ### TRAIN / TEST SPLIT ###################
  ###########################################
  
  test_games <- base_stints %>%
    mutate(
      month = as.integer(
        format(
          as.Date(game_date),
          "%m"
        )
      )
    ) %>%
    filter(month %in% c(3, 4)) %>%
    pull(game_id) %>%
    unique()
  
  train_idx <- which(
    !base_stints$game_id %in% test_games
  )
  
  test_idx <- which(
    base_stints$game_id %in% test_games
  )
  
  y <- 100 *
    base_stints$home_net_points /
    base_stints$n_possessions
  
  w <- base_stints$n_possessions
  
  w_train <- w[train_idx]
  w_test <- w[test_idx]
  
  ###########################################
  ### TRAINING MINUTES ONLY #################
  ###########################################
  
  train_game_ids <- unique(
    base_stints$game_id[train_idx]
  )
  
  player_game_minutes_train <- player_game_seconds %>%
    filter(game_id %in% train_game_ids)
  
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
  
  ###########################################
  ### GRID SEARCH ###########################
  ###########################################
  
  grid_results <- list()
  
  for (min_minutes in minutes_thresholds) {
    
    ###########################################
    ### QUALIFIED PLAYERS #####################
    ###########################################
    
    qualified_players <- player_game_minutes_train %>%
      group_by(all_players) %>%
      summarise(
        avg_minutes_per_game = mean(
          total_minutes,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      filter(
        avg_minutes_per_game > min_minutes
      ) %>%
      pull(all_players)
    
    ###########################################
    ### DUMMY LINEUP FUNCTION #################
    ###########################################
    
    dummy_lineup <- function(key) {
      
      players <- strsplit(
        key,
        "\\|"
      )[[1]]
      
      n_dummies <- sum(
        !players %in% qualified_players
      )
      
      players <- ifelse(
        players %in% qualified_players,
        players,
        as.character(n_dummies)
      )
      
      paste(
        sort(players),
        collapse = "|"
      )
    }
    
    home_dummy <- sapply(
      base_stints$home_lineup_key,
      dummy_lineup
    )
    
    away_dummy <- sapply(
      base_stints$away_lineup_key,
      dummy_lineup
    )
    
    ###########################################
    ### DUMMY MATRIX ##########################
    ###########################################
    
    all_players_dummy <- sort(unique(c(
      unlist(strsplit(home_dummy, "\\|")),
      unlist(strsplit(away_dummy, "\\|"))
    )))
    
    X_dummy <- do.call(
      rbind,
      lapply(
        seq_len(nrow(base_stints)),
        function(i) {
          
          home <- strsplit(
            home_dummy[i],
            "\\|"
          )[[1]]
          
          away <- strsplit(
            away_dummy[i],
            "\\|"
          )[[1]]
          
          as.integer(
            all_players_dummy %in% home
          ) -
            as.integer(
              all_players_dummy %in% away
            )
        }
      )
    )
    
    colnames(X_dummy) <- all_players_dummy
    
    
    ###################################################
    ### Finding the players with most common stints ###
    ###################################################
    
    player_pairs <- base_play_by_play %>%
      
      # one row per stint
      group_by(game_id, stint_id) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      
      # create lineup vectors from lineup keys
      mutate(
        home_lineup = strsplit(home_lineup_key, "\\|"),
        away_lineup = strsplit(away_lineup_key, "\\|")
      ) %>%
      
      # process home and away separately
      pivot_longer(
        cols = c(home_lineup, away_lineup),
        names_to = "side",
        values_to = "lineup"
      ) %>%
      
      mutate(
        team = if_else(
          side == "home_lineup",
          home_team_id,
          away_team_id
        )
      ) %>%
      
      # generate all pairs from each lineup
      rowwise() %>%
      mutate(
        pairs = list({
          players <- sort(unique(unlist(lineup)))
          
          if (length(players) < 2) {
            NULL
          } else {
            as.data.frame(t(combn(players, 2)))
          }
        })
      ) %>%
      ungroup() %>%
      filter(!sapply(pairs, is.null)) %>%
      
      unnest(pairs) %>%
      rename(
        player1 = V1,
        player2 = V2
      ) %>%
      
      # count how often each pair appears together
      group_by(team, player1, player2) %>%
      summarise(
        shared_stints = n(),
        .groups = "drop"
      ) %>%
      
      # most common pair for each team
      group_by(team) %>%
      slice_max(shared_stints, n = 1, with_ties = FALSE) %>%
      ungroup()
    
    ##########################################
    ### Adding interaction terms to Matrix ###
    ##########################################
    
    # Build interaction columns for each pair
    pair_matrix <- do.call(
      cbind,
      lapply(seq_len(nrow(player_pairs)), function(i) {
        
        p1 <- player_pairs$player1[i]
        p2 <- player_pairs$player2[i]
        
        col <- sapply(seq_len(nrow(base_stints)), function(j) {
          
          home <- strsplit(
            base_stints$home_lineup_key[j],
            "\\|"
          )[[1]]
          
          away <- strsplit(
            base_stints$away_lineup_key[j],
            "\\|"
          )[[1]]
          
          both_home <- p1 %in% home & p2 %in% home
          both_away <- p1 %in% away & p2 %in% away
          
          as.integer(both_home) -
            as.integer(both_away)
        })
        
        matrix(
          col,
          ncol = 1,
          dimnames = list(
            NULL,
            paste0("pair_", p1, "_", p2)
          )
        )
      })
    )
    
    # Append to existing X_dummy matrix
    X_dummy <- cbind(X_dummy, pair_matrix)
    
    # Splitting Matrix Train and Test
    
    
    X_dummy_sparse <- Matrix(
      X_dummy,
      sparse = TRUE
    )
    
    X_dummy_train <- X_dummy_sparse[
      train_idx,
    ]
    
    X_dummy_test <- X_dummy_sparse[
      test_idx,
    ]
    
    
    ###########################################
    ### FIT MODELS ############################
    ###########################################
    
    set.seed(1)
    
    dummy_model <- cv.glmnet(
      X_dummy_train,
      y[train_idx],
      alpha = 0,
      weights = w_train
    )
    
    
    ###########################################
    ### EVALUATION FUNCTION ###################
    ###########################################
    
    eval_model <- function(
    model,
    X_test,
    y_test,
    w_test,
    game_ids
    ) {
      
      preds <- as.vector(
        predict(
          model,
          newx = X_test,
          s = "lambda.min"
        )
      )
      
      data.frame(
        game_id = game_ids,
        actual_y = y_test,
        predicted_y = preds,
        possessions = w_test
      ) %>%
        group_by(game_id) %>%
        summarise(
          actual_margin = sum(
            actual_y * possessions / 100
          ),
          predicted_margin = sum(
            predicted_y * possessions / 100
          ),
          .groups = "drop"
        )
    }
    
    results_dummy <- eval_model(
      dummy_model,
      X_dummy_test,
      y[test_idx],
      w_test,
      base_stints$game_id[test_idx]
    )
    
    
    ###########################################
    ### STORE RESULTS #########################
    ###########################################
    
    extract_coefs <- function(model, label) {
      coef_mat <- coef(model, s = "lambda.min")
      data.frame(
        season      = season,
        min_minutes = min_minutes,
        model       = label,
        player_id   = rownames(as.matrix(coef_mat)),
        coefficient = as.vector(as.matrix(coef_mat))
      ) %>%
        filter(player_id != "(Intercept)") %>%
        mutate(player_id = as.character(player_id)) %>%
        left_join(
          player_names %>% mutate(athlete_id_1 = as.character(athlete_id_1)),
          by = c("player_id" = "athlete_id_1")
        ) %>%
        select(season, min_minutes, model, player_id, player_name, coefficient) %>%
        arrange(desc(coefficient))
    }
    
    all_season_coefs[[paste(season, min_minutes, "dummy", sep = "_")]] <-
      extract_coefs(dummy_model, "dummy")
    
    grid_results[[paste0(
      season,
      "_",
      min_minutes
    )]] <- data.frame(
      
      season = season,
      min_minutes = min_minutes,
      n_players = length(
        qualified_players
      ),
      
      dummy_lambda =
        dummy_model$lambda.min,
      
      dummy_RMSE =
        sqrt(mean(
          (
            results_dummy$actual_margin -
              results_dummy$predicted_margin
          )^2
        )),
      
      dummy_R2 =
        r_squared(
          results_dummy$actual_margin,
          results_dummy$predicted_margin
        )
    )
      
    
    cat(
      "Season:",
      season,
      "| Threshold:",
      min_minutes,
      "| Players:",
      length(qualified_players),
      "| Dummy RMSE:",
      round(
        grid_results[[paste0(
          season,
          "_",
          min_minutes
        )]]$dummy_RMSE,
        3
      ),
      "\n"
    )
  
  ###########################################
  ### STORE SEASON RESULTS ##################
  ###########################################
  
  all_season_results[[as.character(season)]] <-
    bind_rows(grid_results)
  
  cat(
    "\nFinished season:",
    season,
    "\n"
  )
  }
}

#################################################
### FINAL RESULTS ###############################
#################################################

final_grid_results <- bind_rows(
  all_season_results
)

#################################################
### SUMMARY BY THRESHOLD ########################
#################################################

threshold_summary <- final_grid_results %>%
  
  group_by(min_minutes) %>%
  
  summarise(
    
    avg_dummy_RMSE = mean(
      dummy_RMSE,
      na.rm = TRUE
    ),
    
    avg_dummy_R2 = mean(
      dummy_R2,
      na.rm = TRUE
    ),
    
    avg_players = mean(
      n_players,
      na.rm = TRUE
    ),
    
    .groups = "drop"
    
  ) %>%
  
  arrange(avg_dummy_RMSE)

print(threshold_summary)

#################################################
### BEST THRESHOLD ##############################
#################################################

best_threshold <- threshold_summary %>%
  slice_min(
    avg_dummy_RMSE,
    n = 1
  )

print(best_threshold)

#################################################
### PLOTS #######################################
#################################################

ggplot(
  threshold_summary,
  aes(
    x = min_minutes,
    y = avg_dummy_RMSE
  )
) +
  geom_line() +
  geom_point(size = 3) +
  labs(
    title = "Average Dummy RMSE by Minutes Threshold",
    x = "Minimum Average Minutes Threshold",
    y = "Average RMSE"
  ) +
  theme_minimal()


#################################################
### SEASON-BY-SEASON RESULTS ####################
#################################################

final_grid_results %>%
  arrange(season, min_minutes)

########################################################
### RANK MODELS ACROSS SEASONS #########################
########################################################

model_results <- bind_rows(
  
  # Dummy models
  final_grid_results %>%
    transmute(
      season,
      min_minutes,
      model_type = "dummy",
      model_name = paste0(min_minutes, "_dummy"),
      RMSE = dummy_RMSE,
      R2   = dummy_R2
    )
)

########################################################
### SUMMARY TABLE ######################################
########################################################

model_rankings <- model_results %>%
  
  group_by(model_name, min_minutes, model_type) %>%
  
  summarise(
    
    mean_RMSE = mean(RMSE, na.rm = TRUE),
    sd_RMSE   = sd(RMSE, na.rm = TRUE),
    
    mean_R2   = mean(R2, na.rm = TRUE),
    sd_R2     = sd(R2, na.rm = TRUE),
    
    best_RMSE = min(RMSE, na.rm = TRUE),
    worst_RMSE = max(RMSE, na.rm = TRUE),
    
    n_seasons = n(),
    
    .groups = "drop"
    
  ) %>%
  
  arrange(mean_RMSE)

########################################################
### BEST MODELS BY RMSE ################################
########################################################


model_rankings %>%
  arrange(mean_RMSE) %>%
  select(
    model_name,
    mean_RMSE,
    mean_R2,
    sd_RMSE,
    sd_R2,
    n_seasons
  ) %>%
  print(n = Inf)

################################
### Showing Top Player Pairs ###
################################

# Adding names to pairs

player_names <- play_by_play_data_small %>%
  filter(
    !is.na(athlete_id_1),
    !is.na(text),
    str_detect(type_text, "Free Throw")
  ) %>%
  mutate(player_name = word(text, 1, 2)) %>%
  distinct(athlete_id_1, .keep_all = TRUE) %>%
  select(athlete_id_1, player_name) %>%
  mutate(athlete_id_1 = as.character(athlete_id_1))

player_pairs <- player_pairs %>%
  left_join(player_names, by = c("player1" = "athlete_id_1")) %>%
  rename(player1_name = player_name) %>%
  left_join(player_names, by = c("player2" = "athlete_id_1")) %>%
  rename(player2_name = player_name)

player_pairs

pair_coefs <- bind_rows(all_season_coefs) %>%
  filter(
    min_minutes == 10,
    grepl("pair", player_id)
  )

pair_coefs

#################################################
### FINAL COEFFICIENTS ##########################
#################################################

final_coefs <- bind_rows(all_season_coefs)

write.csv(final_coefs, "interaction_final_coefs_2015_2019.csv", row.names = FALSE)

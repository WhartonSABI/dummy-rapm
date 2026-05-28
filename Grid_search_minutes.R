# Grid Search Minimum Training Set Minutes

# RAPM Stint Code

library(dplyr)
library(glmnet)
library(tidyr)
library(hoopR)
library(purrr)
library(zoo)
library(Matrix)
library(glmnet)
library(brms)
library(ggplot2)
library(stringr)

set.seed(1)

# Loading Play by Play Data from hoopR

seasons <- c(2025)

play_by_play_data <- load_nba_pbp(seasons)

head(play_by_play_data)

# Keep only regular season plays
play_by_play_data <- play_by_play_data %>%
  filter(season_type == 2)

# Keep only actual nba teams (no all-star games)
play_by_play_data <- play_by_play_data %>%
  filter(team_id %in% (1:30))

# Sort by game date (earliest games first)
play_by_play_data <- play_by_play_data %>%
  arrange(game_date)

# Filtering for necessary columns
play_by_play_data_small <- play_by_play_data %>%
  select(game_play_number, type_text, text, score_value, team_id, game_id,
         athlete_id_1, athlete_id_2, athlete_id_3, home_team_id, away_team_id,
         start_game_seconds_remaining, away_score, home_score, game_date, period_number)

head(play_by_play_data_small)

############################
### Defining Possessions ###
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
      score_value > 0 & grepl("Shot|Dunk|Layup", type_text) ~ TRUE,
      
      # Defensive rebound
      type_text == "Defensive Rebound" ~ TRUE,
      
      # Last free throw of a sequence
      type_text %in% c("Free Throw - 1 of 1", "Free Throw - 2 of 2", 
                       "Free Throw - 3 of 3", "Free Throw - Flagrant 1 of 1",
                       "Free Throw - Flagrant 2 of 2", "Free Throw - Flagrant 3 of 3",
                       "Free Throw - Clear Path 2 of 2") & score_value > 0 ~ TRUE,
      
      # Travel ends the possession
      type_text == "Traveling" ~ TRUE,
      
      # Turnovers (excluding No Turnover)
      grepl("Turnover", type_text) & type_text != "No Turnover" ~ TRUE,
      
      TRUE ~ FALSE
    )
  ) %>%
  group_by(game_id) %>%
  mutate(possession_id = lag(cumsum(possession_end), default = 0) + 1) %>%
  ungroup()


# Showing Number of Possessions in a game

play_by_play_data_small %>%
  group_by(game_id) %>%
  summarise(total_possessions = max(possession_id)) %>%
  ggplot(aes(x = total_possessions)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Possessions per Game",
    x = "Total Possessions",
    y = "Number of Games"
  ) +
  theme_minimal()

##############################################
### Getting Starting Fives in each quarter ###
##############################################

###########################################
### Create team-specific player events ###
###########################################

# Normal team events (players belonging to row's team_id)
team_player_events <- play_by_play_data_small %>%
  mutate(
    athlete_id_2_team_player = case_when(
      
      # Assists: athlete_id_2 is same team
      grepl("assists", text) ~ athlete_id_2,
      
      TRUE ~ NA_integer_
    )
  ) %>%
  tidyr::pivot_longer(
    cols = c(athlete_id_1, athlete_id_2_team_player),
    names_to  = "id_source",
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
### Add stealers to opposing team rows ###
###########################################

# Build game-team lookup
game_teams <- play_by_play_data_small %>%
  filter(!is.na(game_id), !is.na(team_id)) %>%
  distinct(game_id, team_id)

steal_events <- play_by_play_data_small %>%
  filter(grepl("steals", text)) %>%
  filter(!is.na(athlete_id_2)) %>%
  
  # offense/turnover team
  rename(turnover_team_id = team_id) %>%
  
  # attach all teams from that game
  left_join(game_teams, by = "game_id") %>%
  
  # keep the OTHER team
  filter(team_id != turnover_team_id) %>%
  
  transmute(
    game_id,
    period_number,
    game_play_number,
    team_id   = team_id,        # defensive team
    player_id = athlete_id_2    # stealer
  )

###########################################
### Combine all player appearance rows ###
###########################################

all_player_events <- bind_rows(
  team_player_events,
  steal_events
)

#############################
### Build inferred lineups ##
#############################

start_five <- all_player_events %>%
  filter(!if_any(c(team_id, game_id), ~ is.na(.))) %>%
  group_by(game_id, team_id, period_number) %>%
  arrange(game_play_number, .by_group = TRUE) %>%
  distinct(player_id, .keep_all = TRUE) %>%
  slice_head(n = 5) %>%
  summarise(
    starting_five = list(player_id),
    .groups = "drop"
  )

# Adding Starting Lineups to first play of each quarter

play_by_play_data_small <- play_by_play_data_small %>%
  left_join(start_five, by = c("game_id", "team_id", "period_number")) %>%
  group_by(game_id, team_id, period_number) %>%
  mutate(
    starting_five = if_else(row_number() == 1, starting_five, NA)
  ) %>%
  ungroup()

##############################
### Handling Substitutions ###
##############################

# Switching Players when Substituted
play_by_play_data_small <- play_by_play_data_small %>%
  mutate(starting_five = map(starting_five, ~ if (is.null(.x)) NA else .x))

lineup_changes <- play_by_play_data_small %>%
  filter(!map_lgl(starting_five, is.null) | type_text == "Substitution") %>%
  arrange(game_id, team_id, game_play_number) %>%
  group_by(game_id, team_id) %>%
  group_split()

updated_lineup_list <- list()

for (grp in lineup_changes) {
  n <- nrow(grp)
  updated_lineups <- vector("list", n)
  
  for (i in seq_len(n)) {
    # Case 1: Use initial starting five if available
    if (!is.null(grp$starting_five[[i]]) && length(grp$starting_five[[i]]) == 5) {
      updated_lineups[[i]] <- grp$starting_five[[i]]
      
      # Case 2: Otherwise, carry over previous lineup
    } else if (i > 1 && !is.null(updated_lineups[[i - 1]])) {
      updated_lineups[[i]] <- updated_lineups[[i - 1]]
    } else {
      updated_lineups[[i]] <- NA
    }
    
    # If it's a substitution, try replacing the players
    if (grp$type_text[i] == "Substitution" &&
        !is.na(grp$athlete_id_1[i]) && !is.na(grp$athlete_id_2[i]) &&
        !all(is.na(updated_lineups[[i]]))) {
      
      out_player  <- as.character(grp$athlete_id_2[i])
      in_player   <- as.character(grp$athlete_id_1[i])
      lineup      <- as.character(updated_lineups[[i]])
      
      in_already  <- in_player  %in% lineup
      out_present <- out_player %in% lineup
      
      if (in_already && out_present) {
        # In-player already on court — just remove the out-player
        lineup <- lineup[lineup != out_player]
        updated_lineups[[i]] <- lineup
        
      } else if (in_already && !out_present) {
        # In-player already there, out-player not found — lineup already correct, skip
        
      } else if (!in_already && out_present) {
        # Normal case — swap out for in
        idx <- which(lineup == out_player)
        lineup[idx] <- in_player
        updated_lineups[[i]] <- lineup
        
      } else {
        # Neither found — lineup is stale, skip
      }
    }
  }
  
  # Save updated lineups back to the group
  grp$updated_lineup <- updated_lineups
  updated_lineup_list <- append(updated_lineup_list, list(grp))
}

lineup_changes_updated <- bind_rows(updated_lineup_list)

play_by_play_data_small <- play_by_play_data_small %>%
  left_join(
    lineup_changes_updated %>%
      select(game_id, team_id, game_play_number, updated_lineup),
    by = c("game_id", "team_id", "game_play_number")
  ) %>%
  group_by(game_id, team_id) %>%
  tidyr::fill(updated_lineup, .direction = "down") %>%
  ungroup()

# Checking Substitutions worked correctly

pbp_with_lineups_check <- play_by_play_data_small %>%
  arrange(game_id, team_id, game_play_number) %>%
  group_by(game_id, team_id) %>%
  mutate(first_play = row_number() == 1) %>%
  ungroup() %>%
  filter(first_play | type_text == "Substitution") %>%
  select(game_id, team_id, game_play_number, type_text, 
         athlete_id_1, athlete_id_2, updated_lineup)

pbp_with_lineups_check <- pbp_with_lineups_check %>%
  filter(game_id == pbp_with_lineups_check$game_id[1], team_id == 5)


####################################
### Making Home and Away lineups ###
####################################

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
  tidyr::fill(home_lineup, away_lineup, .direction = "down") %>%
  tidyr::fill(home_lineup, away_lineup, .direction = "up") %>%
  ungroup()

###############################################
### Correcting Substitutions Mid-Free-Throw ###
###############################################


# Parse FT attempt and total directly from type_text
ft_info <- play_by_play_data_small %>%
  filter(grepl("Free Throw", type_text), type_text != "Free Throw - Technical") %>%
  mutate(
    ft_attempt = as.integer(gsub(".*(\\d) of (\\d).*", "\\1", type_text)),
    ft_total   = as.integer(gsub(".*(\\d) of (\\d).*", "\\2", type_text))
  )

# Snapshot lineups at FT 1-of-N
ft_first <- ft_info %>%
  filter(ft_attempt == 1L, ft_total >= 2L) %>%
  group_by(game_id) %>%
  mutate(ft_trip_id = row_number()) %>%
  ungroup() %>%
  select(game_id, ft_trip_id,
         lineup_at_ft1      = updated_lineup,
         home_lineup_at_ft1 = home_lineup,
         away_lineup_at_ft1 = away_lineup)

# Find last FT of each trip
ft_last <- ft_info %>%
  filter(ft_attempt == ft_total, ft_total >= 2L) %>%
  group_by(game_id) %>%
  mutate(ft_trip_id = row_number()) %>%
  ungroup() %>%
  select(game_id, ft_trip_id, game_play_number)

# Join first-FT lineups onto last-FT rows
ft_fix <- ft_last %>%
  left_join(ft_first, by = c("game_id", "ft_trip_id"))

# Patch using row indexing instead of if_else (works with list columns)
play_by_play_data_small <- play_by_play_data_small %>%
  left_join(ft_fix %>% select(game_id, game_play_number,
                              lineup_at_ft1,
                              home_lineup_at_ft1,
                              away_lineup_at_ft1),
            by = c("game_id", "game_play_number"))

# Find rows that need fixing
fix_rows <- which(!sapply(play_by_play_data_small$lineup_at_ft1, is.null))

# Overwrite list columns directly by index
play_by_play_data_small$updated_lineup[fix_rows] <- play_by_play_data_small$lineup_at_ft1[fix_rows]
play_by_play_data_small$home_lineup[fix_rows]    <- play_by_play_data_small$home_lineup_at_ft1[fix_rows]
play_by_play_data_small$away_lineup[fix_rows]    <- play_by_play_data_small$away_lineup_at_ft1[fix_rows]

# Drop temp columns
play_by_play_data_small <- play_by_play_data_small %>%
  select(-lineup_at_ft1, -home_lineup_at_ft1, -away_lineup_at_ft1)


##################################
### Filtering out Garbage Time ###
##################################

play_by_play_data_small <- play_by_play_data_small %>%
  mutate(score_difference = abs(home_score - away_score)) %>%
  filter(!(score_difference > 15 & start_game_seconds_remaining < 120))

############################
### Filtering for scrubs ###
############################

player_game_seconds <- play_by_play_data_small %>%
  # Step 1: isolate possession-ending rows and calculate duration
  filter(possession_end == TRUE) %>%
  group_by(game_id) %>%
  arrange(game_play_number) %>%
  mutate(
    prev_seconds_remaining = lag(start_game_seconds_remaining, default = 2880),
    possession_duration    = prev_seconds_remaining - start_game_seconds_remaining
  ) %>%
  ungroup() %>%
  # Step 2: expand to one row per player per possession
  mutate(
    home_lineup = lapply(home_lineup, as.character),
    away_lineup = lapply(away_lineup, as.character)
  ) %>%
  rowwise() %>%
  mutate(all_players = list(c(home_lineup, away_lineup))) %>%
  ungroup() %>%
  unnest(all_players) %>%
  # Step 3: sum duration per player per game
  group_by(game_id, all_players) %>%
  summarise(
    total_seconds = sum(possession_duration, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(total_minutes = total_seconds / 60)


#############################
### Grid Search - Minutes ###
#############################

# Build stints ONCE using real lineups (for scores and possessions)
base_play_by_play <- play_by_play_data_small %>%
  mutate(
    home_lineup_key = sapply(home_lineup, function(x) {
      paste(sort(trimws(as.character(x[!is.na(x)]))), collapse = "|")
    }),
    away_lineup_key = sapply(away_lineup, function(x) {
      paste(sort(trimws(as.character(x[!is.na(x)]))), collapse = "|")
    }),
    lineup_pair = paste(home_lineup_key, away_lineup_key, sep = "||")
  ) %>%
  mutate(
    lineup_change = lineup_pair != lag(lineup_pair, default = first(lineup_pair)),
    stint_id = cumsum(lineup_change)
  )

base_stints <- base_play_by_play %>%
  filter(type_text != "Substitution") %>%
  group_by(game_id, stint_id) %>%
  summarise(
    home_lineup_key   = first(home_lineup_key),
    away_lineup_key   = first(away_lineup_key),
    n_possessions     = sum(possession_end, na.rm = TRUE),
    start_points_home = first(home_score),
    end_points_home   = last(home_score),
    start_points_away = first(away_score),
    end_points_away   = last(away_score),
    game_date         = first(game_date),
    .groups = "drop"
  ) %>%
  mutate(
    home_net_points = (end_points_home - start_points_home)
    - (end_points_away - start_points_away)
  ) %>%
  filter(n_possessions > 0) %>%
  arrange(game_date, stint_id)

# Fixed train/test split based on base_stints
latest_season_year <- max(as.integer(format(as.Date(base_stints$game_date), "%Y")))

test_games <- base_stints %>%
  mutate(
    year  = as.integer(format(as.Date(game_date), "%Y")),
    month = as.integer(format(as.Date(game_date), "%m"))
  ) %>%
  filter(year == latest_season_year, month %in% c(3, 4)) %>%
  pull(game_id) %>%
  unique()

train_idx <- which(!base_stints$game_id %in% test_games)
test_idx  <- which(base_stints$game_id %in% test_games)

y <- 100 * base_stints$home_net_points / base_stints$n_possessions
w <- base_stints$n_possessions

w_train <- w[train_idx]
w_test  <- w[test_idx]

###############################
### TRAIN-ONLY MINUTES DATA ###
###############################

train_game_ids <- unique(base_stints$game_id[train_idx])

player_game_minutes_train <- player_game_seconds %>%
  filter(game_id %in% train_game_ids)

# Player name lookup
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

r_squared <- function(actual, predicted) {
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  1 - (ss_res / ss_tot)
}

minutes_thresholds <- seq(0, 2000, by = 250)

grid_results <- list()
grid_coefs   <- list()

for (min_minutes in minutes_thresholds) {
  
  #############################################
  ### QUALIFIED PLAYERS FROM TRAINING ONLY ###
  #############################################
  
  qualified_players <- player_game_minutes_train %>%
    group_by(all_players) %>%
    summarise(
      train_minutes = sum(total_minutes, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(train_minutes > min_minutes) %>%
    pull(all_players)
  
  ##################################
  ### BUILD DUMMY LINEUP KEYS    ###
  ##################################
  
  dummy_lineup <- function(key) {
    
    players <- strsplit(key, "\\|")[[1]]
    
    n_dummies <- sum(!players %in% qualified_players)
    
    players <- ifelse(
      players %in% qualified_players,
      players,
      as.character(n_dummies)
    )
    
    paste(sort(players), collapse = "|")
  }
  
  home_dummy <- sapply(base_stints$home_lineup_key, dummy_lineup)
  away_dummy <- sapply(base_stints$away_lineup_key, dummy_lineup)
  
  #####################
  ### Dummy Matrix  ###
  #####################
  
  all_players_dummy <- sort(unique(c(
    unlist(strsplit(home_dummy, "\\|")),
    unlist(strsplit(away_dummy, "\\|"))
  )))
  
  X_dummy <- do.call(rbind, lapply(seq_len(nrow(base_stints)), function(i) {
    
    home <- strsplit(home_dummy[i], "\\|")[[1]]
    away <- strsplit(away_dummy[i], "\\|")[[1]]
    
    as.integer(all_players_dummy %in% home) -
      as.integer(all_players_dummy %in% away)
  }))
  
  colnames(X_dummy) <- all_players_dummy
  
  X_dummy_sparse <- Matrix(X_dummy, sparse = TRUE)
  
  X_dummy_train <- X_dummy_sparse[train_idx, ]
  X_dummy_test  <- X_dummy_sparse[test_idx, ]
  
  ########################
  ### No Dummy Matrix  ###
  ########################
  
  all_players_no_dummy <- sort(qualified_players)
  
  X_no_dummy <- do.call(rbind, lapply(seq_len(nrow(base_stints)), function(i) {
    
    home <- strsplit(home_dummy[i], "\\|")[[1]]
    away <- strsplit(away_dummy[i], "\\|")[[1]]
    
    as.integer(all_players_no_dummy %in% home) -
      as.integer(all_players_no_dummy %in% away)
  }))
  
  colnames(X_no_dummy) <- all_players_no_dummy
  
  X_no_dummy_sparse <- Matrix(X_no_dummy, sparse = TRUE)
  
  X_no_dummy_train <- X_no_dummy_sparse[train_idx, ]
  X_no_dummy_test  <- X_no_dummy_sparse[test_idx, ]
  
  ####################
  ### Fit Models   ###
  ####################
  
  set.seed(1)
  
  dummy_model <- cv.glmnet(
    X_dummy_train,
    y[train_idx],
    alpha = 0,
    weights = w_train
  )
  
  no_dummy_model <- cv.glmnet(
    X_no_dummy_train,
    y[train_idx],
    alpha = 0,
    weights = w_train
  )
  
  ########################
  ### Test Performance ###
  ########################
  
  eval_model <- function(model, X_test, y_test, w_test, game_ids) {
    
    preds <- as.vector(
      predict(model, newx = X_test, s = "lambda.min")
    )
    
    data.frame(
      game_id     = game_ids,
      actual_y    = y_test,
      predicted_y = preds,
      possessions = w_test
    ) %>%
      group_by(game_id) %>%
      summarise(
        actual_margin    = sum(actual_y    * possessions / 100),
        predicted_margin = sum(predicted_y * possessions / 100),
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
  
  results_no_dummy <- eval_model(
    no_dummy_model,
    X_no_dummy_test,
    y[test_idx],
    w_test,
    base_stints$game_id[test_idx]
  )
  
  #########################
  ### Extract Coefs     ###
  #########################
  
  extract_coefs <- function(model, label) {
    
    coef_mat <- coef(model, s = "lambda.min")
    
    data.frame(
      min_minutes = min_minutes,
      model       = label,
      player_id   = rownames(coef_mat),
      coefficient = as.vector(coef_mat)
    ) %>%
      filter(player_id != "(Intercept)") %>%
      left_join(player_names, by = c("player_id" = "athlete_id_1")) %>%
      arrange(desc(coefficient))
  }
  
  grid_coefs[[paste(min_minutes, "dummy", sep = "_")]] <-
    extract_coefs(dummy_model, "dummy")
  
  grid_coefs[[paste(min_minutes, "no_dummy", sep = "_")]] <-
    extract_coefs(no_dummy_model, "no_dummy")
  
  ##########################
  ### Store Grid Results ###
  ##########################
  
  grid_results[[as.character(min_minutes)]] <- data.frame(
    min_minutes     = min_minutes,
    n_players       = length(qualified_players),
    dummy_lambda    = dummy_model$lambda.min,
    dummy_RMSE      = sqrt(mean(
      (results_dummy$actual_margin -
         results_dummy$predicted_margin)^2
    )),
    dummy_R2        = r_squared(
      results_dummy$actual_margin,
      results_dummy$predicted_margin
    ),
    no_dummy_lambda = no_dummy_model$lambda.min,
    no_dummy_RMSE   = sqrt(mean(
      (results_no_dummy$actual_margin -
         results_no_dummy$predicted_margin)^2
    )),
    no_dummy_R2     = r_squared(
      results_no_dummy$actual_margin,
      results_no_dummy$predicted_margin
    ),
    SD_actual       = sd(results_dummy$actual_margin)
  )
  
  cat(
    "Done:",
    min_minutes,
    "min |",
    length(qualified_players),
    "players |",
    "Dummy RMSE:",
    round(grid_results[[as.character(min_minutes)]]$dummy_RMSE, 3),
    "| No Dummy RMSE:",
    round(grid_results[[as.character(min_minutes)]]$no_dummy_RMSE, 3),
    "\n"
  )
}

grid_summary <- bind_rows(grid_results)
all_coefs    <- bind_rows(grid_coefs)

print(grid_summary)

all_coefs %>%
  filter(min_minutes == 750, model == "dummy") %>%
  arrange(desc(coefficient)) %>%
  head(20)

# Single player across all thresholds
all_coefs %>%
  filter(player_id == "3934719") %>%
  select(min_minutes, model, player_name, coefficient)

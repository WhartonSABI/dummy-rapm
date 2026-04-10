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

set.seed(1)

# Loading Play by Play Data from hoopR

seasons <- c(2023, 2024, 2025)

play_by_play_data <- load_nba_pbp(seasons)

head(play_by_play_data)


# Filtering for necessary columns

play_by_play_data_small <- play_by_play_data %>%
  select(game_play_number, type_text, text, score_value, team_id, game_id,
         athlete_id_1, athlete_id_2, athlete_id_3, home_team_id, away_team_id,
         start_game_seconds_remaining, away_score, home_score)

head(play_by_play_data_small)

##############################
### Getting Starting Fives ###
##############################

# We remove rows with all NAs, and filter for the first 5 athletes that appear
# for each team in each game (in theory if a player is subbed early he won't appear)

start_five <- play_by_play_data_small %>%
  filter(!if_all(c(athlete_id_1, athlete_id_2, athlete_id_3), ~ is.na(.))) %>%
  filter(!if_any(c(team_id, game_id), ~ is.na(.))) %>%
  group_by(game_id, team_id) %>%
  arrange(game_play_number) %>%
  distinct(athlete_id_1, .keep_all = TRUE) %>%
  slice_head(n = 5) %>%
  summarise(starting_five = list(athlete_id_1), .groups = "drop")

# Adding Starting Lineups to first play

play_by_play_data_small <- play_by_play_data_small %>%
  left_join(start_five, by = c("game_id", "team_id")) %>%
  group_by(game_id, team_id) %>%
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
      updated_lineups[[i]] <- NA  # Still nothing to carry
    }
    
    # If it's a substitution, try replacing the players
    if (grp$type_text[i] == "Substitution" &&
        !is.na(grp$athlete_id_1[i]) && !is.na(grp$athlete_id_2[i]) &&
        !all(is.na(updated_lineups[[i]]))) {
      
      out_player <- grp$athlete_id_2[i]
      in_player  <- grp$athlete_id_1[i]
      
      lineup <- as.character(updated_lineups[[i]])
      idx <- which(lineup == out_player)
      if (length(idx) > 0) {
        lineup[idx] <- in_player
        updated_lineups[[i]] <- lineup
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
  select(game_id, team_id, game_play_number, type_text, athlete_id_1, athlete_id_2, updated_lineup)

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
  group_by(game_id) %>%
  tidyr::fill(home_lineup, away_lineup, .direction = "down") %>%
  tidyr::fill(home_lineup, away_lineup, .direction = "up") %>%
  ungroup()

############################
### Defining Possessions ###
############################

play_by_play_data_small <- play_by_play_data_small %>%
  mutate(
    possession_end = case_when(
      # Made field goals (score_value > 0 and it's a shot)
      score_value > 0 & grepl("Shot|Dunk|Layup", type_text) ~ TRUE,
      
      # Defensive rebound (missed shot rebounded by defense)
      type_text == "Defensive Rebound" ~ TRUE,
      
      # Last free throw of a sequence
      type_text %in% c("Free Throw - 1 of 1", "Free Throw - 2 of 2", 
                       "Free Throw - 3 of 3", "Free Throw - Flagrant 1 of 1",
                       "Free Throw - Flagrant 2 of 2", "Free Throw - Flagrant 3 of 3",
                       "Free Throw - Clear Path 2 of 2") ~ TRUE,
      
      # Turnovers (excluding No Turnover)
      grepl("Turnover", type_text) & type_text != "No Turnover" ~ TRUE,
      
      TRUE ~ FALSE
    )
  ) %>%
  group_by(game_id) %>%
  mutate(possession_id = cumsum(possession_end)) %>%
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

##################################
### Filtering out Garbage Time ###
##################################

play_by_play_data_small <- play_by_play_data_small %>%
  mutate(score_difference = abs(home_score - away_score)) %>%
  filter(!(score_difference > 15 & start_game_seconds_remaining < 120))


################################################
### Filtering for players with p possessions ###
################################################

# Literature has had filter at ~200 minutes played

# ~200 possessions in a game
p <- 2000

qualified_players <- play_by_play_data_small %>%
  group_by(game_id, possession_id) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(
    home_lineup = lapply(home_lineup, as.character),
    away_lineup = lapply(away_lineup, as.character)
  ) %>%
  rowwise() %>%
  mutate(all_players = list(c(home_lineup, away_lineup))) %>%
  ungroup() %>%
  unnest(all_players) %>%
  group_by(all_players) %>%
  summarise(total_possessions = n()) %>%
  filter(total_possessions >= p) %>%
  pull(all_players)

# Adding dummy ids (number of dummys is the id number)

dummy_play_by_play_data <- play_by_play_data_small %>%
  mutate(
    home_lineup = lapply(home_lineup, function(lineup) {
      n_dummies <- sum(!lineup %in% qualified_players)
      ifelse(lineup %in% qualified_players, lineup, as.character(n_dummies))
    }),
    away_lineup = lapply(away_lineup, function(lineup) {
      n_dummies <- sum(!lineup %in% qualified_players)
      ifelse(lineup %in% qualified_players, lineup, as.character(n_dummies))
    })
  )

# Number of dummies by count

dummy_play_by_play_data %>%
  mutate(
    home_lineup = lapply(home_lineup, as.character),
    away_lineup = lapply(away_lineup, as.character)
  ) %>%
  rowwise() %>%
  mutate(all_players = list(c(home_lineup, away_lineup))) %>%
  ungroup() %>%
  unnest(all_players) %>%
  filter(all_players %in% as.character(1:5)) %>%
  count(all_players)


#####################
### Making Matrix ###
#####################

# Y vector - net points per possession
y <- dummy_play_by_play_data %>%
  group_by(game_id, possession_id) %>%
  summarise(
    home_points = sum(score_value[team_id == first(home_team_id)], na.rm = TRUE),
    away_points = sum(score_value[team_id == first(away_team_id)], na.rm = TRUE),
    net_points = home_points - away_points,
    .groups = "drop"
  ) %>%
  pull(net_points)

# X matrix - +1 if home player, -1 if away player, 0 if not on court

possession_lineups <- dummy_play_by_play_data %>%
  mutate(
    home_lineup = lapply(home_lineup, as.character),
    away_lineup = lapply(away_lineup, as.character)
  ) %>%
  group_by(game_id, possession_id) %>%
  slice_head(n = 1) %>%
  ungroup()

all_players <- sort(unique(c(unlist(possession_lineups$home_lineup),
                             unlist(possession_lineups$away_lineup))))

X <- do.call(rbind, lapply(seq_len(nrow(possession_lineups)), function(i) {
  home <- possession_lineups$home_lineup[[i]]
  away <- possession_lineups$away_lineup[[i]]
  as.integer(all_players %in% home) - as.integer(all_players %in% away)
}))

colnames(X) <- all_players


#######################
### No Dummy Matrix ###
#######################

possession_lineups_no_dummy <- play_by_play_data_small %>%
  mutate(
    home_lineup = lapply(home_lineup, as.character),
    away_lineup = lapply(away_lineup, as.character)
  ) %>%
  group_by(game_id, possession_id) %>%
  slice_head(n = 1) %>%
  ungroup()

# Only qualified players become columns
all_players_no_dummy <- sort(qualified_players)

X_no_dummy <- do.call(rbind, lapply(seq_len(nrow(possession_lineups_no_dummy)), function(i) {
  home <- possession_lineups_no_dummy$home_lineup[[i]]
  away <- possession_lineups_no_dummy$away_lineup[[i]]
  as.integer(all_players_no_dummy %in% home) - as.integer(all_players_no_dummy %in% away)
}))

colnames(X_no_dummy) <- all_players_no_dummy

################################
### Splitting Train and Test ###
################################

n_possessions <- nrow(possession_lineups)
train_size    <- floor(0.8 * n_possessions)

train_idx <- seq_len(train_size)
test_idx  <- seq(train_size + 1, n_possessions)

###################
### Dummy Split ###
###################
X_train       <- X[train_idx, ]
X_test        <- X[test_idx, ]
y_train       <- y[train_idx]
y_test        <- y[test_idx]

######################
### No Dummy Split ###
######################
X_no_dummy_train <- X_no_dummy[train_idx, ]
X_no_dummy_test  <- X_no_dummy[test_idx, ]
# y is shared - same possessions, same net points
y_no_dummy_train <- y[train_idx]
y_no_dummy_test  <- y[test_idx]


##############################
### Dummy Ridge Regression ###
##############################

library(glmnet)

# Ridge regression (alpha = 0)
dummy_ridge_model <- cv.glmnet(X_train, y_train, alpha = 0)

# Extracting coefficients at optimal lambda
dummy_ridge_coef <- coef(dummy_ridge_model, s = "lambda.min")

# Converting to a readable dataframe
player_impact_dummy <- data.frame(
  player_id = rownames(dummy_ridge_coef)[-1],  # remove intercept
  impact = as.vector(dummy_ridge_coef)[-1]
) %>%
  arrange(desc(impact))

player_impact_dummy

# Adding Player names to impact

library(stringr)

player_names_dummy <- dummy_play_by_play_data %>%
  select(athlete_id_1, text) %>%
  filter(!is.na(athlete_id_1), !is.na(text)) %>%
  mutate(
    player_name = word(text, 1, 2)  # first two words
  ) %>%
  distinct(athlete_id_1, .keep_all = TRUE) %>%
  select(athlete_id_1, player_name)

player_impact_dummy <- player_impact_dummy %>%
  left_join(player_names_dummy %>% mutate(athlete_id_1 = as.character(athlete_id_1)),
            by = c("player_id" = "athlete_id_1"))

player_impact_dummy

#################################
### No Dummy Ridge Regression ###
#################################

no_dummy_ridge_model <- cv.glmnet(X_no_dummy_train, y_no_dummy_train, alpha = 0)

# Extracting coefficients at optimal lambda
no_dummy_ridge_coef <- coef(no_dummy_ridge_model, s = "lambda.min")

# Converting to a readable dataframe
player_impact_no_dummy <- data.frame(
  player_id = rownames(no_dummy_ridge_coef)[-1],
  impact = as.vector(no_dummy_ridge_coef)[-1]
) %>%
  arrange(desc(impact))

# Adding player names
player_names_no_dummy <- play_by_play_data_small %>%
  select(athlete_id_1, text) %>%
  filter(!is.na(athlete_id_1), !is.na(text)) %>%
  mutate(player_name = word(text, 1, 2)) %>%
  distinct(athlete_id_1, .keep_all = TRUE) %>%
  select(athlete_id_1, player_name)

player_impact_no_dummy <- player_impact_no_dummy %>%
  left_join(player_names_no_dummy %>% mutate(athlete_id_1 = as.character(athlete_id_1)),
            by = c("player_id" = "athlete_id_1"))

player_impact_no_dummy

########################
### Test Performance ###
########################

# Individual Play Level Performance

# R-squared
r_squared <- function(actual, predicted) {
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  1 - (ss_res / ss_tot)
}

dummy_r2    <- r_squared(y_test, dummy_preds_test)
no_dummy_r2 <- r_squared(y_no_dummy_test, no_dummy_preds_test)

data.frame(
  model  = c("Dummy", "No Dummy"),
  RMSE   = c(dummy_rmse_test, no_dummy_rmse_test),
  R2     = c(dummy_r2, no_dummy_r2)
)

# Grouping into chunks of 100 possessions and aggregate

test_results_dummy <- data.frame(
  possession  = test_idx,
  actual_y    = y_test,
  predicted_y = as.vector(predict(dummy_ridge_model, newx = X_test, s = "lambda.min"))
)

test_results_no_dummy <- data.frame(
  possession  = test_idx,
  actual_y    = y_no_dummy_test,
  predicted_y = as.vector(predict(no_dummy_ridge_model, newx = X_no_dummy_test, s = "lambda.min"))
)

aggregate_100 <- function(df) {
  df %>%
    mutate(chunk = ceiling(seq_len(n()) / 100)) %>%
    group_by(chunk) %>%
    summarise(
      actual_per_100    = sum(actual_y) / n() * 100,
      predicted_per_100 = sum(predicted_y) / n() * 100,
      .groups = "drop"
    )
}

dummy_100    <- aggregate_100(test_results_dummy)
no_dummy_100 <- aggregate_100(test_results_no_dummy)

data.frame(
  model = c("Dummy", "No Dummy"),
  RMSE  = c(sqrt(mean((dummy_100$actual_per_100 - dummy_100$predicted_per_100)^2)),
            sqrt(mean((no_dummy_100$actual_per_100 - no_dummy_100$predicted_per_100)^2))),
  R2    = c(r_squared(dummy_100$actual_per_100, dummy_100$predicted_per_100),
            r_squared(no_dummy_100$actual_per_100, no_dummy_100$predicted_per_100))
)

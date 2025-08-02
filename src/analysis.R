rm(list = ls())

suppressMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
})

# Define paths
current_dir <- dirname(sys.frame(1)$ofile)
all_games_path <- file.path(current_dir, "../data/clean_data/all_games.csv")
long_form_path <- file.path(current_dir, "../data/clean_data/long_form_games.csv")

# Read the combined data
all_games_df <- read_csv(all_games_path, show_col_types = FALSE)
games_long_df <- read_csv(long_form_path, show_col_types = FALSE)

############################### FUNCTIONS ###############################

# Summarise n recent years of data per team
summarise_recent_years <- function(games_long_df, n_years) {

  # Calculate the cutoff date for n years ago
  cutoff_date <- max(games_long_df$Date) - years(n_years)

  # Filter games from the last n years
  recent_games_df <- games_long_df %>%
    filter(Date >= cutoff_date)

  # Summarise statistics per team
  recent_team_stats <- recent_games_df %>%
    group_by(Team) %>%
    summarise(
      Total_Games = n(),
      Total_Points = sum(Points),
      Avg_Points = mean(Points),
      Win_Rate = sum(Result == "Win") / Total_Games,
      Loss_Rate = sum(Result == "Loss") / Total_Games,
      Draw_Rate = sum(Result == "Draw") / Total_Games
    ) %>%
    ungroup()

  # Add n_years column
  recent_team_stats <- recent_team_stats %>%
    mutate(n_Years = n_years)

  return(recent_team_stats)
}

############################ TEAM STATISTICS ############################

# Assign points to each result
games_long_df <- games_long_df %>%
  mutate(
    Points = case_when(
      Result == "Win" ~ 3,
      Result == "Draw" ~ 1,
      TRUE ~ 0
    )
  )

# Determine stats over whole period
all_time_team_df <- games_long_df %>%
  group_by(Team) %>%
  summarise(
    Total_Games = n(),
    Total_Points = sum(Points),
    Avg_Points = mean(Points),
    Win_Rate = sum(Result == "Win") / Total_Games,
    Loss_Rate = sum(Result == "Loss") / Total_Games,
    Draw_Rate = sum(Result == "Draw") / Total_Games
  ) %>%
  ungroup()

# Determine win/loss/draw rates
team_stats_df <- bind_rows(
  summarise_recent_years(games_long_df, 1),
  summarise_recent_years(games_long_df, 3),
  summarise_recent_years(games_long_df, 5),
  summarise_recent_years(games_long_df, 10),
  summarise_recent_years(games_long_df, 15),
  summarise_recent_years(games_long_df, 20),
  summarise_recent_years(games_long_df, 25)
)

# Pivot to wide form form important fields
periods_win_df <- team_stats_df %>%
  select(Team, n_Years, Win_Rate) %>%
  pivot_wider(names_from = n_Years, values_from = Win_Rate, names_prefix = "Win_Rate_")
period_points_df <- team_stats_df %>%
  select(Team, n_Years, Avg_Points) %>%
  pivot_wider(names_from = n_Years, values_from = Avg_Points, names_prefix = "Avg_Points_")

######################### IMPLIED PROBABILITIES #########################

# Split into home and away games
home_games_df <- games_long_df %>%
  filter(Location == "Home")
away_games_df <- games_long_df %>%
  filter(Location == "Away")

# Determine implied loss odds (prob. of opponent win)
home_games_df <- home_games_df %>%
  left_join(
    away_games_df %>%
      select(Game_ID, Win_Pinnacle_Odds, Win_Bet365_Odds) %>%
      rename(
        Loss_Pinnacle_Odds = Win_Pinnacle_Odds,
        Loss_Bet365_Odds = Win_Bet365_Odds
      ),
    by = "Game_ID"
  )
away_games_df <- away_games_df %>%
  left_join(
    home_games_df %>%
      select(Game_ID, Win_Pinnacle_Odds, Win_Bet365_Odds) %>%
      rename(
        Loss_Pinnacle_Odds = Win_Pinnacle_Odds,
        Loss_Bet365_Odds = Win_Bet365_Odds
      ),
    by = "Game_ID"
  )

# Combine back into a single dataframe
games_long_df <- bind_rows(home_games_df, away_games_df)

# Determine implied draw odds
games_long_df <- games_long_df %>%
  left_join(
    all_games_df %>%
      select(Game_ID, Draw_Pinnacle_Odds, Draw_Bet365_Odds),
    by = "Game_ID"
  )

# Calculate implied probabilities
games_long_df <- games_long_df %>%
  mutate(
    Win_Pinnacle_Prob = 1 / Win_Pinnacle_Odds,
    Win_Bet365_Prob = 1 / Win_Bet365_Odds,
    Loss_Pinnacle_Prob = 1 / Loss_Pinnacle_Odds,
    Loss_Bet365_Prob = 1 / Loss_Bet365_Odds,
    Draw_Pinnacle_Prob = 1 / Draw_Pinnacle_Odds,
    Draw_Bet365_Prob = 1 / Draw_Bet365_Odds
  )

# Calculate total implied probabilities
games_long_df <- games_long_df %>%
  mutate(
    Total_Pinnacle_Prob = Win_Pinnacle_Prob + Loss_Pinnacle_Prob + Draw_Pinnacle_Prob,
    Total_Bet365_Prob = Win_Bet365_Prob + Loss_Bet365_Prob + Draw_Bet365_Prob
  )

# Plot distribution of total probabilities with color legend
games_long_df %>%
  filter(!is.na(Total_Pinnacle_Prob) & !is.na(Total_Bet365_Prob)) %>%
  group_by(Game_ID) %>%
  summarise(
    Total_Pinnacle_Prob = mean(Total_Pinnacle_Prob),
    Total_Bet365_Prob = mean(Total_Bet365_Prob)
  ) %>%
  ungroup() %>%
ggplot() +
  geom_density(aes(x = Total_Pinnacle_Prob, fill = "Pinnacle"), alpha = 0.3) +
  geom_density(aes(x = Total_Bet365_Prob, fill = "Bet365"), alpha = 0.3) +
  scale_fill_manual(values = c("Pinnacle" = "blue", "Bet365" = "red"), name = "Source") +
  labs(
    title = "Distribution of Total Implied Probabilities",
    x = "Total Implied Probability",
    y = "Density"
  )
ggsave(file.path(current_dir, "../output/total_probabilities_distribution.png"))

# Per-team Pinnacle statistics
prob_team_df <- games_long_df %>%
  filter(!is.na(Total_Pinnacle_Prob)) %>%
  group_by(Team) %>%
  summarise(
    n = n(),
    Avg_Win_Pinnacle_Prob = mean(Win_Pinnacle_Prob, na.rm = TRUE),
    Avg_Loss_Pinnacle_Prob = mean(Loss_Pinnacle_Prob, na.rm = TRUE),
    Avg_Draw_Pinnacle_Prob = mean(Draw_Pinnacle_Prob, na.rm = TRUE),
    Avg_Total_Pinnacle_Prob = mean(Total_Pinnacle_Prob, na.rm = TRUE)
  ) %>%
  ungroup()

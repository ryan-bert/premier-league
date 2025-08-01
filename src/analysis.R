rm(list = ls())

suppressMessages({
  library(readr)
  library(dplyr)
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
        Lose_Pinnacle_Odds = Win_Pinnacle_Odds,
        Lose_Bet365_Odds = Win_Bet365_Odds
      ),
    by = "Game_ID"
  )
away_games_df <- away_games_df %>%
  left_join(
    home_games_df %>%
      select(Game_ID, Win_Pinnacle_Odds, Win_Bet365_Odds) %>%
      rename(
        Lose_Pinnacle_Odds = Win_Pinnacle_Odds,
        Lose_Bet365_Odds = Win_Bet365_Odds
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
    Lose_Pinnacle_Prob = 1 / Lose_Pinnacle_Odds,
    Lose_Bet365_Prob = 1 / Lose_Bet365_Odds,
    Draw_Pinnacle_Prob = 1 / Draw_Pinnacle_Odds,
    Draw_Bet365_Prob = 1 / Draw_Bet365_Odds
  )

# Calculate total implied probabilities
games_long_df <- games_long_df %>%
  mutate(
    Total_Pinnacle_Prob = Win_Pinnacle_Prob + Lose_Pinnacle_Prob + Draw_Pinnacle_Prob,
    Total_Bet365_Prob = Win_Bet365_Prob + Lose_Bet365_Prob + Draw_Bet365_Prob
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
    Avg_Lose_Pinnacle_Prob = mean(Lose_Pinnacle_Prob, na.rm = TRUE),
    Avg_Draw_Pinnacle_Prob = mean(Draw_Pinnacle_Prob, na.rm = TRUE),
    Avg_Total_Pinnacle_Prob = mean(Total_Pinnacle_Prob, na.rm = TRUE)
  ) %>%
  ungroup()

rm(list = ls())

suppressMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
})

############################ DEFINE VARIABLES ############################

# Define paths
current_dir <- dirname(sys.frame(1)$ofile)
raw_dir <- file.path(current_dir, "../data/raw_data")
clean_dir <- file.path(current_dir, "../data/clean_data")
seasons_dir <- file.path(current_dir, "../data/clean_data/seasons")
raw_files <- list.files(raw_dir, pattern = "\\.csv$", full.names = TRUE)

# Define map of fields
fields_map <- c(
  Date = "Date",
  Home_Team = "HomeTeam",
  Away_Team = "AwayTeam",
  Full_Time_Result = "FTR",
  Home_Full_Time_Goals = "FTHG",
  Away_Full_Time_Goals = "FTAG",
  Home_Shots = "HS",
  Away_Shots = "AS",
  Home_Shots_On_Target = "HST",
  Away_Shots_On_Target = "AST",
  Home_Corners = "HC",
  Away_Corners = "AC",
  Home_Fouls = "HF",
  Away_Fouls = "AF",
  Home_Yellow_Cards = "HY",
  Away_Yellow_Cards = "AY",
  Home_Red_Cards = "HR",
  Away_Red_Cards = "AR",
  Home_Win_Bet365_Odds = "B365H",
  Draw_Bet365_Odds = "B365D",
  Away_Win_Bet365_Odds = "B365A",
  Home_Win_Pinnacle_Odds = "PSCH",
  Draw_Pinnacle_Odds = "PSCD",
  Away_Win_Pinnacle_Odds = "PSCA"
)

########################## CLEAN & COMBINE DATA ##########################

# Define initial df for combined data
all_games_df <- data.frame()

# Iterate over each file
for (file in raw_files) {

  # Load data from CSV file
  season_df <- read_csv(file, show_col_types = FALSE)

  # Determine which columns exist
  cols_to_use <- intersect(fields_map, names(season_df))

  # Select only those columns
  season_df <- season_df %>%
    select(all_of(cols_to_use))

  # Rename columns
  season_df <- season_df %>%
    rename_with(
      ~ names(fields_map)[match(., fields_map)],
      all_of(cols_to_use)
    )

  # No NA values in critical fields
  season_df <- season_df %>%
    filter(
      !is.na(Date) &
      !is.na(Home_Team) &
      !is.na(Away_Team) &
      !is.na(Full_Time_Result) &
      !is.na(Home_Full_Time_Goals) &
      !is.na(Away_Full_Time_Goals)
    )

  # Format the Date column
  season_df <- season_df %>%
    mutate(Date = dmy(Date))

  # Determine season from Date column
  season_df <- season_df %>%
    mutate(Season = paste0(
      min(year(Date)), "-",
      max(year(Date))
    ))

  # Arrange by Date
  season_df <- season_df %>%
    arrange(Date)

  # Save the cleaned season data to a CSV file
  season_name <- paste0("season_", season_df$Season[1], ".csv")
  write_csv(season_df, file.path(seasons_dir, season_name), na = "")

  # Append the season data to the all_games_df
  all_games_df <- all_games_df %>%
    bind_rows(season_df)
}

# Make season 1st column
all_games_df <- all_games_df %>%
  select(Season, everything())

# Arrange by Date
all_games_df <- all_games_df %>%
  arrange(Date)

# Assign match IDs
all_games_df <- all_games_df %>%
  mutate(Match_ID = row_number()) %>%
  select(Match_ID, everything())

# Save the combined game data to a CSV file
write_csv(all_games_df, file.path(clean_dir, "all_games.csv"), na = "")

####################### CONVERT TO LONG-FORM DATA #######################

# Home related data
home_df <- all_games_df %>%
  select(Season, Date, Full_Time_Result, matches("^Home")) %>%
  rename_with(~ sub("^Home_", "", .), everything()) %>%
  mutate(Location = "Home") %>%
  mutate(Result = case_when(
    Full_Time_Result == "H" ~ "Win",
    Full_Time_Result == "D" ~ "Draw",
    Full_Time_Result == "A" ~ "Lose"
  )) %>%
  select(-Full_Time_Result)

# Away related data
away_df <- all_games_df %>%
  select(Season, Date, Full_Time_Result, matches("^Away")) %>%
  rename_with(~ sub("^Away_", "", .), everything()) %>%
  mutate(Location = "Away") %>%
  mutate(Result = case_when(
    Full_Time_Result == "A" ~ "Win",
    Full_Time_Result == "D" ~ "Draw",
    Full_Time_Result == "H" ~ "Lose"
  )) %>%
  select(-Full_Time_Result)

# Combine home and away data into long-form
long_form_df <- bind_rows(home_df, away_df)

# Save the long-form data to a CSV file
write_csv(long_form_df, file.path(clean_dir, "long_form_games.csv"), na = "")
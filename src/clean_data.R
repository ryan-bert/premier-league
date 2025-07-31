suppressMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
})

# Define paths
current_dir <- dirname(sys.frame(1)$ofile)
raw_dir <- file.path(current_dir, "../data/raw_data")
clean_dir <- file.path(current_dir, "../data/clean_data")
seasons_dir <- file.path(current_dir, "../data/clean_data/seasons")

# Define map of fields
fields_map <- c(
  Date = "Date",
  Home_Team = "HomeTeam",
  Away_Team = "AwayTeam",
  Full_Time_Result = "FTR",
  Full_Time_Home_Goals = "FTHG",
  Full_Time_Away_Goals = "FTAG",
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
  Bet365_Home_Odds = "B365H",
  Bet365_Draw_Odds = "B365D",
  Bet365_Away_Odds = "B365A",
  Pinnacle_Home_Odds = "PSCH",
  Pinnacle_Draw_Odds = "PSCD",
  Pinnacle_Away_Odds = "PSCA"
)

# List all CSV files in the raw data directory
raw_files <- list.files(raw_dir, pattern = "\\.csv$", full.names = TRUE)

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
      !is.na(Full_Time_Home_Goals) &
      !is.na(Full_Time_Away_Goals)
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

# Save the combined game data to a CSV file
write_csv(all_games_df, file.path(clean_dir, "all games.csv"), na = "")
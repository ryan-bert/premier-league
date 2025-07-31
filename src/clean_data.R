suppressMessages({
  library(readr)
  library(dplyr)
})

# Define paths
current_dir <- dirname(sys.frame(1)$ofile)
raw_dir <- file.path(current_dir, "../data/raw_data")
clean_dir <- file.path(current_dir, "../data/clean_data")
seasons_dir <- file.path(current_dir, "../data/clean_data/seasons")

# List all CSV files in the raw data directory
raw_files <- list.files(raw_dir, pattern = "\\.csv$", full.names = TRUE)

# Define initial df for combined data
matches_df <- data.frame()

# Iterate over each file
for (file in raw_files) {

  # Load data from CSV file
  season_df <- read_csv(file, show_col_types = FALSE)

  # Column map
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
  
  season_df <- season_df %>%
    select(any_of(fields_map)) %>%
    rename_with(~ names(fields_map)[match(., fields_map)], any_of(fields_map))

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

  # Append the season data to the matches_df
  matches_df <- matches_df %>%
    bind_rows(season_df)
}

# Count no. of NA values in each column put in a data frame
na_counts <- sapply(matches_df, function(x) sum(is.na(x)))
na_counts_df <- data.frame(column = names(na_counts), na_count = na_counts)
rm(list = ls())

suppressMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
})

############################ DEFINE VARIABLES ############################

# Define paths
current_dir <- dirname(sys.frame(1)$ofile)
all_games_path <- file.path(current_dir, "../data/clean_data/all_games.csv")
long_form_path <- file.path(current_dir, "../data/clean_data/long_form_games.csv")

# Read the combined data
all_games_df <- read_csv(all_games_path, show_col_types = FALSE)
games_long_df <- read_csv(long_form_path, show_col_types = FALSE)

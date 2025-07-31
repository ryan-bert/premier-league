suppressMessages({
  library(readr)
  library(dplyr)
})

# Define paths
current_dir <- dirname(sys.frame(1)$ofile)
raw_dir <- file.path(current_dir, "data/raw_data")
clean_dir <- file.path(current_dir, "data/clean_data")


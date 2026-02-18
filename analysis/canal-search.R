# Canal Search - Explore Cached Results ----
#
# Quick script to load cached intermediate data and explore results.
#
# Usage: source("analysis/canal-search.R")

# Setup ----
devtools::load_all()
library(dplyr)

# Load cached data ----
cache_dir <- "analysis/cache"

latest_file <- list.files(
  path = cache_dir,
  pattern = "^intermediate_data_.*\\.rds$",
  full.names = TRUE
) |>
  (\(f) f[which.max(file.info(f)$mtime)])()


intermediate_data <- readRDS(latest_file)

# Plot example city ----
plot_city_canals_map('Manchester', intermediate_data$city_polys, intermediate_data$canals_all)

# View results ----
intermediate_data$results %>% View()

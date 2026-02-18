# UK Canal Analysis - Step by Step Script ----

# Setup ----
devtools::load_all()
library(dplyr)

cache_dir <- "analysis/cache"

latest_file <- list.files(
  path = cache_dir,
  pattern = "^intermediate_data_.*\\.rds$",
  full.names = TRUE
) |>
  (\(f) f[which.max(file.info(f)$mtime)])()


intermediate_data <- readRDS(latest_file)

plot_city_canals_map('Manchester', intermediate_data$city_polys, intermediate_data$canals_all)

intermediate_data$results %>% View()

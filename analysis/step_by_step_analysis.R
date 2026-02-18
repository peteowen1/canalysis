# UK Canal Analysis - Step by Step Script ----

# Setup ----
devtools::load_all()
library(dplyr)

cat("UK Canal Analysis - Step by Step\n")
cat("================================\n\n")

# Step 1: Setup UK Bounding Box ----
cat("Step 1: Setup UK bounding box...\n")
uk_bb_vec <- setup_uk_bbox()
cat("✓ UK bounding box obtained:", paste(round(uk_bb_vec, 2), collapse = ", "), "\n\n")

# Step 2: Fetch UK Canals Data ----
cat("Step 2: Fetch UK canals data...\n")
canals_all <- fetch_uk_canals(uk_bb_vec)
cat("✓ Found", nrow(canals_all), "canal segments\n\n")

# Step 3: Fetch UK Administrative Areas ----
cat("Step 3: Fetch UK cities data...\n")
cities_pts <- fetch_uk_cities(uk_bb_vec)
cat("✓ Found", nrow(cities_pts), "cities\n\n")

# Step 4: Enrich with Population Data ----
cat("Step 4: Get administrative polygons for cities...\n")
city_polys <- get_city_admin_polygons(cities_pts)
cat("✓ Matched", nrow(city_polys), "cities to admin polygons\n\n")

# Step 5: Calculate Canal Metrics ----
cat("Step 5: Calculate canal metrics...\n")
results <- calculate_canal_metrics(city_polys, canals_all)
cat("✓ Canal metrics calculated for", nrow(results), "cities\n\n")

# Format Results ----
results <- results %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

cat("Top 10 cities by canal length:\n")
print(head(results, 10))

cat("\nAnalysis complete! Results stored in 'results' variable.\n")

# Step 6: Save Results ----
cat("\nStep 6: Saving results...\n")

# Create cache directory if it doesn't exist
cache_dir <- "analysis/cache"
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
  cat("✓ Created cache directory:", cache_dir, "\n")
}

## Save as RDS ----
results_file_rds <- file.path(cache_dir, paste0("canal_analysis_results_", Sys.Date(), ".rds"))
saveRDS(results, results_file_rds)
cat("✓ Results saved as RDS:", results_file_rds, "\n")

## Save as CSV ----
results_file_csv <- file.path(cache_dir, paste0("canal_analysis_results_", Sys.Date(), ".csv"))
# Clean data for CSV export (remove complex data types)
results_for_csv <- results %>%
  dplyr::select(-any_of("length_m")) %>%  # Remove units column if present
  dplyr::mutate(across(where(is.list), ~ as.character(.x)))  # Convert any remaining lists to character
write.csv(results_for_csv, results_file_csv, row.names = FALSE)
cat("✓ Results saved as CSV:", results_file_csv, "\n")

## Save Intermediate Data ----
intermediate_file <- file.path(cache_dir, paste0("intermediate_data_", Sys.Date(), ".rds"))
intermediate_data <- list(
  uk_bb_vec = uk_bb_vec,
  canals_all = canals_all,
  cities_pts = cities_pts,
  city_polys = city_polys,
  results = results
)
saveRDS(intermediate_data, intermediate_file)
cat("✓ Intermediate data saved:", intermediate_file, "\n")

## Summary ----
cat("\nSaved files:\n")
cat("- Results (RDS):", results_file_rds, "\n")
cat("- Results (CSV):", results_file_csv, "\n")
cat("- Intermediate data:", intermediate_file, "\n")

# Visualization (Optional) ----
# Example usage of the fast plotting function:
# plot_city_canals_map('Halton', city_polys, canals_all)
# plot_city_canals_map('Salford', city_polys, canals_all)
# plot_city_canals_map('Manchester', city_polys, canals_all)
# plot_city_canals_map('Birmingham', city_polys, canals_all)
# plot_city_canals_map('Greater London', city_polys, canals_all)
# plot_all_city_polygons(city_polys)

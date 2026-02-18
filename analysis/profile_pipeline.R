# Profile Pipeline - Find Bottlenecks ----
#
# Profile each step of rank_uk_cities_by_canals() to find bottlenecks.
#
# Usage: source("analysis/profile_pipeline.R")

# Setup ----
devtools::load_all()

cat("=== Pipeline Profiling ===\n\n")

# Individual step timings ----
timings <- list()

t0 <- proc.time()
uk_bb_vec <- setup_uk_bbox()
timings$setup_bbox <- (proc.time() - t0)[["elapsed"]]
cat(sprintf("1. setup_uk_bbox():          %5.2fs\n", timings$setup_bbox))

t0 <- proc.time()
canals_all <- fetch_uk_canals(uk_bb_vec)
timings$fetch_canals <- (proc.time() - t0)[["elapsed"]]
cat(sprintf("2. fetch_uk_canals():        %5.2fs  (%d canals, %d columns)\n",
            timings$fetch_canals, nrow(canals_all), ncol(canals_all)))

t0 <- proc.time()
cities_pts <- fetch_uk_cities(uk_bb_vec)
timings$fetch_cities <- (proc.time() - t0)[["elapsed"]]
cat(sprintf("3. fetch_uk_cities():        %5.2fs  (%d admin areas)\n",
            timings$fetch_cities, nrow(cities_pts)))

t0 <- proc.time()
city_polys <- get_city_admin_polygons(cities_pts)
timings$admin_polys <- (proc.time() - t0)[["elapsed"]]
cat(sprintf("4. get_city_admin_polygons():%5.2fs  (%d polygons)\n",
            timings$admin_polys, nrow(city_polys)))

# calculate_canal_metrics() breakdown ----
cat("\n--- calculate_canal_metrics() breakdown ---\n")

t0 <- proc.time()
canals_geom <- sf::st_geometry(canals_all)
cities_bbox <- sf::st_as_sfc(sf::st_bbox(city_polys))
sf::st_crs(cities_bbox) <- sf::st_crs(city_polys)
in_bbox <- sf::st_intersects(canals_geom, cities_bbox, sparse = FALSE)[, 1]
canals_geom <- canals_geom[in_bbox]
timings$bbox_filter <- (proc.time() - t0)[["elapsed"]]
cat(sprintf("5a. bbox pre-filter:         %5.2fs  (%d -> %d canals)\n",
            timings$bbox_filter, nrow(canals_all), length(canals_geom)))

t0 <- proc.time()
city_utm <- sf::st_transform(city_polys, 32630)
canals_utm <- sf::st_transform(sf::st_sfc(canals_geom, crs = sf::st_crs(canals_all)), 32630)
timings$transform <- (proc.time() - t0)[["elapsed"]]
cat(sprintf("5b. st_transform (both):     %5.2fs\n", timings$transform))

t0 <- proc.time()
clipped <- suppress_sf_attr_warnings(sf::st_intersection(city_utm, canals_utm))
timings$intersection <- (proc.time() - t0)[["elapsed"]]
cat(sprintf("5c. st_intersection:         %5.2fs  (%d clipped segments)\n",
            timings$intersection, nrow(clipped)))

t0 <- proc.time()
clipped <- clipped[!sf::st_is_empty(clipped), ]
clipped$length_m <- sf::st_length(clipped)
timings$length <- (proc.time() - t0)[["elapsed"]]
cat(sprintf("5d. st_length + filter:      %5.2fs\n", timings$length))

t0 <- proc.time()
canal_lengths <- clipped |>
  sf::st_drop_geometry() |>
  dplyr::group_by(city) |>
  dplyr::summarise(total_length_m = sum(as.numeric(length_m), na.rm = TRUE), .groups = "drop")
timings$aggregate <- (proc.time() - t0)[["elapsed"]]
cat(sprintf("5e. aggregate + join:        %5.2fs\n", timings$aggregate))

# Summary ----
total <- sum(unlist(timings))
cat(sprintf("\n=== TOTAL: %.2fs ===\n", total))
cat(sprintf("\nTop bottlenecks:\n"))
sorted <- sort(unlist(timings), decreasing = TRUE)
for (nm in names(sorted)) {
  cat(sprintf("  %-25s %5.2fs (%4.1f%%)\n", nm, sorted[nm], 100 * sorted[nm] / total))
}

# Full pipeline end-to-end ----
cat("\n--- Full pipeline end-to-end ---\n")
t0 <- proc.time()
res <- rank_uk_cities_by_canals()
full_time <- (proc.time() - t0)[["elapsed"]]
cat(sprintf("rank_uk_cities_by_canals(): %5.2fs  (%d rows)\n", full_time, nrow(res)))

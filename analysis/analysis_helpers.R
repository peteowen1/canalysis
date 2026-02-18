library(canalysis)

run_step <- function(step_name, step_function, ...) {
  cat("Running:", step_name, "...\n")
  start_time <- Sys.time()
  result <- step_function(...)
  end_time <- Sys.time()
  cat("✓ Completed in", round(as.numeric(end_time - start_time, units = "secs"), 1), "seconds\n\n")
  result
}

save_intermediate_result <- function(obj, name, dir = "analysis/cache") {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file_path <- file.path(dir, paste0(name, ".rds"))
  saveRDS(obj, file_path)
  cat("💾 Saved", name, "to", file_path, "\n")
  invisible(file_path)
}

load_intermediate_result <- function(name, dir = "analysis/cache") {
  file_path <- file.path(dir, paste0(name, ".rds"))
  if (file.exists(file_path)) {
    cat("📂 Loading", name, "from", file_path, "\n")
    readRDS(file_path)
  } else {
    cat("❌ No cached file found for", name, "\n")
    NULL
  }
}

run_individual_step <- function(step_number, overpass_url = NULL,
                               use_cache = TRUE, use_parallel = TRUE, workers = NULL) {
  switch(step_number,
    "1" = {
      cat("=== STEP 1: Setup UK Bounding Box ===\n")
      run_step("Setup UK bounding box", setup_uk_bbox, overpass_url)
    },
    "2" = {
      cat("=== STEP 2: Fetch UK Canals ===\n")
      uk_bb_vec <- load_intermediate_result("uk_bb_vec")
      if (is.null(uk_bb_vec)) {
        cat("Need to run Step 1 first!\n")
        return(NULL)
      }
      run_step("Fetch UK canals", fetch_uk_canals, uk_bb_vec, use_cache)
    },
    "3" = {
      cat("=== STEP 3: Fetch UK Cities ===\n")
      uk_bb_vec <- load_intermediate_result("uk_bb_vec")
      if (is.null(uk_bb_vec)) {
        cat("Need to run Step 1 first!\n")
        return(NULL)
      }
      run_step("Fetch UK cities", fetch_uk_cities, uk_bb_vec, use_cache)
    },
    "4" = {
      cat("=== STEP 4: Get City Admin Polygons ===\n")
      cities_pts <- load_intermediate_result("cities_pts")
      if (is.null(cities_pts)) {
        cat("Need to run Step 3 first!\n")
        return(NULL)
      }
      run_step("Get city admin polygons", get_city_admin_polygons, cities_pts)
    },
    "5" = {
      cat("=== STEP 5: Calculate Canal Metrics ===\n")
      city_polys <- load_intermediate_result("city_polys")
      canals_all <- load_intermediate_result("canals_all")
      if (is.null(city_polys) || is.null(canals_all)) {
        cat("Need to run Steps 2 and 4 first!\n")
        return(NULL)
      }
      run_step("Calculate canal metrics", calculate_canal_metrics, city_polys, canals_all, use_parallel, workers)
    },
    stop("Invalid step number. Use 1-5.")
  )
}

run_full_analysis_with_caching <- function(overpass_url = NULL,
                                          force_refresh = FALSE, use_cache = TRUE,
                                          use_parallel = TRUE, workers = NULL) {
  cat("=== FULL UK CANAL ANALYSIS WITH CACHING ===\n\n")

  if (!force_refresh && !is.null(load_intermediate_result("results"))) {
    cat("Found cached results. Use force_refresh = TRUE to re-run.\n")
    return(load_intermediate_result("results"))
  }

  uk_bb_vec <- run_individual_step("1", overpass_url = overpass_url,
                                  use_cache = use_cache, use_parallel = use_parallel, workers = workers)
  save_intermediate_result(uk_bb_vec, "uk_bb_vec")

  canals_all <- run_individual_step("2", use_cache = use_cache, use_parallel = use_parallel, workers = workers)
  save_intermediate_result(canals_all, "canals_all")

  cities_pts <- run_individual_step("3", use_cache = use_cache, use_parallel = use_parallel, workers = workers)
  save_intermediate_result(cities_pts, "cities_pts")

  city_polys <- run_individual_step("4", use_cache = use_cache, use_parallel = use_parallel, workers = workers)
  save_intermediate_result(city_polys, "city_polys")

  results <- run_individual_step("5", use_cache = use_cache, use_parallel = use_parallel, workers = workers)
  save_intermediate_result(results, "results")

  cat("🎉 Full analysis complete!\n")
  results
}

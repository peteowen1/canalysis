# Persistent cache directory (survives R restarts)
.cache_dir <- function() {
  d <- tools::R_user_dir("canalysis", "cache")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

# Overpass API timeout in seconds for all OSM queries
.osm_timeout <- 450

# Package-level environment for memoised function singletons
.memo_env <- new.env(parent = emptyenv())

.get_cached_fn <- function(name, fn) {
  if (is.null(.memo_env[[name]])) {
    .memo_env[[name]] <- memoise::memoise(fn, cache = cachem::cache_disk(.cache_dir()))
  }
  .memo_env[[name]]
}

#' Clear cached OSM data
#'
#' Removes the persistent disk cache used by fetch_uk_canals() and fetch_uk_cities().
#'
#' @return The cache directory path (invisibly)
#' @export
clear_cache <- function() {
  d <- tools::R_user_dir("canalysis", "cache")
  if (dir.exists(d)) unlink(d, recursive = TRUE)
  # Reset memoised singletons so they pick up the fresh (empty) cache
  rm(list = ls(.memo_env), envir = .memo_env)
  message("Cache cleared: ", d)
  invisible(d)
}

#' Setup UK bounding box for analysis
#'
#' Configures Overpass URL and retrieves UK bounding box coordinates.
#'
#' @param overpass_url Optional Overpass endpoint (character). If NULL, uses default.
#' @return Numeric vector with UK bounding box coordinates (xmin, ymin, xmax, ymax)
#' @export
setup_uk_bbox <- function(overpass_url = NULL) {
  old_opts <- options(osmdata_bbox = NULL)
  on.exit(options(old_opts), add = TRUE)
  if (!is.null(overpass_url)) {
    old_overpass <- options(osmdata_overpass_url = overpass_url)
    on.exit(options(old_overpass), add = TRUE, after = FALSE)
  }

  uk_bb_matrix <- tryCatch(
    osmdata::getbb("United Kingdom", format_out = "matrix"),
    error = function(e) stop("Failed to fetch UK bounding box: ", conditionMessage(e), call. = FALSE)
  )
  as.numeric(c(uk_bb_matrix[1,1], uk_bb_matrix[2,1], uk_bb_matrix[1,2], uk_bb_matrix[2,2]))
}

#' Fetch UK canals from OpenStreetMap
#'
#' Downloads and filters canal data from OpenStreetMap within UK boundaries.
#'
#' @param uk_bb_vec UK bounding box vector from setup_uk_bbox()
#' @param use_cache Logical, whether to use disk caching for OSM queries
#' @return sf object with filtered canal lines
#' @export
fetch_uk_canals <- function(uk_bb_vec, use_cache = TRUE) {
  .fetch_and_process_canals <- function(bbox) {
    q_canals <- osmdata::opq(bbox = bbox, timeout = .osm_timeout) |>
      osmdata::add_osm_feature("waterway", "canal")
    dat_canals <- osmdata::osmdata_sf(q_canals)

    filtered <- dat_canals$osm_lines |>
      ensure_chr_cols(c("disused","abandoned","proposed","canal")) |>
      dplyr::filter(is.na(disused)  | disused  != "yes",
                    is.na(abandoned)| abandoned!= "yes",
                    is.na(proposed) | proposed != "yes",
                    is.na(canal)    | !(canal %in% c("drain","irrigation"))) |>
      dplyr::distinct(osm_id, .keep_all = TRUE)

    # Strip to geometry-only sf — downstream only needs line geometries,
    # not the dozens of OSM tag columns. This speeds up transform/intersection.
    result <- sf::st_sf(geometry = sf::st_geometry(filtered)) |>
      sf::st_make_valid()

    # Ensure WGS84 CRS is set (OSM data should be in WGS84)
    if (is.na(sf::st_crs(result))) {
      result <- sf::st_set_crs(result, 4326)
    }
    result
  }

  if (use_cache) {
    .get_cached_fn("canals", .fetch_and_process_canals)(uk_bb_vec)
  } else {
    .fetch_and_process_canals(uk_bb_vec)
  }
}

# Internal helper to run the 4 OSM admin boundary queries
.fetch_admin_boundaries <- function(bbox) {
  # Query for metropolitan districts (admin_level=8, designation=metropolitan_district)
  q_metro <- osmdata::opq(bbox = bbox, timeout = .osm_timeout) |>
    osmdata::add_osm_feature("boundary", "administrative") |>
    osmdata::add_osm_feature("admin_level", "8") |>
    osmdata::add_osm_feature("designation", "metropolitan_district")
  dat_metro <- osmdata::osmdata_sf(q_metro)

  # Query for unitary authorities (admin_level=6, designation=unitary_authority)
  q_unitary <- osmdata::opq(bbox = bbox, timeout = .osm_timeout) |>
    osmdata::add_osm_feature("boundary", "administrative") |>
    osmdata::add_osm_feature("admin_level", "6") |>
    osmdata::add_osm_feature("designation", "unitary_authority")
  dat_unitary <- osmdata::osmdata_sf(q_unitary)

  # Query for combined authorities (admin_level=5) with specific designation
  q_combined <- osmdata::opq(bbox = bbox, timeout = .osm_timeout) |>
    osmdata::add_osm_feature("boundary", "administrative") |>
    osmdata::add_osm_feature("admin_level", "5") |>
    osmdata::add_osm_feature("designation", "combined_authority")
  dat_combined <- osmdata::osmdata_sf(q_combined)

  # Query for Greater London specifically (may not have combined_authority designation)
  q_london <- osmdata::opq(bbox = bbox, timeout = .osm_timeout) |>
    osmdata::add_osm_feature("boundary", "administrative") |>
    osmdata::add_osm_feature("admin_level", "5") |>
    osmdata::add_osm_feature("name", "Greater London")
  dat_london <- osmdata::osmdata_sf(q_london)

  list(metro = dat_metro, unitary = dat_unitary, combined = dat_combined, london = dat_london)
}

#' Fetch UK administrative boundary polygons from OpenStreetMap
#'
#' Downloads metropolitan districts, unitary authorities, and combined authorities boundary polygons from OpenStreetMap.
#' Queries for admin_level=8 (metropolitan districts), admin_level=6 (unitary authorities), and admin_level=5 (combined authorities including Greater London).
#'
#' @param uk_bb_vec UK bounding box vector from setup_uk_bbox()
#' @param use_cache Logical, whether to use disk caching for OSM queries
#' @return sf object with full administrative boundary polygons
#' @export
fetch_uk_cities <- function(uk_bb_vec, use_cache = TRUE) {

  .fetch_and_process_cities <- function(bbox) {
    dat_all <- .fetch_admin_boundaries(bbox)

    # Strip each OSM result to name + geometry BEFORE binding to avoid
    # accumulating dozens of OSM tag columns (prevents 914 MB OOM crash)
    .strip_osm <- function(sf_obj) {
      if (is.null(sf_obj) || nrow(sf_obj) == 0) return(NULL)
      sf_obj[, c("name", "geometry")]
    }

    metro_polys <- sf_bind_rows(list(.strip_osm(dat_all$metro$osm_multipolygons), .strip_osm(dat_all$metro$osm_polygons)))
    if (!is.null(metro_polys) && nrow(metro_polys) > 0) {
      metro_polys$admin_type <- "Metropolitan District"
    }

    unitary_polys <- if (!is.null(dat_all$unitary)) sf_bind_rows(list(.strip_osm(dat_all$unitary$osm_multipolygons), .strip_osm(dat_all$unitary$osm_polygons))) else NULL
    if (!is.null(unitary_polys) && nrow(unitary_polys) > 0) {
      unitary_polys$admin_type <- "Unitary Authority"
    }

    combined_polys <- sf_bind_rows(list(.strip_osm(dat_all$combined$osm_multipolygons), .strip_osm(dat_all$combined$osm_polygons)))
    if (!is.null(combined_polys) && nrow(combined_polys) > 0) {
      combined_polys$admin_type <- "Combined Authority"
    }

    london_polys <- sf_bind_rows(list(.strip_osm(dat_all$london$osm_multipolygons), .strip_osm(dat_all$london$osm_polygons)))
    if (!is.null(london_polys) && nrow(london_polys) > 0) {
      london_polys$admin_type <- "Combined Authority"  # Greater London is also a combined authority
    }

    all_polys <- sf_bind_rows(list(metro_polys, unitary_polys, combined_polys, london_polys))

    if (is.null(all_polys) || nrow(all_polys) == 0) {
      stop("No metropolitan districts, unitary authorities, or combined authorities found")
    }

    result <- all_polys |>
      dplyr::mutate(name = as.character(name)) |>
      dplyr::filter(!is.na(name), !sf::st_is_empty(geometry)) |>
      sf::st_make_valid()

    # Ensure WGS84 CRS is set (OSM data should be in WGS84)
    if (is.na(sf::st_crs(result))) {
      result <- sf::st_set_crs(result, 4326)
    }

    result |>
      dplyr::select(name, admin_type, geometry)
  }

  if (use_cache) {
    result <- .get_cached_fn("cities", .fetch_and_process_cities)(uk_bb_vec)
  } else {
    result <- .fetch_and_process_cities(uk_bb_vec)
  }

  message("Found ", nrow(result), " administrative areas")
  result
}


#' Get administrative polygons for cities
#'
#' Enriches administrative boundary polygons with population data from CSV file
#' and calculates area in km².
#'
#' @param admin_polys sf object with administrative boundary polygons from fetch_uk_cities()
#' @param min_area_km2 Minimum area in km² to include (default 0.1)
#' @param min_area_no_pop_km2 Minimum area for areas without population data (default 1.0)
#' @return sf object with administrative polygons including population and area_km2 data
#' @export
get_city_admin_polygons <- function(admin_polys, min_area_km2 = 0.1, min_area_no_pop_km2 = 1.0) {

  # Load population data from CSV
  csv_path <- system.file("extdata", "uk_metropolitan_districts.csv", package = "canalysis")
  if (!nzchar(csv_path)) {
    # Fallback to inst/extdata/ relative to package root for development
    dev_path <- file.path(getwd(), "inst", "extdata", "uk_metropolitan_districts.csv")
    if (file.exists(dev_path)) csv_path <- dev_path
  }

  if (nzchar(csv_path) && file.exists(csv_path)) {
    pop_data <- utils::read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE)
    expected_cols <- c("Code", "Name", "Geography")
    if (ncol(pop_data) < 6 || !all(expected_cols %in% names(pop_data))) {
      stop("Population CSV has unexpected structure. Expected columns: Code, Name, Geography, ...",
           "\nGot: ", paste(names(pop_data), collapse = ", "), call. = FALSE)
    }
    # Select only the first 6 columns (extra trailing columns from Excel are ignored)
    pop_data <- pop_data[, 1:6]
    names(pop_data) <- c("Code", "Name", "Geography", "Area_sq_km", "Population_mid_2022", "People_per_sq_km")

    pop_data$clean_name <- clean_admin_name(pop_data$Name)
    pop_data$name_lower <- tolower(pop_data$Name)
    pop_data$clean_name_lower <- tolower(pop_data$clean_name)

    # Build a city-keyed lookup: city name -> population
    # Work on a plain data frame (no geometry) for joining
    city_keys <- data.frame(
      city = admin_polys$name,
      name_lower = tolower(admin_polys$name),
      clean_name_lower = tolower(clean_admin_name(admin_polys$name)),
      stringsAsFactors = FALSE
    )

    # Join 1: exact name match
    pop_exact <- pop_data[, c("name_lower", "Population_mid_2022")]
    names(pop_exact) <- c("name_lower", "pop_exact")
    city_keys <- dplyr::left_join(city_keys, pop_exact, by = "name_lower")

    # Join 2: cleaned name match
    pop_clean <- pop_data[, c("clean_name_lower", "Population_mid_2022")]
    names(pop_clean) <- c("clean_name_lower", "pop_clean")
    city_keys <- dplyr::left_join(city_keys, pop_clean, by = "clean_name_lower")

    city_keys$population <- dplyr::coalesce(
      as.numeric(city_keys$pop_exact),
      as.numeric(city_keys$pop_clean)
    )

    # Fuzzy fallback using edit distance for remaining NAs
    max_edit_dist <- 10L
    unmatched <- which(is.na(city_keys$population))
    for (idx in unmatched) {
      city_clean <- city_keys$clean_name_lower[idx]
      if (nchar(city_clean) < 4L) next  # skip short names to avoid false matches
      dists <- as.integer(utils::adist(city_clean, pop_data$clean_name_lower))
      best_dist <- min(dists)
      if (best_dist > max_edit_dist) next  # reject poor matches entirely
      best_idx <- which(dists == best_dist)
      # If tied, pick the first (alphabetical) — deterministic and auditable
      best <- best_idx[1]
      pop_val <- pop_data$Population_mid_2022[best]
      if (!is.na(pop_val)) {
        city_keys$population[idx] <- as.numeric(pop_val)
        message("Fuzzy population match (edit dist ", best_dist, "): '",
                city_keys$city[idx], "' -> '", pop_data$Name[best], "'")
      }
    }

    # Join population back to admin_polys by key (not by position)
    pop_lookup <- city_keys[, c("city", "population")]
    result <- admin_polys |>
      dplyr::mutate(city = name) |>
      dplyr::left_join(pop_lookup, by = "city") |>
      dplyr::select(city, admin_type, population, geometry) |>
      dplyr::distinct(city, .keep_all = TRUE) |>
      sf::st_make_valid()

  } else {
    message("Warning: Population CSV file not found. Population data will be NA.")
    result <- admin_polys |>
      dplyr::mutate(city = name, population = NA_real_) |>
      dplyr::select(city, admin_type, population, geometry) |>
      dplyr::distinct(city, .keep_all = TRUE) |>
      sf::st_make_valid()
  }

  # Add area in km² using UTM projection for accurate area calculation
  # Single vectorized transform + area call (UTM 30N covers all UK)
  result_utm <- sf::st_transform(result, 32630)
  result$area_km2 <- as.numeric(units::set_units(sf::st_area(result_utm), "km^2"))

  # Filter out very small areas or areas without population data
  result <- result |>
    dplyr::filter(
      area_km2 >= min_area_km2 &
      (area_km2 >= min_area_no_pop_km2 | (!is.na(population) & population > 0))
    )

  result
}

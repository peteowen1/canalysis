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
  if (!is.null(overpass_url)) options(osmdata_overpass_url = overpass_url)

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
  if (use_cache) {
    cache_dir <- file.path(tempdir(), "canalysis_cache")
    if (!dir.exists(cache_dir)) dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

    fetch_canals_internal <- function(bbox) {
      q_canals <- osmdata::opq(bbox = bbox, timeout = 450) |>
        osmdata::add_osm_feature("waterway", "canal")
      osmdata::osmdata_sf(q_canals)
    }

    fetch_canals_cached <- memoise::memoise(fetch_canals_internal,
                                            cache = cachem::cache_disk(cache_dir))
    dat_canals <- fetch_canals_cached(uk_bb_vec)
  } else {
    q_canals <- osmdata::opq(bbox = uk_bb_vec, timeout = 450) |>
      osmdata::add_osm_feature("waterway", "canal")
    dat_canals <- osmdata::osmdata_sf(q_canals)
  }
  result <- dat_canals$osm_lines |>
    ensure_chr_cols(c("disused","abandoned","proposed","canal")) |>
    dplyr::filter(is.na(disused)  | disused  != "yes",
                  is.na(abandoned)| abandoned!= "yes",
                  is.na(proposed) | proposed != "yes",
                  is.na(canal)    | !(canal %in% c("drain","irrigation"))) |>
    dplyr::distinct(osm_id, .keep_all = TRUE) |>
    sf::st_make_valid()

  # Ensure WGS84 CRS is set (OSM data should be in WGS84)
  if (is.na(sf::st_crs(result))) {
    result <- sf::st_set_crs(result, 4326)
  }

  result
}

# Internal helper to run the 4 OSM admin boundary queries
.fetch_admin_boundaries <- function(bbox) {
  # Query for metropolitan districts (admin_level=8, designation=metropolitan_district)
  q_metro <- osmdata::opq(bbox = bbox, timeout = 450) |>
    osmdata::add_osm_feature("boundary", "administrative") |>
    osmdata::add_osm_feature("admin_level", "8") |>
    osmdata::add_osm_feature("designation", "metropolitan_district")
  dat_metro <- osmdata::osmdata_sf(q_metro)

  # Query for unitary authorities (admin_level=6, designation=unitary_authority)
  q_unitary <- osmdata::opq(bbox = bbox, timeout = 450) |>
    osmdata::add_osm_feature("boundary", "administrative") |>
    osmdata::add_osm_feature("admin_level", "6") |>
    osmdata::add_osm_feature("designation", "unitary_authority")
  dat_unitary <- osmdata::osmdata_sf(q_unitary)

  # Query for combined authorities (admin_level=5) with specific designation
  q_combined <- osmdata::opq(bbox = bbox, timeout = 450) |>
    osmdata::add_osm_feature("boundary", "administrative") |>
    osmdata::add_osm_feature("admin_level", "5") |>
    osmdata::add_osm_feature("designation", "combined_authority")
  dat_combined <- osmdata::osmdata_sf(q_combined)

  # Query for Greater London specifically (may not have combined_authority designation)
  q_london <- osmdata::opq(bbox = bbox, timeout = 450) |>
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

  if (use_cache) {
    cache_dir <- file.path(tempdir(), "canalysis_cache")
    if (!dir.exists(cache_dir)) dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

    fetch_cached <- memoise::memoise(.fetch_admin_boundaries,
                                     cache = cachem::cache_disk(cache_dir))
    dat_all <- fetch_cached(uk_bb_vec)
  } else {
    dat_all <- .fetch_admin_boundaries(uk_bb_vec)
  }

  # Combine metropolitan districts, unitary authorities, and combined authorities polygons
  metro_polys <- sf_bind_rows(list(dat_all$metro$osm_multipolygons, dat_all$metro$osm_polygons))
  if (!is.null(metro_polys) && nrow(metro_polys) > 0) {
    metro_polys$admin_type <- "Metropolitan District"
  }

  unitary_polys <- if (!is.null(dat_all$unitary)) sf_bind_rows(list(dat_all$unitary$osm_multipolygons, dat_all$unitary$osm_polygons)) else NULL
  if (!is.null(unitary_polys) && nrow(unitary_polys) > 0) {
    unitary_polys$admin_type <- "Unitary Authority"
  }

  combined_polys <- sf_bind_rows(list(dat_all$combined$osm_multipolygons, dat_all$combined$osm_polygons))
  if (!is.null(combined_polys) && nrow(combined_polys) > 0) {
    combined_polys$admin_type <- "Combined Authority"
  }

  london_polys <- sf_bind_rows(list(dat_all$london$osm_multipolygons, dat_all$london$osm_polygons))
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

  message("Found ", nrow(result), " administrative areas (",
          ifelse(is.null(metro_polys), 0, nrow(metro_polys)), " metropolitan districts + ",
          ifelse(is.null(unitary_polys), 0, nrow(unitary_polys)), " unitary authorities + ",
          ifelse(is.null(combined_polys), 0, nrow(combined_polys)), " combined authorities + ",
          ifelse(is.null(london_polys), 0, nrow(london_polys)), " Greater London)")

  # Return the full administrative polygons, not centroids
  result |>
    dplyr::select(name, admin_type, geometry) |>
    sf::st_make_valid()
}


#' Get administrative polygons for cities
#'
#' Enriches administrative boundary polygons with population data from CSV file
#' and calculates area in km².
#'
#' @param admin_polys sf object with administrative boundary polygons from fetch_uk_cities()
#' @return sf object with administrative polygons including population and area_km2 data
#' @export
get_city_admin_polygons <- function(admin_polys) {

  # Load population data from CSV
  csv_path <- system.file("extdata", "uk_metropolitan_districts.csv", package = "canalysis")
  if (!file.exists(csv_path)) {
    # Fallback to inst/extdata/ directory for development
    csv_path <- "inst/extdata/uk_metropolitan_districts.csv"
  }

  if (file.exists(csv_path)) {
    pop_data <- utils::read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE)
    expected_cols <- c("Code", "Name", "Geography")
    if (length(names(pop_data)) < 6 || !all(expected_cols %in% names(pop_data))) {
      stop("Population CSV has unexpected structure. Expected columns: Code, Name, Geography, ...",
           "\nGot: ", paste(names(pop_data), collapse = ", "), call. = FALSE)
    }
    names(pop_data) <- c("Code", "Name", "Geography", "Area_sq_km", "Population_mid_2022", "People_per_sq_km")

    # Clean names for joining
    clean_name <- function(x) {
      x <- gsub(", City of$", "", x)
      x <- gsub(" City$", "", x)
      x <- gsub(" Borough$", "", x)
      x <- gsub(" Council$", "", x)
      x <- gsub("upon", "on", x, ignore.case = TRUE)
      trimws(x)
    }

    pop_data$clean_name <- clean_name(pop_data$Name)
    pop_data$name_lower <- tolower(pop_data$Name)
    pop_data$clean_name_lower <- tolower(pop_data$clean_name)

    # Prepare admin polygons with join keys
    polys <- admin_polys |>
      dplyr::mutate(
        city = name,
        name_lower = tolower(name),
        clean_name_lower = tolower(clean_name(name))
      )

    # Join 1: exact name match
    pop_exact <- pop_data[, c("name_lower", "Population_mid_2022")]
    names(pop_exact) <- c("name_lower", "pop_exact")
    polys <- dplyr::left_join(sf::st_drop_geometry(polys) |> dplyr::select(city, name_lower, clean_name_lower),
                              pop_exact, by = "name_lower") |>
      dplyr::left_join(
        stats::setNames(pop_data[, c("clean_name_lower", "Population_mid_2022")], c("clean_name_lower", "pop_clean")),
        by = "clean_name_lower"
      ) |>
      dplyr::mutate(population = dplyr::coalesce(as.numeric(pop_exact), as.numeric(pop_clean)))

    # Fuzzy substring fallback for remaining NAs — require minimum name length
    # and collect all candidates to pick the best match
    unmatched <- which(is.na(polys$population))
    for (idx in unmatched) {
      city_clean <- polys$clean_name_lower[idx]
      if (nchar(city_clean) < 4) next  # skip very short names to avoid false matches
      candidates <- integer(0)
      for (i in seq_len(nrow(pop_data))) {
        csv_clean <- pop_data$clean_name_lower[i]
        if (grepl(city_clean, csv_clean, fixed = TRUE) ||
            grepl(csv_clean, city_clean, fixed = TRUE)) {
          candidates <- c(candidates, i)
        }
      }
      if (length(candidates) == 1) {
        pop_val <- pop_data$Population_mid_2022[candidates]
        if (!is.na(pop_val)) {
          polys$population[idx] <- as.numeric(pop_val)
          message("Fuzzy population match: '", polys$city[idx], "' -> '", pop_data$Name[candidates], "'")
        }
      } else if (length(candidates) > 1) {
        # Multiple fuzzy matches — pick the one with the closest name length
        dists <- abs(nchar(pop_data$clean_name_lower[candidates]) - nchar(city_clean))
        best <- candidates[which.min(dists)]
        pop_val <- pop_data$Population_mid_2022[best]
        if (!is.na(pop_val)) {
          polys$population[idx] <- as.numeric(pop_val)
          message("Fuzzy population match (best of ", length(candidates), "): '",
                  polys$city[idx], "' -> '", pop_data$Name[best], "'")
        }
      }
    }

    # Re-attach geometry from original admin_polys
    result <- admin_polys |>
      dplyr::mutate(
        city = name,
        population = polys$population
      ) |>
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
  result$area_km2 <- purrr::map_dbl(result$geometry, function(geom) {
    poly <- sf::st_sf(geometry = sf::st_sfc(geom, crs = sf::st_crs(result)))
    epsg <- utm_epsg_for(poly)
    units::drop_units(units::set_units(sf::st_area(sf::st_transform(poly, epsg)), "km^2"))
  })

  # Filter out places with very small areas (< 0.1 km²) or missing/invalid population data for small areas
  result <- result |>
    dplyr::filter(
      area_km2 >= 0.1 &  # Minimum area threshold
      (area_km2 >= 1.0 | (!is.na(population) & population > 0))  # Either large enough or has valid population
    )

  result
}

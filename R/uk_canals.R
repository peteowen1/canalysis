#' Setup UK bounding box for analysis
#'
#' Configures Overpass URL and retrieves UK bounding box coordinates.
#'
#' @param overpass_url Optional Overpass endpoint (character). If NULL, uses default.
#' @return Numeric vector with UK bounding box coordinates (xmin, ymin, xmax, ymax)
#' @export
setup_uk_bbox <- function(overpass_url = NULL) {
  if (!is.null(overpass_url)) options(osmdata_overpass_url = overpass_url)
  options(osmdata_bbox = NULL)

  uk_bb_matrix <- tryCatch(osmdata::getbb("United Kingdom", format_out = "matrix"), error = function(e) NULL)
  stopifnot(!is.null(uk_bb_matrix))
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

    message("Found ", nrow(dat_all$metro$osm_multipolygons), " metro areas")
    message("Found ", nrow(dat_all$unitary$osm_multipolygons), " unitary areas")
    message("Found ", nrow(dat_all$combined$osm_multipolygons), " combined authorities")
    message("Found ", nrow(dat_all$london$osm_multipolygons), " Greater London areas")
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
    # Fallback to data/ directory for development
    csv_path <- "data/uk_metropolitan_districts.csv"
  }

  if (file.exists(csv_path)) {
    pop_data <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
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

    # Join 3: fuzzy substring fallback for remaining NAs
    unmatched <- which(is.na(polys$population))
    for (idx in unmatched) {
      city_clean <- polys$clean_name_lower[idx]
      for (i in seq_len(nrow(pop_data))) {
        csv_clean <- pop_data$clean_name_lower[i]
        if (grepl(city_clean, csv_clean, ignore.case = TRUE) ||
            grepl(csv_clean, city_clean, ignore.case = TRUE)) {
          pop_val <- pop_data$Population_mid_2022[i]
          if (!is.na(pop_val)) {
            polys$population[idx] <- as.numeric(pop_val)
            break
          }
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

#' Calculate canal metrics for administrative areas
#'
#' Measures canal length, area coverage, density, and population metrics for each administrative polygon.
#' Uses UTM projections for accurate length calculations. Uses pre-computed area_km2 from city_polys.
#'
#' @param city_polys sf object with administrative polygons from get_city_admin_polygons()
#' @param canals_all sf object with canals from fetch_uk_canals()
#' @param use_parallel Logical, whether to use parallel processing for calculations
#' @param workers Number of parallel workers to use (if use_parallel = TRUE)
#' @return tibble with canal and population metrics per administrative area: city, admin_type, population, length_km, length_mi, area_km2, density_km_per_km2, population_density_per_km2
#' @export
calculate_canal_metrics <- function(city_polys, canals_all, use_parallel = FALSE, workers = NULL) {
  if (use_parallel) {
    if (is.null(workers)) workers <- max(2, parallel::detectCores() - 2)
    future::plan(future::multisession, workers = workers)
  }

  # Ensure both inputs have proper CRS (WGS84 for OSM data)
  if (is.na(sf::st_crs(canals_all))) {
    canals_all <- sf::st_set_crs(canals_all, 4326)
  }
  if (is.na(sf::st_crs(city_polys))) {
    city_polys <- sf::st_set_crs(city_polys, 4326)
  }

  # Transform canals once to match city_polys CRS
  canals_transformed <- sf::st_transform(canals_all, sf::st_crs(city_polys))

  canal_length_for_poly <- function(geom, city_crs = 4326) {
    # Convert single geometry to sf object with proper CRS
    poly <- sf::st_sf(geometry = sf::st_sfc(geom, crs = city_crs))

    bb <- sf::st_bbox(poly)
    bb_sfc <- sf::st_as_sfc(bb)
    sf::st_crs(bb_sfc) <- sf::st_crs(poly)
    # Use pre-transformed canals
    canals_sub <- canals_transformed[sf::st_intersects(canals_transformed, bb_sfc, sparse = FALSE)[,1], ]
    if (nrow(canals_sub) == 0) return(units::set_units(0, "m"))
    clip <- suppressWarnings(sf::st_intersection(sf::st_geometry(canals_sub), sf::st_geometry(poly)))
    if (length(clip) == 0) return(units::set_units(0, "m"))
    clip <- sf::st_as_sf(tibble::tibble(geometry = clip)) |>
      dplyr::filter(!sf::st_is_empty(geometry))
    epsg <- utm_epsg_for(poly)
    sum(sf::st_length(sf::st_transform(clip, epsg)), na.rm = TRUE)
  }

  # Get the CRS of city_polys to pass to helper functions
  city_crs_code <- sf::st_crs(city_polys)$epsg
  if (is.na(city_crs_code)) city_crs_code <- 4326

  if (use_parallel) {
    city_polys |>
      dplyr::mutate(
        length_m   = furrr::future_map(geometry, canal_length_for_poly, city_crs_code, .progress = TRUE),
        length_km  = purrr::map_dbl(length_m, ~ units::drop_units(units::set_units(.x, "km"))),
        length_mi  = purrr::map_dbl(length_m, ~ units::drop_units(units::set_units(.x, "mi"))),
        density_km_per_km2 = dplyr::if_else(area_km2 > 0, length_km / area_km2, NA_real_),
        population_density_per_km2 = dplyr::if_else(area_km2 > 0 & !is.na(population), population / area_km2, NA_real_)
      ) |>
      sf::st_drop_geometry() |>
      dplyr::arrange(dplyr::desc(length_km))
  } else {
    city_polys |>
      dplyr::mutate(
        length_m   = purrr::map(geometry, canal_length_for_poly, city_crs_code, .progress = TRUE),
        length_km  = purrr::map_dbl(length_m, ~ units::drop_units(units::set_units(.x, "km"))),
        length_mi  = purrr::map_dbl(length_m, ~ units::drop_units(units::set_units(.x, "mi"))),
        density_km_per_km2 = dplyr::if_else(area_km2 > 0, length_km / area_km2, NA_real_),
        population_density_per_km2 = dplyr::if_else(area_km2 > 0 & !is.na(population), population / area_km2, NA_real_)
      ) |>
      sf::st_drop_geometry() |>
      dplyr::arrange(dplyr::desc(length_km))
  }
}

#' Rank UK cities by canal length
#'
#' Downloads UK canals and administrative boundaries from OpenStreetMap (via Overpass),
#' retrieves metropolitan districts, unitary authorities, and Greater London, then calculates
#' canal length, area, density, and population metrics for each administrative area.
#'
#' @param overpass_url Optional Overpass endpoint (character). If NULL, uses default.
#' @param use_parallel Logical, whether to use parallel processing for calculations
#' @param use_cache Logical, whether to use disk caching for OSM queries
#' @param workers Number of parallel workers to use (if use_parallel = TRUE)
#' @return A tibble with columns: city, admin_type, population, length_km, length_mi, area_km2, density_km_per_km2, population_density_per_km2.
#' @export
#' @examplesIf interactive()
#' \dontrun{
#'   res <- rank_uk_cities_by_canals()  # Gets all UK administrative areas
#'   head(res)
#' }
rank_uk_cities_by_canals <- function(overpass_url = NULL, use_parallel = FALSE, use_cache = TRUE, workers = NULL) {
  uk_bb_vec <- setup_uk_bbox(overpass_url)
  canals_all <- fetch_uk_canals(uk_bb_vec, use_cache)
  cities_pts <- fetch_uk_cities(uk_bb_vec, use_cache)
  city_polys <- get_city_admin_polygons(cities_pts)
  calculate_canal_metrics(city_polys, canals_all, use_parallel, workers)

}

#' Plot canals over basemap for a city (FAST version using existing data)
#'
#' Uses pre-computed city polygons and canal data instead of refetching from OSM.
#' Much faster than the original version.
#'
#' @param city_name Name of the city to plot.
#' @param city_polys sf object with city polygons from get_city_admin_polygons()
#' @param canals_all sf object with all canal data from fetch_uk_canals()
#' @param provider Tile provider passed to ggspatial::annotation_map_tile().
#' @param zoomin Integer zoom tweak for tiles.
#' @param line_size Thickness of canal lines
#' @return A ggplot object.
#' @export
plot_city_canals_map <- function(city_name, city_polys, canals_all, provider = "osm", zoomin = 0, line_size = 0.8) {

  # Find the city polygon
  city_match <- city_polys[stringr::str_detect(tolower(city_polys$city), stringr::fixed(tolower(city_name))), ]

  if (nrow(city_match) == 0) {
    stop("City '", city_name, "' not found in city_polys. Available cities: ",
         paste(head(city_polys$city, 10), collapse = ", "), "...")
  }

  # If multiple matches, take the first one
  if (nrow(city_match) > 1) {
    message("Multiple matches found for '", city_name, "'. Using: ", city_match$city[1])
    city_match <- city_match[1, ]
  }

  poly <- city_match
  city_actual_name <- poly$city

  # Get canal data within this polygon's bounding box for efficiency
  bb <- sf::st_bbox(poly)
  bb_sfc <- sf::st_as_sfc(bb)
  sf::st_crs(bb_sfc) <- sf::st_crs(poly)

  # Transform canals to match polygon CRS for spatial operations
  canals_transformed <- sf::st_transform(canals_all, sf::st_crs(poly))
  canals_sub <- canals_transformed[sf::st_intersects(canals_transformed, bb_sfc, sparse = FALSE)[,1], ]

  if (nrow(canals_sub) == 0) {
    message("No canals found within ", city_actual_name, " boundaries")
    canals_clip <- sf::st_sf(geometry = sf::st_sfc(crs = sf::st_crs(poly)))
  } else {
    # Clip canals to the exact city polygon
    canals_clip <- suppressWarnings(sf::st_intersection(sf::st_geometry(canals_sub), sf::st_geometry(poly)))
    canals_clip <- sf::st_as_sf(tibble::tibble(geometry = canals_clip)) |>
      dplyr::filter(!sf::st_is_empty(geometry))
  }

  # Transform to Web Mercator for mapping
  p3857 <- sf::st_transform(poly, 3857)
  c3857 <- sf::st_transform(canals_clip, 3857)

  # Create the plot
  p <- ggplot2::ggplot() +
    ggspatial::annotation_map_tile(type = provider, zoomin = zoomin) +
    ggplot2::geom_sf(data = p3857, fill = NA, color = "red", linewidth = 0.8, alpha = 0.7) +
    ggplot2::coord_sf(
      crs  = 3857,
      xlim = sf::st_bbox(p3857)[c("xmin","xmax")],
      ylim = sf::st_bbox(p3857)[c("ymin","ymax")],
      expand = FALSE
    ) +
    ggplot2::labs(
      title = paste0(city_actual_name, " — Canals"),
      subtitle = paste0("Admin type: ", poly$admin_type, " | Population: ",
                       ifelse(is.na(poly$population), "N/A", format(poly$population, big.mark = ",")))
    ) +
    ggplot2::theme_minimal()

  # Add canals if any exist
  if (nrow(c3857) > 0) {
    p <- p + ggplot2::geom_sf(data = c3857, color = "blue", linewidth = line_size)
  }

  return(p)
}

#' Plot all administrative polygons on UK map
#'
#' Creates a map showing all UK administrative boundaries (metropolitan districts, unitary authorities, Greater London)
#' with area labels on a proper basemap, colored by area size.
#'
#' @param city_polys sf object with administrative polygons from get_city_admin_polygons()
#' @param show_labels Logical, whether to show administrative area names and areas as labels
#' @param label_size Numeric, size of text labels
#' @param use_basemap Logical, whether to use tile basemap (requires ggspatial)
#' @param tile_provider Tile provider for basemap ("osm", "cartodb", etc.)
#' @return A ggplot object showing all administrative polygons
#' @export
plot_all_city_polygons <- function(city_polys, show_labels = TRUE, label_size = 2.5,
                                   use_basemap = TRUE, tile_provider = "osm") {
  if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("sf", quietly = TRUE)) {
    stop("ggplot2 and sf packages are required for plotting")
  }

  if (use_basemap && !requireNamespace("ggspatial", quietly = TRUE)) {
    warning("ggspatial package required for basemap. Falling back to plain map.")
    use_basemap <- FALSE
  }

  # Transform to Web Mercator for basemap compatibility, or BNG for plain map
  target_crs <- if (use_basemap) 3857 else 27700  # Web Mercator vs British National Grid
  city_polys_proj <- sf::st_transform(city_polys, target_crs)

  # Calculate centroids for labels
  if (show_labels) {
    centroids <- sf::st_centroid(city_polys_proj)
    centroids$label <- paste0(city_polys$city, "\n(", round(city_polys$area_km2, 1), " km²)")
  }

  # Create the plot
  p <- ggplot2::ggplot()

  # Add basemap if requested
  if (use_basemap) {
    p <- p + ggspatial::annotation_map_tile(type = tile_provider, zoomin = -1)
  }

  # Add city polygons
  p <- p + ggplot2::geom_sf(data = city_polys_proj,
                           ggplot2::aes(fill = area_km2),
                           alpha = if (use_basemap) 0.7 else 0.8,
                           color = "white",
                           size = 0.4) +
    ggplot2::scale_fill_viridis_c(name = "Area (km²)",
                                  trans = "log10",
                                  labels = scales::comma_format()) +
    ggplot2::coord_sf(crs = target_crs) +
    ggplot2::labs(title = "UK Administrative Boundaries",
                  subtitle = paste0("Showing ", nrow(city_polys), " metropolitan districts, unitary authorities, and Greater London")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank())

  # Add labels if requested
  if (show_labels && nrow(city_polys) <= 50) {  # Only show labels if not too crowded
    p <- p + ggplot2::geom_sf_text(data = centroids,
                                   ggplot2::aes(label = label),
                                   size = label_size,
                                   color = "black",
                                   fontface = "bold",
                                   check_overlap = TRUE)
  } else if (show_labels) {
    message("Too many administrative areas (", nrow(city_polys), ") to show labels clearly. Set show_labels = FALSE or filter to fewer areas.")
  }

  return(p)
}

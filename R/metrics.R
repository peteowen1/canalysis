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
    clip <- suppress_sf_attr_warnings(sf::st_intersection(sf::st_geometry(canals_sub), sf::st_geometry(poly)))
    if (length(clip) == 0) return(units::set_units(0, "m"))
    clip <- sf::st_as_sf(tibble::tibble(geometry = clip)) |>
      dplyr::filter(!sf::st_is_empty(geometry))
    epsg <- utm_epsg_for(poly)
    sum(sf::st_length(sf::st_transform(clip, epsg)), na.rm = TRUE)
  }

  # Get the CRS of city_polys to pass to helper functions
  city_crs_code <- sf::st_crs(city_polys)$epsg
  if (is.na(city_crs_code)) city_crs_code <- 4326

  map_fn <- if (use_parallel) furrr::future_map else purrr::map

  city_polys |>
    dplyr::mutate(
      length_m   = map_fn(geometry, canal_length_for_poly, city_crs_code, .progress = TRUE),
      length_km  = purrr::map_dbl(length_m, ~ units::drop_units(units::set_units(.x, "km"))),
      length_mi  = purrr::map_dbl(length_m, ~ units::drop_units(units::set_units(.x, "mi"))),
      density_km_per_km2 = dplyr::if_else(area_km2 > 0, length_km / area_km2, NA_real_),
      population_density_per_km2 = dplyr::if_else(area_km2 > 0 & !is.na(population), population / area_km2, NA_real_)
    ) |>
    sf::st_drop_geometry() |>
    dplyr::arrange(dplyr::desc(length_km))
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

.metres_per_mile <- 1609.344

#' Calculate canal metrics for administrative areas
#'
#' Measures canal length, area coverage, density, and population metrics for each administrative polygon.
#' Uses UTM projections for accurate length calculations. Uses pre-computed area_km2 from city_polys.
#'
#' @param city_polys sf object with administrative polygons from get_city_admin_polygons()
#' @param canals_all sf object with canals from fetch_uk_canals()
#' @return tibble with canal and population metrics per administrative area: city, admin_type, population, length_km, length_mi, area_km2, density_km_per_km2, population_density_per_km2
#' @export
calculate_canal_metrics <- function(city_polys, canals_all) {
  # Ensure both inputs have proper CRS (WGS84 for OSM data)
  if (is.na(sf::st_crs(canals_all))) {
    canals_all <- sf::st_set_crs(canals_all, 4326)
  }
  if (is.na(sf::st_crs(city_polys))) {
    city_polys <- sf::st_set_crs(city_polys, 4326)
  }

  # Strip canal attributes — only geometry matters for length calculation.
  # This dramatically reduces st_transform and st_intersection cost.
  canals_geom <- sf::st_geometry(canals_all)

  # Pre-filter canals to those within the bounding box of all cities
  cities_bbox <- sf::st_as_sfc(sf::st_bbox(city_polys))
  sf::st_crs(cities_bbox) <- sf::st_crs(city_polys)
  in_bbox <- sf::st_intersects(canals_geom, cities_bbox, sparse = FALSE)[, 1]
  canals_geom <- canals_geom[in_bbox]

  # Transform to UTM 30N (covers all UK) for accurate length measurement
  city_utm <- sf::st_transform(city_polys, 32630)
  canals_utm <- sf::st_transform(sf::st_sfc(canals_geom, crs = sf::st_crs(canals_all)), 32630)

  # Vectorized intersection: clip canals to city polygons
  # city_utm (sf) must be x so its attributes (city name) propagate to result;
  # canals_utm (sfc) as y provides the clipping geometry
  clipped <- suppress_sf_attr_warnings(
    sf::st_intersection(city_utm, canals_utm)
  )

  # Remove empty geometries
  clipped <- clipped[!sf::st_is_empty(clipped), ]

  if (nrow(clipped) == 0) {
    return(
      city_polys |>
        sf::st_drop_geometry() |>
        dplyr::mutate(
          length_km = 0, length_mi = 0,
          density_km_per_km2 = NA_real_,
          population_density_per_km2 = NA_real_
        ) |>
        dplyr::arrange(dplyr::desc(length_km))
    )
  }

  # Compute lengths on already-projected UTM geometries
  clipped$length_m <- sf::st_length(clipped)

  # Aggregate total canal length per city
  canal_lengths <- clipped |>
    sf::st_drop_geometry() |>
    dplyr::group_by(city) |>
    dplyr::summarise(
      total_length_m = sum(as.numeric(length_m), na.rm = TRUE),
      .groups = "drop"
    )

  # Join back to city data and compute derived metrics
  city_polys |>
    sf::st_drop_geometry() |>
    dplyr::left_join(canal_lengths, by = "city") |>
    dplyr::mutate(
      total_length_m = dplyr::coalesce(total_length_m, 0),
      length_km = total_length_m / 1000,
      length_mi = total_length_m / .metres_per_mile,
      density_km_per_km2 = dplyr::if_else(area_km2 > 0, length_km / area_km2, NA_real_),
      population_density_per_km2 = dplyr::if_else(area_km2 > 0 & !is.na(population), population / area_km2, NA_real_)
    ) |>
    dplyr::select(-total_length_m) |>
    dplyr::arrange(dplyr::desc(length_km))
}

#' Rank UK cities by canal length
#'
#' Downloads UK canals and administrative boundaries from OpenStreetMap (via Overpass),
#' retrieves metropolitan districts, unitary authorities, and Greater London, then calculates
#' canal length, area, density, and population metrics for each administrative area.
#'
#' @param overpass_url Optional Overpass endpoint (character). If NULL, uses default.
#' @param use_cache Logical, whether to use disk caching for OSM queries
#' @return A tibble with columns: city, admin_type, population, length_km, length_mi, area_km2, density_km_per_km2, population_density_per_km2.
#' @export
#' @examplesIf interactive()
#' \dontrun{
#'   res <- rank_uk_cities_by_canals()  # Gets all UK administrative areas
#'   head(res)
#' }
rank_uk_cities_by_canals <- function(overpass_url = NULL, use_cache = TRUE) {
  uk_bb_vec <- setup_uk_bbox(overpass_url)
  canals_all <- fetch_uk_canals(uk_bb_vec, use_cache)
  cities_pts <- fetch_uk_cities(uk_bb_vec, use_cache)
  city_polys <- get_city_admin_polygons(cities_pts)
  calculate_canal_metrics(city_polys, canals_all)
}

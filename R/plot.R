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

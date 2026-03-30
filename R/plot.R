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

  # Try exact match first (case-insensitive), fall back to substring
  city_match <- city_polys[tolower(city_polys$city) == tolower(city_name), ]

  if (nrow(city_match) == 0) {
    # Substring fallback for convenience (e.g., "Manchester" matching "Greater Manchester")
    city_match <- city_polys[stringr::str_detect(tolower(city_polys$city), stringr::fixed(tolower(city_name))), ]
  }

  if (nrow(city_match) == 0) {
    stop("City '", city_name, "' not found in city_polys. Available cities: ",
         paste(head(city_polys$city, 10), collapse = ", "), "...")
  }

  if (nrow(city_match) > 1) {
    message("Multiple matches for '", city_name, "': ",
            paste(city_match$city, collapse = ", "), ". Using: ", city_match$city[1])
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
    canals_clip <- suppress_sf_attr_warnings(sf::st_intersection(sf::st_geometry(canals_sub), sf::st_geometry(poly)))
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
      title = paste0(city_actual_name, " \u2014 Canals"),
      subtitle = paste0("Admin type: ", poly$admin_type, " | Population: ",
                       ifelse(is.na(poly$population), "N/A", format(poly$population, big.mark = ",")))
    ) +
    ggplot2::theme_minimal()

  # Add canals if any exist
  if (nrow(c3857) > 0) {
    p <- p + ggplot2::geom_sf(data = c3857, color = "blue", linewidth = line_size)
  }

  p
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
  # Transform to Web Mercator for basemap compatibility, or BNG for plain map
  target_crs <- if (use_basemap) 3857 else 27700  # Web Mercator vs British National Grid
  city_polys_proj <- sf::st_transform(city_polys, target_crs)

  # Calculate centroids for labels
  if (show_labels) {
    centroids <- sf::st_centroid(city_polys_proj)
    centroids$label <- paste0(city_polys$city, "\n(", round(city_polys$area_km2, 1), " km\u00b2)")
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
    ggplot2::scale_fill_viridis_c(name = "Area (km\u00b2)",
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

  p
}

#' Interactive leaflet map of canals in a city
#'
#' Creates a zoomable, pannable HTML map showing canals over a city boundary.
#' Canal segments display name and length on click. Requires the leaflet package.
#'
#' @param city_name Name of the city to map (substring matching supported).
#' @param city_polys sf object with administrative polygons from get_city_admin_polygons()
#' @param canals_all sf object with canal data from fetch_uk_canals()
#' @param canal_color Color for canal lines (default "#2166AC")
#' @param canal_weight Line thickness for canals (default 3)
#' @return A leaflet htmlwidget object
#' @export
leaflet_city_canals <- function(city_name, city_polys, canals_all,
                                canal_color = "#2166AC", canal_weight = 3) {
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("The 'leaflet' package is required. Install it with: install.packages('leaflet')")
  }

  # Match city using same logic as plot_city_canals_map
  city_match <- city_polys[tolower(city_polys$city) == tolower(city_name), ]
  if (nrow(city_match) == 0) {
    city_match <- city_polys[stringr::str_detect(tolower(city_polys$city), stringr::fixed(tolower(city_name))), ]
  }
  if (nrow(city_match) == 0) {
    stop("City '", city_name, "' not found in city_polys. Available cities: ",
         paste(head(city_polys$city, 10), collapse = ", "), "...")
  }
  if (nrow(city_match) > 1) {
    message("Multiple matches for '", city_name, "': ",
            paste(city_match$city, collapse = ", "), ". Using: ", city_match$city[1])
    city_match <- city_match[1, ]
  }

  poly <- city_match

  # Clip canals to city boundary
  canals_transformed <- sf::st_transform(canals_all, sf::st_crs(poly))
  canals_clip <- suppress_sf_attr_warnings(
    sf::st_intersection(canals_transformed, sf::st_geometry(poly))
  )
  canals_clip <- canals_clip[!sf::st_is_empty(canals_clip), ]

  # Compute segment lengths in metres for popups
  canals_utm <- sf::st_transform(canals_clip, 32630)
  canals_clip$length_m <- as.numeric(sf::st_length(canals_utm))

  # Ensure WGS84 for leaflet
  canals_wgs <- sf::st_transform(canals_clip, 4326)
  poly_wgs <- sf::st_transform(poly, 4326)

  # Build popup content for each canal segment
  canals_wgs$popup <- build_canal_popup(canals_wgs)

  # Build the map
  leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
    leaflet::addPolygons(
      data = poly_wgs, fill = FALSE,
      color = "red", weight = 2, opacity = 0.8
    ) |>
    leaflet::addPolylines(
      data = canals_wgs,
      color = canal_color, weight = canal_weight, opacity = 0.8,
      popup = ~popup,
      highlightOptions = leaflet::highlightOptions(
        weight = canal_weight + 2, color = "#FF4500", bringToFront = TRUE
      )
    )
}

# Build HTML popup string for each canal segment.
# Receives an sf data frame with columns: name (character, may be NA)
# and length_m (numeric, segment length in metres).
# Should return a character vector of HTML strings, one per row.
build_canal_popup <- function(canals) {
  canal_name <- ifelse(is.na(canals$name), "Unnamed canal", canals$name)
  length_km <- round(canals$length_m / 1000, 2)
  length_mi <- round(canals$length_m / 1609.344, 2)
  paste0(
    "<b>", canal_name, "</b><br>",
    length_km, " km (", length_mi, " mi)"
  )
}

#' Create bounding box around point with specified radius
#'
#' @param pt_sf sf point object
#' @param radius_km radius in kilometers
#' @return numeric vector with bounding box coordinates
bbox_from_point_km <- function(pt_sf, radius_km = 20) {
  stopifnot(sf::st_geometry_type(pt_sf, by_geometry = TRUE)[1] == "POINT")
  buf <- sf::st_transform(pt_sf, 3857) |> sf::st_buffer(radius_km * 1000) |> sf::st_transform(4326)
  bb  <- sf::st_bbox(buf)
  as.numeric(bb[c("xmin","ymin","xmax","ymax")])
}

#' Get administrative polygon for a city
#'
#' @param city_row single row sf object with city point
#' @param levels admin levels to query
#' @param radii_km radii to try in km
#' @param timeout timeout in seconds
#' @return sf object with city administrative polygon or NULL
get_admin_for_city <- function(city_row, levels = c("6","7","8"),
                               radii_km = c(15, 30), timeout = 60) {
  nm <- city_row$name
  
  # Special handling for London - use different admin levels and larger radius
  if (tolower(nm) == "london") {
    levels <- c("4","6","7","8")  # Include region level for London
    radii_km <- c(25, 40, 60)    # Much larger search radius for London
    timeout <- 120               # Longer timeout for complex London queries
    message("Using special London parameters: admin levels 4,6,7,8 and radii 25,40,60 km")
  }
  
  # Load ONS metropolitan district reference data
  ons_metro_areas <- tryCatch({
    csv_path <- file.path(dirname(getwd()), "data", "uk_metropolitan_districts.csv")
    if (!file.exists(csv_path)) {
      csv_path <- file.path(getwd(), "data", "uk_metropolitan_districts.csv")
    }
    if (file.exists(csv_path)) {
      read.csv(csv_path, stringsAsFactors = FALSE)
    } else {
      NULL
    }
  }, error = function(e) NULL)
  
  # Get expected area for this city from ONS data
  expected_area <- NULL
  if (!is.null(ons_metro_areas)) {
    city_match <- which(tolower(ons_metro_areas$Name) == tolower(nm))
    if (length(city_match) > 0) {
      expected_area <- ons_metro_areas$Area_sq_km[city_match[1]]
      message("Expected area for ", nm, ": ", expected_area, " km²")
    }
  }
  
  # Major metropolitan areas that should prefer larger administrative areas
  major_metros <- c("london", "manchester", "birmingham", "glasgow", "edinburgh", 
                   "leeds", "liverpool", "bristol", "sheffield", "cardiff",
                   "newcastle", "belfast")
  
  # Cities that often get county-level polygons when they should get city-level
  prefer_smaller <- c("chester", "nottingham", "winchester", "canterbury", "bath")
  
  is_major_metro <- tolower(nm) %in% major_metros
  is_prefer_smaller <- tolower(nm) %in% prefer_smaller
  
  for (r in radii_km) {
    bb  <- bbox_from_point_km(city_row, r)
    q   <- osmdata::opq(bbox = bb, timeout = timeout) |>
      osmdata::add_osm_feature("boundary", "administrative") |>
      osmdata::add_osm_feature("admin_level", levels, value_exact = FALSE)
    dat <- tryCatch(osmdata::osmdata_sf(q), error = function(e) NULL)
    polys <- sf_bind_rows(list(dat$osm_multipolygons, dat$osm_polygons))
    if (!is.null(polys) && nrow(polys) > 0) {
      polys <- polys |>
        dplyr::mutate(name = ifelse(is.na(name), "", name)) |>
        dplyr::filter(!sf::st_is_empty(geometry)) |>
        sf::st_make_valid()

      # Ensure WGS84 CRS is set (OSM data should be in WGS84)
      if (is.na(sf::st_crs(polys))) {
        polys <- sf::st_set_crs(polys, 4326)
      }

      polys$area_pick <- suppressWarnings(sf::st_area(sf::st_transform(polys, 3857)))
      
      # Filter by area based on city type
      if (is_major_metro) {
        # For major metros, filter out very small areas (< 5 km²)
        polys <- polys[units::drop_units(units::set_units(polys$area_pick, "km^2")) >= 5, ]
        if (nrow(polys) == 0) next
      } else if (is_prefer_smaller) {
        # For cities that often get counties, filter out very large areas (> 500 km²)
        polys <- polys[units::drop_units(units::set_units(polys$area_pick, "km^2")) <= 500, ]
        if (nrow(polys) == 0) next
      }
      
      exact <- which(tolower(polys$name) == tolower(nm))
      if (length(exact) >= 1) {
        if (!is.null(expected_area) && length(exact) > 1) {
          # Find polygon closest to expected ONS area
          areas_km2 <- units::drop_units(units::set_units(polys$area_pick[exact], "km^2"))
          area_diffs <- abs(areas_km2 - expected_area)
          j <- exact[which.min(area_diffs)]
          message("Selected polygon with area ", round(areas_km2[which.min(area_diffs)]), " km² (expected ", expected_area, " km²)")
        } else if (is_major_metro && length(exact) > 1) {
          # For major metros with exact matches, prefer larger areas
          j <- exact[which.max(polys$area_pick[exact])]
        } else {
          j <- exact[1]
        }
        result <- polys[j, c("name","geometry")]
        if (is.na(sf::st_crs(result))) result <- sf::st_set_crs(result, 4326)
        return(result)  # Early exit - found exact match
      }

      has <- which(stringr::str_detect(tolower(polys$name), stringr::fixed(tolower(nm))))
      if (length(has) >= 1) {
        if (!is.null(expected_area) && length(has) > 1) {
          # Find polygon closest to expected ONS area
          areas_km2 <- units::drop_units(units::set_units(polys$area_pick[has], "km^2"))
          area_diffs <- abs(areas_km2 - expected_area)
          j <- has[which.min(area_diffs)]
          message("Selected polygon with area ", round(areas_km2[which.min(area_diffs)]), " km² (expected ", expected_area, " km²)")
        } else if (is_major_metro) {
          # For major metros with partial matches, prefer larger areas
          j <- has[which.max(polys$area_pick[has])]
        } else if (is_prefer_smaller) {
          j <- has[which.min(polys$area_pick[has])]
        } else {
          j <- has[which.min(polys$area_pick[has])]
        }
        result <- polys[j, c("name","geometry")]
        if (is.na(sf::st_crs(result))) result <- sf::st_set_crs(result, 4326)
        return(result)  # Early exit - found partial match
      }

      # Fallback: take smallest area if no name matches
      j <- which.min(polys$area_pick)
      result <- polys[j, c("name","geometry")]
      if (is.na(sf::st_crs(result))) result <- sf::st_set_crs(result, 4326)
      return(result)  # Early exit - found fallback match
    }
  }
  NULL
}

# Create cached version of get_admin_for_city
cache_dir <- file.path(tempdir(), "canalysis_cache")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

#' Get administrative polygon for a city (cached version)
#'
#' Cached version of get_admin_for_city that uses disk caching for performance.
#'
#' @param city_row single row sf object with city point
#' @param ... additional arguments passed to get_admin_for_city
#' @return sf object with city administrative polygon or NULL
#' @export
get_admin_for_city_cached <- memoise::memoise(get_admin_for_city, cache = cachem::cache_disk(cache_dir))
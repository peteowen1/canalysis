# Offline tests for calculate_canal_segment_metrics()
# Uses synthetic city polygons and canal lines — no network access needed.

# Helper: build a city polygon sf
make_city_poly <- function(city_name, x0 = -2, y0 = 51, dx = 0.1, dy = 0.1) {
  poly <- sf::st_polygon(list(matrix(c(
    x0, y0, x0, y0 + dy, x0 + dx, y0 + dy, x0 + dx, y0, x0, y0
  ), ncol = 2, byrow = TRUE)))
  sf::st_sf(
    city = city_name,
    admin_type = "Unitary Authority",
    population = 100000,
    area_km2 = 50,
    geometry = sf::st_sfc(poly, crs = 4326)
  )
}

# Helper: build canal linestrings
make_canals <- function(coords_list) {
  lines <- lapply(coords_list, function(m) sf::st_linestring(m))
  sf::st_sf(
    name = rep(NA_character_, length(lines)),
    geometry = sf::st_sfc(lines, crs = 4326)
  )
}

test_that("segment metrics returns correct counts for two touching segments", {
  city <- make_city_poly("TestCity")

  # Two canal segments that share an endpoint (connected)
  canals <- make_canals(list(
    matrix(c(-1.98, 51.02, -1.95, 51.05), ncol = 2, byrow = TRUE),
    matrix(c(-1.95, 51.05, -1.92, 51.08), ncol = 2, byrow = TRUE)
  ))

  result <- calculate_canal_segment_metrics(city, canals)

  expect_equal(nrow(result), 1)
  expect_equal(result$city, "TestCity")
  expect_equal(result$n_segments, 2L)
  expect_equal(result$n_components, 1L)  # one connected network
  expect_gt(result$mean_length_km, 0)
  expect_gt(result$max_length_km, 0)
})

test_that("segment metrics detects disconnected components", {
  city <- make_city_poly("TestCity")

  # Two segments that do NOT share endpoints
  canals <- make_canals(list(
    matrix(c(-1.99, 51.01, -1.97, 51.03), ncol = 2, byrow = TRUE),
    matrix(c(-1.94, 51.06, -1.92, 51.08), ncol = 2, byrow = TRUE)
  ))

  result <- calculate_canal_segment_metrics(city, canals)

  expect_equal(result$n_segments, 2L)
  expect_equal(result$n_components, 2L)  # two separate networks
})

test_that("segment metrics returns zeros for city with no canals", {
  city <- make_city_poly("EmptyCity", x0 = 5, y0 = 55)  # far from canals
  canals <- make_canals(list(
    matrix(c(-2, 51, -1.9, 51.05), ncol = 2, byrow = TRUE)
  ))

  result <- calculate_canal_segment_metrics(city, canals)

  expect_equal(result$n_segments, 0L)
  expect_equal(result$n_components, 0L)
  expect_true(is.na(result$mean_length_km))
})

test_that("segment metrics handles single segment", {
  city <- make_city_poly("OneCanal")
  canals <- make_canals(list(
    matrix(c(-1.98, 51.02, -1.92, 51.08), ncol = 2, byrow = TRUE)
  ))

  result <- calculate_canal_segment_metrics(city, canals)

  expect_equal(result$n_segments, 1L)
  expect_equal(result$n_components, 1L)
  expect_equal(result$mean_length_km, result$max_length_km)
  expect_equal(result$mean_length_km, result$median_length_km)
})

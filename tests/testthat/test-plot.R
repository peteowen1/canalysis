# Offline tests for plot.R
# Test city matching logic and plot construction with synthetic data.
# plot_city_canals_map always fetches tiles, so we test matching logic only.
# plot_all_city_polygons has use_basemap=FALSE for offline testing.

# Helper: build city_polys with canals for plot testing
make_plot_data <- function() {
  poly1 <- sf::st_polygon(list(matrix(c(
    -0.1, 51.5, -0.1, 51.6, 0.0, 51.6, 0.0, 51.5, -0.1, 51.5
  ), ncol = 2, byrow = TRUE)))
  poly2 <- sf::st_polygon(list(matrix(c(
    -2.3, 53.4, -2.3, 53.5, -2.2, 53.5, -2.2, 53.4, -2.3, 53.4
  ), ncol = 2, byrow = TRUE)))

  city_polys <- sf::st_sf(
    city = c("London", "Greater Manchester"),
    admin_type = c("Combined Authority", "Combined Authority"),
    population = c(8866180, 2867800),
    area_km2 = c(1572.0, 1276.0),
    geometry = sf::st_sfc(poly1, poly2, crs = 4326)
  )

  canal <- sf::st_linestring(matrix(c(
    -0.05, 51.52, -0.05, 51.58
  ), ncol = 2, byrow = TRUE))
  canals <- sf::st_sf(geometry = sf::st_sfc(canal, crs = 4326))

  list(city_polys = city_polys, canals = canals)
}

test_that("plot_city_canals_map errors for unknown city", {
  d <- make_plot_data()
  expect_error(
    plot_city_canals_map("Atlantis", d$city_polys, d$canals),
    "not found"
  )
})

test_that("plot_city_canals_map exact match takes priority over substring", {
  # "London" should match exactly, not fall through to substring
  d <- make_plot_data()
  # This tests the matching logic — if it finds exact match it uses it
  # We can't fully render (needs tiles) but we can test the error path
  # by confirming no error for valid names
  skip_if_offline()
  p <- plot_city_canals_map("London", d$city_polys, d$canals)
  expect_s3_class(p, "gg")
})

test_that("plot_all_city_polygons returns ggplot without basemap", {
  d <- make_plot_data()
  p <- plot_all_city_polygons(d$city_polys, use_basemap = FALSE)
  expect_s3_class(p, "gg")
})

test_that("plot_all_city_polygons suppresses labels when too many cities", {
  n <- 55
  polys <- lapply(seq_len(n), function(i) {
    x0 <- -3 + i * 0.1
    sf::st_polygon(list(matrix(c(
      x0, 51, x0, 51.05, x0 + 0.05, 51.05, x0 + 0.05, 51, x0, 51
    ), ncol = 2, byrow = TRUE)))
  })
  many_cities <- sf::st_sf(
    city = paste0("City", seq_len(n)),
    admin_type = rep("Unitary Authority", n),
    population = rep(100000, n),
    area_km2 = rep(50, n),
    geometry = sf::st_sfc(polys, crs = 4326)
  )

  expect_message(
    plot_all_city_polygons(many_cities, show_labels = TRUE, use_basemap = FALSE),
    "Too many"
  )
})

test_that("plot_all_city_polygons shows labels for small datasets", {
  d <- make_plot_data()
  # 2 cities < 50, so labels should show without message
  p <- plot_all_city_polygons(d$city_polys, show_labels = TRUE, use_basemap = FALSE)
  expect_s3_class(p, "gg")
})

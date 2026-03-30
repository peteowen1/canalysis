test_that("calculate_canal_metrics returns correct structure with canals", {
  # Create two city polygons in UTM-friendly coordinates (near London, WGS84)
  poly1 <- sf::st_polygon(list(matrix(c(
    -0.1, 51.5,
    -0.1, 51.6,
     0.0, 51.6,
     0.0, 51.5,
    -0.1, 51.5
  ), ncol = 2, byrow = TRUE)))
  poly2 <- sf::st_polygon(list(matrix(c(
    -1.0, 52.0,
    -1.0, 52.1,
    -0.9, 52.1,
    -0.9, 52.0,
    -1.0, 52.0
  ), ncol = 2, byrow = TRUE)))

  city_polys <- sf::st_sf(
    city = c("CityA", "CityB"),
    admin_type = c("Unitary Authority", "Metropolitan District"),
    population = c(100000, 50000),
    area_km2 = c(80.0, 60.0),
    geometry = sf::st_sfc(poly1, poly2, crs = 4326)
  )

  # Create a canal line that crosses CityA
  canal_line <- sf::st_linestring(matrix(c(
    -0.05, 51.52,
    -0.05, 51.58
  ), ncol = 2, byrow = TRUE))

  canals <- sf::st_sf(geometry = sf::st_sfc(canal_line, crs = 4326))

  result <- calculate_canal_metrics(city_polys, canals)

  expect_s3_class(result, "data.frame")
  expected_cols <- c("city", "admin_type", "population", "area_km2",
                     "length_km", "length_mi", "density_km_per_km2",
                     "population_density_per_km2")
  expect_true(all(expected_cols %in% names(result)))
  expect_equal(nrow(result), 2)

  # CityA should have canal length > 0
  city_a <- result[result$city == "CityA", ]
  expect_gt(city_a$length_km, 0)

  # CityB has no canals nearby
  city_b <- result[result$city == "CityB", ]
  expect_equal(city_b$length_km, 0)

  # Results should be sorted by length_km descending
  expect_equal(result$city[1], "CityA")
})

test_that("calculate_canal_metrics handles no canal intersections", {
  poly <- sf::st_polygon(list(matrix(c(
    -0.1, 51.5,
    -0.1, 51.6,
     0.0, 51.6,
     0.0, 51.5,
    -0.1, 51.5
  ), ncol = 2, byrow = TRUE)))

  city_polys <- sf::st_sf(
    city = "CityA",
    admin_type = "Unitary Authority",
    population = 100000,
    area_km2 = 80.0,
    geometry = sf::st_sfc(poly, crs = 4326)
  )

  # Canal far away from the city
  canal_line <- sf::st_linestring(matrix(c(10.0, 40.0, 10.1, 40.1), ncol = 2, byrow = TRUE))
  canals <- sf::st_sf(geometry = sf::st_sfc(canal_line, crs = 4326))

  result <- calculate_canal_metrics(city_polys, canals)
  expect_equal(result$length_km, 0)
})

test_that("calculate_canal_metrics computes density correctly", {
  poly <- sf::st_polygon(list(matrix(c(
    -0.1, 51.5,
    -0.1, 51.6,
     0.0, 51.6,
     0.0, 51.5,
    -0.1, 51.5
  ), ncol = 2, byrow = TRUE)))

  city_polys <- sf::st_sf(
    city = "CityA",
    admin_type = "Unitary Authority",
    population = 200000,
    area_km2 = 50.0,
    geometry = sf::st_sfc(poly, crs = 4326)
  )

  canal_line <- sf::st_linestring(matrix(c(
    -0.05, 51.52,
    -0.05, 51.58
  ), ncol = 2, byrow = TRUE))
  canals <- sf::st_sf(geometry = sf::st_sfc(canal_line, crs = 4326))

  result <- calculate_canal_metrics(city_polys, canals)

  # density = length_km / area_km2
  expect_equal(result$density_km_per_km2, result$length_km / result$area_km2)
  # population_density = population / area_km2
  expect_equal(result$population_density_per_km2, 200000 / 50.0)
  # length_mi should be length_km converted
  expect_equal(result$length_mi, result$length_km * 1000 / 1609.344, tolerance = 1e-6)
})

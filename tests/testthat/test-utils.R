test_that("ensure_chr converts values to character", {
  expect_equal(ensure_chr(42), "42")
  expect_equal(ensure_chr("hello"), "hello")
  expect_equal(ensure_chr(TRUE), "TRUE")
})

test_that("ensure_chr returns NA_character_ for NULL", {
  expect_identical(ensure_chr(NULL), NA_character_)
})

test_that("ensure_chr_cols adds missing columns as NA", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
  result <- ensure_chr_cols(df, c("b", "c"))
  expect_true("c" %in% names(result))
  expect_true(all(is.na(result$c)))
  expect_type(result$b, "character")
})

test_that("ensure_chr_cols converts existing columns to character", {
  df <- data.frame(a = 1:3, b = 4:6)
  result <- ensure_chr_cols(df, c("b"))
  expect_type(result$b, "character")
  expect_equal(result$b, c("4", "5", "6"))
})

test_that("utm_epsg_for returns Northern Hemisphere EPSG for UK coordinates", {
  # Create a simple point in London (lon=-0.1, lat=51.5)
  pt <- sf::st_sfc(sf::st_point(c(-0.1, 51.5)), crs = 4326)
  poly <- sf::st_sf(geometry = pt)
  epsg <- utm_epsg_for(poly)
  # London is in UTM zone 30N -> EPSG 32630
  expect_equal(epsg, 32630)
})

test_that("utm_epsg_for returns Southern Hemisphere EPSG for southern coordinates", {
  # Create a point in Melbourne, Australia (lon=144.9, lat=-37.8)
  pt <- sf::st_sfc(sf::st_point(c(144.9, -37.8)), crs = 4326)
  poly <- sf::st_sf(geometry = pt)
  epsg <- utm_epsg_for(poly)
  # Melbourne is in UTM zone 55S -> EPSG 32755
  expect_equal(epsg, 32755)
})

test_that("sf_bind_rows returns NULL for empty list", {
  expect_null(sf_bind_rows(list()))
  expect_null(sf_bind_rows(list(NULL, NULL)))
})

test_that("sf_bind_rows returns single element unchanged", {
  pt <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  df <- sf::st_sf(name = "a", geometry = pt)
  result <- sf_bind_rows(list(df))
  expect_equal(nrow(result), 1)
  expect_equal(result$name, "a")
})

test_that("sf_bind_rows combines sf objects with different columns", {
  pt1 <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  pt2 <- sf::st_sfc(sf::st_point(c(1, 1)), crs = 4326)
  df1 <- sf::st_sf(name = "a", extra = "x", geometry = pt1)
  df2 <- sf::st_sf(name = "b", geometry = pt2)
  result <- sf_bind_rows(list(df1, df2))
  expect_equal(nrow(result), 2)
  expect_true("extra" %in% names(result))
})

test_that("suppress_sf_attr_warnings suppresses attribute warning", {
  expect_silent(suppress_sf_attr_warnings({
    warning("attribute variables are assumed to be spatially constant throughout all geometries")
  }))
})

test_that("suppress_sf_attr_warnings propagates other warnings", {
  expect_warning(
    suppress_sf_attr_warnings(warning("real problem")),
    "real problem"
  )
})

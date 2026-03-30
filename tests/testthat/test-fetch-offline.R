# Offline tests for fetch.R data wrangling logic
# These test population matching, CSV parsing, and admin boundary processing
# without hitting the network.

# Helper: build a minimal admin_polys sf object
make_admin_polys <- function(names, admin_types = NULL) {
  n <- length(names)
  if (is.null(admin_types)) admin_types <- rep("Unitary Authority", n)
  # Simple square polygons spread apart so they don't overlap
  polys <- lapply(seq_len(n), function(i) {
    x0 <- -2 + i * 0.2
    sf::st_polygon(list(matrix(c(
      x0, 51.0, x0, 51.1, x0 + 0.1, 51.1, x0 + 0.1, 51.0, x0, 51.0
    ), ncol = 2, byrow = TRUE)))
  })
  sf::st_sf(
    name = names,
    admin_type = admin_types,
    geometry = sf::st_sfc(polys, crs = 4326)
  )
}

test_that("get_city_admin_polygons matches exact city names to CSV", {
  admin <- make_admin_polys(c("Birmingham", "Leeds", "London"))
  result <- get_city_admin_polygons(admin)

  expect_true("population" %in% names(result))
  expect_true("area_km2" %in% names(result))

  # Birmingham and Leeds should have population from CSV
  bham <- result[result$city == "Birmingham", ]
  expect_false(is.na(bham$population))
  expect_gt(bham$population, 1000000)  # ~1.15M

  leeds <- result[result$city == "Leeds", ]
  expect_false(is.na(leeds$population))
  expect_gt(leeds$population, 500000)
})

test_that("get_city_admin_polygons handles clean_admin_name variations", {
  # "Kingston upon Hull, City of" should match "Kingston upon Hull" in CSV
  # via clean_admin_name stripping ", City of" and "upon" -> "on"
  admin <- make_admin_polys("Kingston upon Hull, City of")
  result <- get_city_admin_polygons(admin)

  expect_equal(nrow(result), 1)
  expect_false(is.na(result$population))
})

test_that("get_city_admin_polygons fuzzy matches with adist", {
  # "North Yorkshire" exists in CSV; "North Yorks" should fuzzy match it
  # Use large min_area thresholds to avoid filtering out our small synthetic polygon
  admin <- make_admin_polys("North Yorks")
  expect_message(
    result <- get_city_admin_polygons(admin, min_area_km2 = 0, min_area_no_pop_km2 = 0),
    "Fuzzy population match"
  )

  expect_equal(nrow(result), 1)
  expect_false(is.na(result$population))
})

test_that("get_city_admin_polygons rejects poor fuzzy matches", {
  # A completely unrelated name shouldn't fuzzy-match anything
  # Disable area filters so we can inspect the population value directly
  admin <- make_admin_polys("Zzyzzyville Nowhere")
  result <- get_city_admin_polygons(admin, min_area_km2 = 0, min_area_no_pop_km2 = 0)

  expect_equal(nrow(result), 1)
  expect_true(is.na(result$population))
})

test_that("get_city_admin_polygons filters small areas", {
  admin <- make_admin_polys(
    c("Birmingham", "TinyPlace"),
    c("Metropolitan District", "Unitary Authority")
  )
  # Override geometry for TinyPlace to be extremely small
  tiny_poly <- sf::st_polygon(list(matrix(c(
    0, 51, 0, 51.00001, 0.00001, 51.00001, 0.00001, 51, 0, 51
  ), ncol = 2, byrow = TRUE)))
  admin$geometry[2] <- sf::st_sfc(tiny_poly, crs = 4326)[[1]]

  result <- get_city_admin_polygons(admin)

  # TinyPlace should be filtered out (< min_area_km2 or no population + small)
  expect_false("TinyPlace" %in% result$city)
  expect_true("Birmingham" %in% result$city)
})

test_that("get_city_admin_polygons min_area params work", {
  admin <- make_admin_polys("Birmingham")
  # With a huge min_area, Birmingham should be filtered out
  result <- get_city_admin_polygons(admin, min_area_km2 = 1e6)
  expect_equal(nrow(result), 0)
})

test_that("get_city_admin_polygons preserves admin_type", {
  admin <- make_admin_polys(
    c("Birmingham", "Leeds"),
    c("Metropolitan District", "Metropolitan District")
  )
  result <- get_city_admin_polygons(admin)

  expect_true(all(result$admin_type == "Metropolitan District"))
})

test_that("sf_bind_rows handles mixed column sets", {
  line1 <- sf::st_sf(
    name = "A", extra = 1,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  )
  line2 <- sf::st_sf(
    name = "B", other = "x",
    geometry = sf::st_sfc(sf::st_point(c(1, 1)), crs = 4326)
  )
  result <- sf_bind_rows(list(line1, line2))

  expect_equal(nrow(result), 2)
  expect_true("extra" %in% names(result))
  expect_true("other" %in% names(result))
})

test_that("sf_bind_rows handles NULL and empty inputs", {
  line1 <- sf::st_sf(
    name = "A",
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  )
  expect_null(sf_bind_rows(list(NULL, NULL)))
  expect_equal(nrow(sf_bind_rows(list(line1, NULL))), 1)
  expect_null(sf_bind_rows(list()))
})

test_that("ensure_chr_cols adds missing columns as NA_character_", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  result <- ensure_chr_cols(df, c("b", "c", "d"))

  expect_true("c" %in% names(result))
  expect_true("d" %in% names(result))
  expect_true(all(is.na(result$c)))
  expect_true(is.character(result$b))
})

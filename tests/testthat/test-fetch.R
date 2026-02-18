test_that("CSV population data loads and has expected structure", {
  csv_path <- system.file("extdata", "uk_metropolitan_districts.csv", package = "canalysis")
  if (!nzchar(csv_path)) {
    csv_path <- "inst/extdata/uk_metropolitan_districts.csv"
  }
  skip_if(!file.exists(csv_path), "Population CSV not available")

  pop_data <- utils::read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE)
  expect_gte(ncol(pop_data), 6)
  expect_true("Code" %in% names(pop_data))
  expect_true("Name" %in% names(pop_data))
  expect_true("Geography" %in% names(pop_data))
  expect_gt(nrow(pop_data), 100)
})

test_that("canal filtering removes disused/abandoned/proposed canals", {
  skip_if_not_installed("sf")

  # Create a mock canal dataset with various statuses
  lines <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2)),
    sf::st_linestring(matrix(c(1, 1, 2, 2), ncol = 2)),
    sf::st_linestring(matrix(c(2, 2, 3, 3), ncol = 2)),
    sf::st_linestring(matrix(c(3, 3, 4, 4), ncol = 2)),
    sf::st_linestring(matrix(c(4, 4, 5, 5), ncol = 2)),
    crs = 4326
  )
  canals <- sf::st_sf(
    osm_id = as.character(1:5),
    disused = c(NA, "yes", NA, NA, NA),
    abandoned = c(NA, NA, "yes", NA, NA),
    proposed = c(NA, NA, NA, "yes", NA),
    canal = c(NA, NA, NA, NA, "drain"),
    geometry = lines
  )

  # Apply the same filtering logic used in fetch_uk_canals
  result <- canals |>
    ensure_chr_cols(c("disused", "abandoned", "proposed", "canal")) |>
    dplyr::filter(
      is.na(disused) | disused != "yes",
      is.na(abandoned) | abandoned != "yes",
      is.na(proposed) | proposed != "yes",
      is.na(canal) | !(canal %in% c("drain", "irrigation"))
    ) |>
    dplyr::distinct(osm_id, .keep_all = TRUE)

  # Only the first canal (all NA statuses) should remain
  expect_equal(nrow(result), 1)
  expect_equal(result$osm_id, "1")
})

test_that("clean_name function handles city name variations", {
  # Replicate the internal clean_name logic
  clean_name <- function(x) {
    x <- gsub(", City of$", "", x)
    x <- gsub(" City$", "", x)
    x <- gsub(" Borough$", "", x)
    x <- gsub(" Council$", "", x)
    x <- gsub("upon", "on", x, ignore.case = TRUE)
    trimws(x)
  }

  expect_equal(clean_name("Kingston upon Hull, City of"), "Kingston on Hull")
  expect_equal(clean_name("Newcastle upon Tyne"), "Newcastle on Tyne")
  expect_equal(clean_name("Manchester City"), "Manchester")
  expect_equal(clean_name("Some Borough"), "Some")
  expect_equal(clean_name("Leeds"), "Leeds")
})

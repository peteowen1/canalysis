skip_on_cran()
skip_if_offline()

res <- rank_uk_cities_by_canals()

test_that("function returns a tibble with expected columns", {
  expect_true(all(c("city","population","length_km","length_mi","area_km2","density_km_per_km2","population_density_per_km2") %in% names(res)))
  expect_gt(nrow(res), 10)
})

test_that("population data is included and valid", {
  expect_true("population" %in% names(res))
  expect_true("population_density_per_km2" %in% names(res))
  # Some cities should have population data (not all will be NA)
  expect_true(sum(!is.na(res$population)) > 0)
})

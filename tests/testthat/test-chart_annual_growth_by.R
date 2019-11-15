context("chart_annual_growth")

test_that("test_ems_data, by cat_id", {

  test_ems_data %>%
    chart_annual_growth(
      base_year = CY(2015),
      verbose = TRUE)

})

test_that("test_ems_data (spread by year), by cat_id", {

  test_ems_data %>%
    spread(
      year, ems_qty) %>%
    chart_annual_growth(
      base_year = CY(2015),
      verbose = TRUE)

})

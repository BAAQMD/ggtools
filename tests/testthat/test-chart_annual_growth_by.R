context("chart_annual_growth")

test_that("test_growth_data", {
  expect_silent(
    test_growth_data %>%
      chart_annual_growth(
        base_year = CY(2015),
        verbose = FALSE))
})

test_that("test_growth_data, gathered by year", {
  expect_silent(
    test_growth_data %>%
      gather(
        year,
        gf_qty,
        starts_with("CY")) %>%
      chart_annual_growth(
        base_year = CY(2015),
        verbose = FALSE))
})

test_that("test_ems_data, color = cnty_abbr", {

  expect_silent(
    test_ems_data %>%
      chart_annual_growth_by(
        cnty_abbr,
        flag_years = CY(2015),
        base_year = CY(2015),
        verbose = FALSE))

})


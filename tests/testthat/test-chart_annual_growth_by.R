context("chart_annual_growth")

test_that("chart_annual_growth_by(test_ems_data, cat_id)", {

  test_ems_data %>%
    chart_annual_growth_by(
      base_year = CY(2015),
      verbose = TRUE)

})

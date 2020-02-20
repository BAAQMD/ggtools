context("chart_annual_control_factors")

test_that("chart_annual_control_factors_by (NOx and PM2.5)", {

  test_cf_data %>%
    filter(
      pol_abbr %in% c("NOx", "PM2.5")) %>%
    chart_annual_control_factors_by(
      color = pol_abbr,
      flag_years = CY(2011),
      base_year = CY(2011),
      verbose = TRUE)

})

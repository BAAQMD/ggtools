context("chart_annual_emissions")

test_that("chart_annual_emissions_by(test_ems_data, cat_id)", {

  test_ems_data %>%
    filter(
      pol_abbr %in% c("NOx", "PM2.5")) %>%
    chart_annual_emissions_by(
      cat_id,
      base_year = BY(2011),
      verbose = TRUE)

})

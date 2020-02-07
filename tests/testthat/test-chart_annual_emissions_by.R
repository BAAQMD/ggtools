context("chart_annual_emissions")

test_that("chart_annual_emissions_by (NOx and PM2.5)", {

  test_ems_data %>%
    filter(
      pol_abbr %in% c("NOx", "PM2.5")) %>%
    chart_annual_emissions_by(
      cat_id,
      flag_years = CY(2011),
      base_year = CY(2011),
      verbose = TRUE)

})

test_that("chart_annual_emissions_by (color by cnty_abbr)", {

  test_ems_data %>%
    filter(
      cat_id == 284L) %>%
    filter(
      pol_abbr %in% c("NOx", "PM2.5")) %>%
    chart_annual_emissions_by(
      color = cnty_abbr,
      flag_years = CY(2011),
      base_year = CY(2011),
      verbose = TRUE)

})

test_that("chart_annual_emissions_by (fill by cnty_abbr)", {

  test_ems_data %>%
    filter(
      cat_id == 284L) %>%
    filter(
      pol_abbr %in% c("NOx", "PM2.5")) %>%
    chart_annual_emissions_by(
      color = cnty_abbr,
      geom = "area",
      base_year = CY(2011))

  test_ems_data %>%
    filter(
      cat_id == 284L) %>%
    filter(
      pol_abbr %in% c("NOx", "PM2.5")) %>%
    chart_annual_emissions_by(
      fill = cnty_abbr,
      base_year = CY(2011))

})

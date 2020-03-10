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

test_that("chart_annual_emissions_by (less than 1 ton/yr", {

  require(BY2011)

  BY2011_annual_emission_data %>%
    filter(
      cat_id == 314) %>% # incineration (point)
    filter(
      pol_abbr == "N2O") %>%
    chart_annual_emissions(
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

test_that("chart_annual_emissions_by (mix of NA and finite values)", {

  BY2011_test_data <-
    BY2011::BY2011_annual_emission_data %>%
    filter(
      cat_id %in% c(1930:1931)) %>%
    filter(
      pol_abbr == "TOG") %>%
    sum_annual_emissions_by(
      cat_id,
      pol_abbr)

  BY2011_test_data %>%
    chart_annual_emissions_by(
      fill = cat_id,
      base_year = CY(2011))

})

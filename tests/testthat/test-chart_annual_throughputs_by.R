context("chart_annual_throughputs")

test_that("chart_annual_throughputs_by, single pollutant", {

  expect_warning(
    test_tput_data %>%
      filter(
        pol_abbr == "NOx") %>%
      chart_annual_throughputs_by(
        cat_id,
        flag_years = CY(2011),
        base_year = CY(2011),
        verbose = TRUE),
    "pol_abbr, ems_qty, ems_unit")

})

test_that("chart_annual_throughputs_by, multi-pollutant (should issue warning)", {

  expect_warning(
    test_tput_data %>%
      filter(
        pol_abbr %in% c("NOx", "PM2.5")) %>%
      chart_annual_throughputs_by(
        cat_id,
        flag_years = CY(2011),
        base_year = CY(2011),
        verbose = TRUE),
    "pol_abbr")

})

context("chart_annual_quantities")

test_that("chart_annual_quantities_by(test_cf_data, cat_id)", {

  test_cf_data %>%
    chart_annual_quantities_by(
      cat_id,
      verbose = TRUE)

})

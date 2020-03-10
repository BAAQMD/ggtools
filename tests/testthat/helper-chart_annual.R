#
# library(DataBank)
#
# BY(2011) %>%
#   DB_control_factors() %>%
#   filter(cat_id == 284L) %>%
#   annualize_DB_control_factors() %>%
#   write_rds(here::here("tests", "testthat", "helper-chart_annual-test_cf_data.Rds"))
#

test_cf_data <-
  read_rds(here::here("tests", "testthat", "helper-chart_annual-test_cf_data.Rds"))

#
# library(BY2011)
#
# BY2011::BY2011_annual_emission_data %>%
#   filter(cat_id == 284L) %>%
#   write_rds(here::here("tests", "testthat", "helper-chart_annual-test_ems_data.Rds"))
#

test_ems_data <-
  read_rds(here::here("tests", "testthat", "helper-chart_annual-test_ems_data.Rds"))

test_tput_data <-
  test_ems_data %>%
  mutate(
    tput_qty = ems_qty / pi,
    tput_unit = "million ft^3")

test_tput_data %>%
  filter(
    pol_abbr == "NOx") %>%
  filter_years(
    CY(2011)) %>%
  pull_total(
    tput_qty)

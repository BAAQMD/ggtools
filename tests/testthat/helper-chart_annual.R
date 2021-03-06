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

# test_tput_data %>%
#   filter(
#     pol_abbr == "NOx") %>%
#   filter_years(
#     CY(2011)) %>%
#   pull_total(
#     tput_qty)

test_growth_data <-
  tibble(
    cat_id = c(283, 284),
    gpf_id = c(
      "GHGs-LBNL-2017-Q1-Residential-shifted",
      "GHGs-LBNL-2017-Q1-Residential-shifted"
    ),
    comment.crosswalk = c(NA, NA),
    comment.profile = c(NA, NA),
    cnty_abbr = c("TOT", "TOT"),
    CY2015 = c(0.80438172, 0.80438172),
    CY2016 = c(0.8094, 0.8094),
    CY2017 = c(0.81449922, 0.81449922),
    CY2018 = c(0.8195175, 0.8195175),
    CY2019 = c(0.82332168, 0.82332168),
    CY2020 = c(0.82583082, 0.82583082),
    CY2021 = c(0.82712586, 0.82712586),
    CY2022 = c(0.82833996, 0.82833996),
    CY2023 = c(0.82712586, 0.82712586),
    CY2024 = c(0.82453578, 0.82453578),
    CY2025 = c(0.8195175, 0.8195175),
    CY2026 = c(0.81449922, 0.81449922),
    CY2027 = c(0.8094, 0.8094),
    CY2028 = c(0.80179164, 0.80179164),
    CY2029 = c(0.79296918, 0.79296918),
    CY2030 = c(0.78285168, 0.78285168),
    CY2031 = c(0.77394828, 0.77394828),
    CY2032 = c(0.76512582, 0.76512582),
    CY2033 = c(0.75500832, 0.75500832),
    CY2034 = c(0.74618586, 0.74618586),
    CY2035 = c(0.73606836, 0.73606836),
    CY2036 = c(0.72595086, 0.72595086),
    CY2037 = c(0.71704746, 0.71704746),
    CY2038 = c(0.70692996, 0.70692996),
    CY2039 = c(0.6981075, 0.6981075),
    CY2040 = c(0.68799, 0.68799),
    CY2041 = c(0.67916754, 0.67916754),
    CY2042 = c(0.67155918, 0.67155918),
    CY2043 = c(0.66645996, 0.66645996),
    CY2044 = c(0.66144168, 0.66144168),
    CY2045 = c(0.65893254, 0.65893254),
    CY2046 = c(0.6576375, 0.6576375),
    CY2047 = c(0.65634246, 0.65634246),
    CY2048 = c(0.6576375, 0.6576375),
    CY2049 = c(0.66014664, 0.66014664),
    CY2050 = c(0.66524586, 0.66524586),
    category = factor(c("Space Heating", "Water Heating"))
  )

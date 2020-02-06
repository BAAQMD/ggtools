#' facet_by_pollutant
#'
#' @note used by [chart_annual_emissions_by()] and [chart_annual_emission_factors_by()]
#'
facet_by_pollutant <- function (
  chart_object,
  facet_var = "pol_abbr",
  verbose = getOption("verbose")
) {

  facet_formula <-
    as.formula(
      str_c("~ ", facet_var))

  chart_faceting <-
    lemon::facet_rep_wrap(
      facet_formula,
      ncol = 1,
      scales = "free_y",
      repeat.tick.labels = TRUE)

  return(chart_object + chart_faceting)

}

#' chart_annual_emissions_by
#'
#' @usage chart_annual_emissions_by(data, ...)
#' @describeIn chart_annual_by Chart annual emissions.
#'
#' @export
chart_annual_emission_factors_by <- function (
  data = NULL,
  ...,
  mapping = aes(),
  qty_var = "ef_qty",
  geom = NULL,
  year_limits = CY(1990, 2040),
  year_breaks = seq(1990, 2050, by = 10),
  year_expand = expand_scale(add = c(5, 5), mult = c(0, 0)),
  flag_years = NULL,
  flag_labels = "{signif(ef_qty, 4)} {ef_unit}",
  base_year = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[chart_annual_emission_factors_by] ", ...)

  #
  # If `year` isn't present, try reshaping the data.
  #
  if (!is.null(data) && ("year" %not_in% names(data))) {
    data <- gather_years(data, "ef_qty", verbose = verbose)
    qty_var <- "ef_qty"
  }

  #
  # Let `chart_y_scale` be a scale specific to emission factors.
  #

  if (is.null(data)) {

    chart_y_unit <- "lb/tput"
    chart_y_scale <- NULL

  } else {

    chart_y_unit <-
      data %>%
      pull_distinct(
        ef_unit) %>%
      ensurer::ensure(
        length(.) == 1)

    chart_y_scale <-
      scale_y_emissions(
        chart_y_unit,
        labels = format_SI,
        expand = expand_scale(mult = c(0, 0.3)))

  }

  #
  # Automatically facet by pollutant.
  #
  chart_object <-
    data %>%
    chart_annual_quantities_by(
      pol_abbr,
      ...,
      mapping = mapping,
      geom = geom,
      qty_var = qty_var,
      chart_y_scale = chart_y_scale,
      year_limits = year_limits,
      year_breaks = year_breaks,
      year_expand = year_expand,
      flag_years = flag_years,
      flag_labels = flag_labels,
      base_year = base_year,
      title = title,
      subtitle = subtitle,
      caption = caption,
      verbose = verbose)

  chart_object <-
    facet_by_pollutant(
      chart_object,
      verbose = verbose)

  return(chart_object)

}

#' chart_annual_emissions
#'
#' @noRd
#'
#' @export
chart_annual_emission_factors <-
  chart_annual_emission_factors_by

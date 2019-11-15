#' chart_annual_emissions_by
#'
#' @usage chart_annual_emissions_by(data, ...)
#' @describeIn chart_annual_by Chart annual emissions.
#'
#' @export
chart_annual_emissions_by <- function (
  data = NULL,
  ...,
  mapping = aes(),
  geom = NULL,
  year_limits = NULL,
  flag_years = NULL,
  flag_labels = "{signif(ems_qty, 4)} {ems_unit}",
  base_year = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[chart_annual_emissions_by] ", ...)

  msg("WARNING: experimental --- do not use in production!")

  #
  # Let `chart_y_scale` be a scale specific to emissions.
  #

  if (is.null(data)) {

    chart_y_unit <- "tons/yr"
    chart_y_scale <- NULL

  } else {

    chart_y_unit <-
      data %>%
      pull_distinct(
        ems_unit) %>%
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

  chart_faceting <-
    lemon::facet_rep_wrap(
      ~ pol_abbr,
      ncol = 1,
      scales = "free_y",
      repeat.tick.labels = TRUE)

  chart_object <-
    data %>%
    chart_annual_quantities_by(
      pol_abbr,
      ...,
      mapping = mapping,
      geom = geom,
      qty_var = "ems_qty",
      chart_y_scale = chart_y_scale,
      year_limits = year_limits,
      flag_years = flag_years,
      flag_labels = flag_labels,
      base_year = base_year,
      title = title,
      subtitle = subtitle,
      caption = caption,
      verbose = verbose) +
    chart_faceting

  return(chart_object)

}

#' chart_annual_emissions
#'
#' @noRd
#'
#' @export
chart_annual_emissions <-
  chart_annual_emissions_by

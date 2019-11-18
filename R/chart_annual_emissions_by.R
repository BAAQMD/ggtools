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
  qty_var = "ems_qty",
  geom = NULL,
  year_limits = CY(1990, 2040),
  year_breaks = seq(1990, 2050, by = 10),
  year_expand = expand_scale(add = c(5, 5), mult = c(0, 0)),
  flag_years = NULL,
  flag_labels = "{signif(ems_qty, 4)} {ems_unit}",
  base_year = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[chart_annual_emissions_by] ", ...)

  #
  # If `year` isn't present, try reshaping the data.
  #
  if (!is.null(data) && ("year" %not_in% names(data))) {
    data <- gather_years(data, "ems_qty", verbose = verbose)
    qty_var <- "ems_qty"
  }

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

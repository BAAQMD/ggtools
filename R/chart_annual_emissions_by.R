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
  base_year = NULL,
  qty_var = "ems_qty",
  chart_y_scale = NULL,
  chart_y_labels = NULL,
  geom = NULL,
  facet_rows = NULL,
  facet_cols = NULL,
  facet_scales = "free_y",
  year_limits = NULL,
  year_breaks = NULL,
  year_expand = NULL,
  flag_years = NULL,
  flag_labels = "{format_qty(ems_qty)} {chart_y_unit}",
  flag_unique = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[chart_annual_emissions_by] ", ...)

  if ("pollutant" %in% names(data)) {
    pol_var <- "pollutant"
  } else if ("pol_abbr" %in% names(data)) {
    pol_var <- "pol_abbr"
  } else {
    err_msg <- "[chart_annual_emissions_by] can't find `pollutant` or `pol_abbr` in your data"
    stop(err_msg)
  }

  if (is.null(facet_rows)) {
    msg("setting `facet_rows` to \"", pol_var, "\"")
    facet_rows <- pol_var
  }


  #
  # If `year` isn't present, try reshaping the data from wide to long.
  #
  if (!is.null(data) && ("year" %not_in% names(data))) {
    qty_var <- "ems_qty"
    data <- gather_years(data, qty_var, verbose = verbose)
  }

  #
  # Let `chart_y_scale` be a scale specific to emissions.
  #

  if (is.null(data)) {

    chart_y_unit <- "emissions"
    chart_y_scale <- NULL

  } else {

    unit_var <-
      str_replace(
        qty_var,
        "_qty$",
        "_unit")

    chart_y_unit <-
      data %>%
      pull(
        unit_var) %>%
      unique()

    if (is.null(chart_y_scale)) {
      chart_y_scale <-
        scale_y_emissions(
          name = chart_y_unit,
          labels = chart_y_labels,
          expand = ggplot2::expansion(mult = c(0, 0.3)),
          verbose = verbose)
    }

  }

  chart_object <-
    data %>%
    chart_annual_quantities_by(
      ...,
      pol_var,
      mapping = mapping,
      qty_var = qty_var,
      geom = geom,
      facet_rows = facet_rows,
      facet_cols = facet_cols,
      facet_scales = facet_scales,
      chart_y_scale = chart_y_scale,
      chart_y_labels = chart_y_labels,
      chart_y_unit = chart_y_unit,
      year_limits = year_limits,
      year_breaks = year_breaks,
      year_expand = year_expand,
      flag_years = flag_years,
      flag_labels = flag_labels,
      flag_unique = flag_unique,
      base_year = base_year,
      title = title,
      subtitle = subtitle,
      caption = caption,
      verbose = verbose)

  return(chart_object)

}

#' chart_annual_emissions
#'
#' @noRd
#'
#' @export
chart_annual_emissions <-
  chart_annual_emissions_by

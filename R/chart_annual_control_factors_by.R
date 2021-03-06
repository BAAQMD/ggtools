#' chart_annual_control_factors_by
#'
#' @usage chart_annual_control_factors_by(data, ...)
#' @describeIn chart_annual_by Chart annual control factors.
#'
#' @export
chart_annual_control_factors_by <- function (
  data = NULL,
  ...,
  mapping = aes(),
  qty_var = "cf_qty",
  chart_y_scale = NULL,
  geom = NULL,
  facet_rows = "category",
  facet_cols = NULL,
  facet_scales = "fixed",
  year_limits = NULL,
  year_breaks = NULL,
  year_expand = NULL,
  flag_years = NULL,
  flag_labels = "{format_percentage(1 - cf_qty, digits = 1)}",
  flag_unique = NULL,
  base_year = NULL,
  reverse = TRUE,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) {
    message("[chart_annual_control_factors_by] ", ...)
  }

  #
  # If `year` isn't present, try reshaping the data.
  #
  if (!is.null(data) && ("year" %not_in% names(data))) {
    qty_var <- "ef_qty"
    data <- gather_years(data, qty_var, verbose = verbose)
  }

  #
  # Let `chart_y_scale` be in terms of percentages. Because these are fractions,
  # they should never be more than 100%, or less than 0%.
  #
  if (is.null(chart_y_scale)) {

    chart_y_scale <-
      scale_y_percentage(
        "% controlled",
        reverse = reverse,
        limits = c(0, 1.00),
        breaks = seq(0, 1, by = 0.2),
        expand = ggplot2::expansion(mult = c(0, 0)))

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
      facet_rows = facet_rows,
      facet_cols = facet_cols,
      facet_scales = facet_scales,
      qty_var = qty_var,
      chart_y_scale = chart_y_scale,
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

  # chart_object <-
  #   facet_by_pollutant(
  #     chart_object,
  #     verbose = verbose)

  return(chart_object)

}

#' chart_annual_emissions
#'
#' @noRd
#'
#' @export
chart_annual_control_factors <-
  chart_annual_control_factors_by

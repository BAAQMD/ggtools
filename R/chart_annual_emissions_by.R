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
  geom = NULL,
  facet_rows = "pol_abbr",
  facet_cols = NULL,
  facet_scales = "free_y",
  year_limits = NULL,
  year_breaks = NULL,
  year_expand = NULL,
  flag_years = NULL,
  flag_labels = "{format_SI(ems_qty, digits = 4)} {chart_y_unit}",
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

    chart_y_unit <- "tput/yr"
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
      unique() %>%
      ensurer::ensure(
        length(.) == 1)

    chart_y_scale <-
      scale_y_emissions(
        chart_y_unit,
        labels = format_SI,
        expand = expand_scale(mult = c(0, 0.3)))

  }

  chart_object <-
    data %>%
    chart_annual_quantities_by(
      ...,
      pol_abbr,
      mapping = mapping,
      qty_var = qty_var,
      geom = geom,
      facet_rows = facet_rows,
      facet_cols = facet_cols,
      facet_scales = facet_scales,
      chart_y_scale = chart_y_scale,
      chart_y_unit = chart_y_unit,
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

  return(chart_object)

}

#' chart_annual_emissions
#'
#' @noRd
#'
#' @export
chart_annual_emissions <-
  chart_annual_emissions_by

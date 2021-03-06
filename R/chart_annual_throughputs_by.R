#' chart_annual_throughputs_by
#'
#' @usage chart_annual_throughputs_by(data, ...)
#'
#' @describeIn chart_annual_by Chart annual throughputs.
#'
#' @export
chart_annual_throughputs_by <- function (
  data = NULL,
  ...,
  mapping = aes(),
  base_year = NULL,
  qty_var = "tput_qty",
  chart_y_scale = NULL,
  chart_y_labels = NULL,
  chart_y_unit = NULL,
  chart_y_title = chart_y_unit,
  geom = NULL,
  facet_rows = NULL,
  facet_cols = NULL,
  facet_scales = "free_y",
  year_limits = NULL,
  year_breaks = NULL,
  year_expand = NULL,
  flag_years = NULL,
  flag_labels = "{format_qty(tput_qty)} {chart_y_unit}",
  flag_unique = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[chart_annual_throughputs_by] ", ...)

  pol_vars <-
    tidyselect::vars_select(
      names(data),
      dplyr::matches("pol_"))

  ems_vars <-
    tidyselect::vars_select(
      names(data),
      dplyr::matches("ems_"))

  if (length(pol_vars) > 0 || length(ems_vars) > 0) {

    warn_msg <- glue::glue(
      "Found these variables in your data: {str_csv(union(pol_vars, ems_vars))}.",
      "You might be double-counting throughputs!",
      "Trying to be helpful via distinct(...).",
      .sep = " ")

    warning(warn_msg)

    key_vars <-
      unname(
        tidyselect::vars_select(
          names(data),
          year,
          ...))

    msg("key_vars is: ", strtools::str_csv(names(key_vars)))

    #
    # Try to be helpful ...
    #
    data <-
      data %>%
      select(
        starts_with("tput_"), # FIXME: should be based on `qty_var`
        !!key_vars) %>%
      distinct()

  }

  #
  # If `year` isn't present, try reshaping the data.
  #
  if (!is.null(data) && ("year" %not_in% names(data))) {
    qty_var <- "tput_qty"
    data <- gather_years(data, qty_var, verbose = verbose)
  }

  #
  # Let `chart_y_scale` be a scale specific to throughputs.
  #

  if (is.null(data)) {

    chart_y_unit <- "throughput"
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


    if (is.null(chart_y_scale)) {

      chart_y_scale <-
        scale_y_throughputs(
          chart_y_title,
          labels = chart_y_labels,
          expand = ggplot2::expansion(mult = c(0, 0.3)),
          verbose = verbose)
    }

  }

  chart_object <-
    data %>%
    chart_annual_quantities_by(
      ...,
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

#' chart_annual_throughputs
#'
#' @noRd
#'
#' @export
chart_annual_throughputs <-
  chart_annual_throughputs_by

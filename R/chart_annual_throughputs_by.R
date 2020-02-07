#' chart_annual_throughputs_by
#'
#' @usage chart_annual_throughputs_by(data, ...)
#' @describeIn chart_annual_by Chart annual throughputs.
#'
#' @details `chart_annual_throughputs_by(...)` tries to avoid double-counting.
#'   If there are variables beginning with `pol_` or `ems_` in your data, it
#'   will first issue a warning. Then, it will try to replace `data` with
#'   something very much like `distinct(data, year, ..., tput_qty, tput_unit)`.
#'
#' @export
chart_annual_throughputs_by <- function (
  data = NULL,
  ...,
  mapping = aes(),
  base_year = NULL,
  qty_var = "tput_qty",
  chart_y_scale = NULL,
  geom = NULL,
  year_limits = NULL,
  year_breaks = NULL,
  year_expand = NULL,
  flag_years = NULL,
  flag_labels = "{format_SI(tput_qty, digits = 4)} {chart_y_unit}",
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

    print(data)

  }

  #
  # If `year` isn't present, try reshaping the data.
  #
  if (!is.null(data) && ("year" %not_in% names(data))) {
    data <- gather_years(data, "tput_qty", verbose = verbose)
    qty_var <- "tput_qty"
  }

  #
  # Let `chart_y_scale` be a scale specific to throughputs.
  #

  if (is.null(data)) {

    chart_y_unit <- "ton/yr"
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
      scale_y_throughputs(
        chart_y_unit,
        labels = format_SI,
        expand = expand_scale(mult = c(0, 0.3)))

  }

  chart_object <-
    data %>%
    chart_annual_quantities_by(
      ...,
      mapping = mapping,
      qty_var = qty_var,
      geom = geom,
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

#' chart_annual_throughputs
#'
#' @noRd
#'
#' @export
chart_annual_throughputs <-
  chart_annual_throughputs_by

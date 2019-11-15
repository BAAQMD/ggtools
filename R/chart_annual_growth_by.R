#' chart_annual_growth_by
#'
#' @note In `chart_annual_growth_by()`, `base_year` serves to index the reference year for normalization.
#'
#' @usage chart_annual_growth_by(data, ..., base_year)
#' @describeIn chart_annual_by Chart annual growth, relative to some base year.
#'
#' @export
chart_annual_growth_by <- function (
  data,
  ...,
  base_year,
  qty_var = NULL,
  geom = "line",
  flag_years = NULL,
  flag_labels = "{format_percent_change(gf_qty, digits = 1)}",
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[chart_annual_growth_by] ", ...)

  #
  # Autodetect `qty_var`, if not specified.
  #

  if (is.null(qty_var)) {

    qty_var <-
      vartools::find_var(
        data,
        suffix = "_qty")

  }

  #
  # To normalize a chunk of (grouped) data. (Look for `group_modify()`, below.)
  #
  normalize <- function (.x, .y) {

    f <- function (x) {
      i <- which(elide_year(.x[["year"]]) == elide_year(base_year))
      return(x / x[i])
    }

    normalized_data <-
      mutate(
        .x,
        gf_qty = f(pull(.x, !!qty_var)))

    return(normalized_data)

  }

  #
  # Let `by_vars` be the `...`.
  #

  by_vars <-
    tidyselect::vars_select(
      names(data),
      ...)

  msg("by_vars is: ", by_vars)

  #
  # Apply `normalize()` (defined above) to each group of data,
  # after aggregating (using `by_vars`).
  #

  normalized_data <-
    data %>%
    annual_quantities_by(
      by_vars,
      verbose = verbose) %>%
    group_by_at(
      vars(by_vars)) %>%
    group_modify(
      normalize)

  #
  # Let `chart_y_scale` be in terms of percentages.
  #
  chart_y_scale <-
    scale_y_percentage(
      NULL, # #glue::glue("{qty_var} (normalized to {base_year})"),
      limits = c(0, NA),
      expand = expand_scale(mult = c(0, 0.3)))

  chart_object <-
    normalized_data %>%
    chart_annual_quantities_by(
      ...,
      qty_var = "gf_qty",
      geom = geom,
      chart_y_scale = chart_y_scale,
      flag_years = flag_years,
      flag_labels = flag_labels,
      base_year = base_year,
      title = title,
      subtitle = subtitle,
      caption = caption,
      verbose = verbose)

  return(chart_object)

}

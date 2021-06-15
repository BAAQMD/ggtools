#' chart_annual_growth_by
#'
#' @note In `chart_annual_growth_by()`, `base_year` serves to index the reference year for normalization.
#'
#' @usage chart_annual_growth_by(data, ..., base_year)
#' @describeIn chart_annual_by Chart annual growth, relative to some base year.
#'
#' @importFrom yeartools CY
#' @importFrom qtytools sum_annual_quantities_by
#'
#' @export
chart_annual_growth_by <- function (
  data,
  ...,
  mapping = aes(),
  base_year,
  ref_year,
  qty_var = NULL,
  chart_y_scale = NULL,
  geom = NULL,
  facet_rows = NULL,
  facet_cols = NULL,
  facet_scales = "fixed",
  year_limits = NULL,
  year_breaks = NULL,
  year_expand = NULL,
  flag_years = NULL,
  flag_labels = "{format_percent_change(gf_qty, digits = 1)}",
  flag_unique = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[chart_annual_growth_by] ", ...)

  if (missing(ref_year)) {
    if (missing(base_year)) {
      stop("[chart_annual_growth_by] either `base_year` or `ref_year` is required.")
    } else {
      ref_year <- CY(elide_year(base_year))
    }
  }

  #
  # If `year` isn't present, try reshaping the data.
  #
  if (!is.null(data) && ("year" %not_in% names(data))) {
    qty_var <- "gf_qty"
    data <- gather_years(data, !!qty_var, verbose = verbose)
  }

  #
  # Autodetect `qty_var`, if not specified.
  #
  # Although `chart_annual_quantities_by()` would autodetect `qty_var`, we
  # need to autodetect it here so that we can use it in our `normalize()`
  # function (as defined below). `normalize()` then computes `gf_qty` ---
  # overwriting any existing `gf_qty` --- and we then explicitly tell
  # `chart_annual_quantities_by()` to use `gf_qty`.
  #

  if (!is.null(data) && is.null(qty_var)) {

    qty_var <-
      vartools::find_qty_var(
        data,
        verbose = verbose)

  }

  msg("qty_var is: ", qty_var)

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
  # Let `by_vars` be the `...`, plus whatever is used for faceting.
  #
  by_vars <-
    tidyselect::vars_select(
      names(data),
      ...)

  if (!is.null(facet_rows) || !is.null(facet_cols)) {

    facet_vars <-
      c(facet_rows, facet_cols) %>%
      set_names()

    by_vars <-
      c(by_vars, facet_vars)

  }

  msg("by_vars is: ", strtools::str_csv(names(by_vars)))

  #
  # Apply `normalize()` (defined above) to each group of data,
  # after aggregating (using `by_vars`).
  #

  normalized_data <-
    data %>%
    sum_annual_quantities_by(   # FIXME: explicitly supply value var (now "gf_qty")
      by_vars,
      verbose = verbose) %>%
    group_by_at(
      vars(by_vars)) %>%
    group_modify(
      normalize)

  #
  # Let `chart_y_scale` be in terms of percentages.
  #
  if (is.null(chart_y_scale)) {

    chart_y_scale <-
      scale_y_percentage(
        "% growth", # #glue::glue("{qty_var} (normalized to {base_year})"),
        limits = c(0, NA),
        #breaks = seq(0, 10, by = 0.2),
        expand = ggplot2::expansion(mult = c(0, 0.3)))

  }

  chart_object <-
    normalized_data %>%
    chart_annual_quantities_by(
      ...,
      mapping = mapping,
      qty_var = "gf_qty",
      geom = geom,
      facet_rows = facet_rows,
      facet_cols = facet_cols,
      facet_scales = facet_scales,
      chart_y_scale = chart_y_scale,
      chart_y_unit = NULL,
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
      verbose = verbose) +
    geom_hline(
      yintercept = 1.000,
      color = "black",
      alpha = I(0.3),
      size = 0.5,
      linetype = "dotted")

  return(chart_object)

}

#' chart_annual_growth
#'
#' @noRd
#'
#' @export
chart_annual_growth <-
  chart_annual_growth_by


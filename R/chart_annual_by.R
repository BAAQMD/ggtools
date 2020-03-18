#' @name chart_annual_by
#'
#' @title Charting Annual Data
#'
#' @description
#' **Please don't use in production code yet! Thank you!** The pollutant-faceting feature of `chart_annual_emissions_by()`, in particular, may change.
#'
#' **`chart_annual_quantities_by()`** is the workhorse function. It will
#' auto-detect the first variable in `data` that ends with `_qty`, unless you
#' supply an explicit `qty_var`. It relies on a default [scale_x_annual()],
#' which you can tweak by supplying `year_breaks`, `year_limits`, and/or
#' `year_expand`. You can use it to chart arbitrary annual quantities, like
#' `cf_qty` (control factors), `ef_qty` (emission factors), or `fee_qty` (fees).
#'
#' **`chart_annual_emissions_by()`** is a specialized variant. It expects to
#' find `ems_qty` and `ems_unit` in `data`. It relies on `scale_y_emissions()`
#' for the y-axis, and displays both quantities and units in flagged values. It
#' also facets the resulting chart by pollutant, relying on `pol_abbr` by
#' default.
#'
#' **`chart_annual_throughputs_by(...)`** tries to avoid double-counting. If
#' there are variables beginning with `pol_` or `ems_` in your data, it will
#' first issue a warning. Then, it will try to replace `data` with something
#' very much like `distinct(data, year, ..., tput_qty, tput_unit)`.
#'
#' **`chart_annual_growth_by()`** is a specialized variant. Like
#' `chart_annual_quantities_by()`, it will try to find a variable ending with
#' `_qty` in your `data`, unless you supply an explicit `qty_var`. It relies on
#' [scale_y_percentage()] for the y-axis, and formats flagged values using
#' [format_percent_change()].
#'
#' @param data tabular, with a column `year`
#' @param ... variables to chart by. Must be present in `data`.
#' @param geom "line", "point", "col", or "area"
#' @param year_limits like `CY(1990, 2040)`
#' @param year_breaks like `seq(1990, 2050, by = 10)`
#' @param year_expand see [ggplot2::expansion()]
#' @param base_year required for `chart_annual_growth()`; optional otherwise. Displays a mark at the corresponding year.
#' @param flag_years if provided, will display actual values at these years.
#' @param flag_labels controls the information displayed in flags. See
#'   [glue::glue()].
#' @param title passed to [ggplot2::labs()]
#' @param subtitle passed to `labs()`
#' @param caption passed to `labs()`
#' @param verbose display messages
#'
#' @rdname chart_annual_by
#'
NULL

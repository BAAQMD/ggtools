#' chart_annual_quantities
#'
#' @note foundation for chart_annual_*
#'
#' @seealso
#' - [chart_annual_growth()]
#' - [chart_annual_emissions()]
#'
#' @export
chart_annual_quantities <- function (
  chart_data,
  year_limits = c(NA, NA),
  year_breaks = seq(1990, 2050, by = 10),
  year_expand = expand_scale(add = c(1, 1), mult = c(0, 0)),
  ...
) {

  require(yeartools)

  chart_color_scale <-
    scale_color_excel_new()

  chart_fill_scale <-
    scale_fill_excel_new()

  chart_theme <-
    theme_simple() +
    theme(
      plot.subtitle = element_text(
        size = rel(0.9)))

  if (!all_true(is.na(year_limits))) {
    year_limits <- elide_year(year_limits)
  }

  chart_x_scale <-
    scale_x_annual(
      limits = year_limits,
      breaks = elide_year(year_breaks),
      expand = year_expand)

  chart_object <-
    chart_data %>%
    ggplot() +
    aes(x = yeartools::elide_year(year)) +
    chart_theme +
    chart_x_scale +
    chart_color_scale +
    chart_fill_scale

  return(chart_object)

}

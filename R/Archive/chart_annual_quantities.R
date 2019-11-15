#' chart_annual_quantities
#'
#' @param data tabular data with column `year`; passed to `ggplot()`
#' @param mapping passed to `ggplot()`
#' @param year_limits four-digit integers; passed to [scale_x_annual()]
#' @param year_breaks four-digit integers; passed to [scale_x_annual()]
#' @param year_expand passed to [scale_x_annual()]
#' @param ... reserved for future use
#'
#' @seealso
#' - [chart_annual_emissions()]
#' - [chart_annual_growth()]
#'
#' @export
chart_annual_quantities <- function (
  data = NULL,
  mapping = aes(),
  year_limits = c(NA, NA),
  year_breaks = seq(1990, 2050, by = 5),
  year_expand = expand_scale(add = c(0, 1), mult = c(0, 0)),
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ...,
  verbose = getOption("verbose")
) {

  require(yeartools)

  chart_color_scale <-
    ggthemes::scale_color_few(
      palette = "Dark")

  chart_fill_scale <-
    ggthemes::scale_fill_few(
      palette = "Dark")

  chart_theme <-
    theme_simple() +
    theme(
      plot.subtitle = element_text(size = rel(0.9)))

  if (!all_true(is.na(year_limits))) {
    year_limits <- elide_year(year_limits)
  }

  if (is.null(data)) {

    chart_x_scale <-
      scale_x_annual(
        limits = year_limits,
        expand = year_expand)

  } else {

    chart_x_scale <-
      scale_x_annual(
        limits = year_limits,
        breaks = year_breaks,
        expand = year_expand)

  }

  chart_description <- local({

    # Default chart caption: "DRAFT YYYY-mm-dd".
    if (is.null(caption)) {
      caption <-
        glue::glue(
          'DRAFT {strtools::str_datestamp()}',
          .sep = "\n")
    }

    chart_description <-
      labs(
        title = title,
        subtitle = subtitle,
        caption = caption)

  })

  chart_object <-
    ggplot(
      data = data,
      mapping = mapping) +
    aes(x = yeartools::elide_year(year)) +
    chart_theme +
    chart_x_scale +
    chart_color_scale +
    chart_fill_scale +
    chart_description

  return(chart_object)

}

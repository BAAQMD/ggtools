#' Plot annual emissions
#'
#' @seealso
#' - [gg_throughput_chart()]
#'
#' @importFrom ggplot2 labs
#'
#' @export
gg_emission_chart <- function (
  chart_data,
  limits = list(x = NULL, y = c(0, NA)),
  caption = "DRAFT {format(Sys.Date(), '%Y-%m-%d')}",
  verbose = TRUE
) {

  msg <- function (...) if(isTRUE(verbose)) message("[gg_emission_chart] ", ...)

  chart_x_scale <-
    ggtools::scale_x_annual(
      name = "",
      limits = limits[["x"]])

  stopifnot(
    unique(chart_data$ems_unit) == "tons/yr")

  chart_y_scale <-
    ggtools::scale_y_emissions(
      c("tons/yr", "tons/day"),
      limits = limits[["y"]])

  chart_description <-
    labs(
      caption = glue::glue(caption, .sep = "\n"))

  chart_data <-
    chart_data %>%
    mutate_at(
      vars(year),
      ~ parse_number(as.character(.)))

  chart_object <-
    chart_data %>%
    ggplot() +
    theme_simple() +
    aes(x = year, y = ems_qty) +
    chart_x_scale +
    chart_y_scale +
    chart_description

  return(chart_object)

}

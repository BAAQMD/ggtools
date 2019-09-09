#' Use gg_chart() to plot tput_qty
#'
#' @note group your data first to alter its presentation
#'
#' @export
gg_throughput_chart <- function (chart_data, ...) {

  stopifnot("tput_qty" %in% names(chart_data))
  gg_annual_chart(chart_data, qty_var = "tput_qty", ...)

}

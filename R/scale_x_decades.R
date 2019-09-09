#' Scale for Years
#'
#' @param name (character) axis label
#' @param limits (numeric) axis range
#' @param breaks (numeric) see [ggplot2::scale_x_continuous()]
#'
#' @export
scale_x_decades <- function (name = "", ..., limits = c(1990, 2050), breaks = seq(1990, 2050, by = 20)) {
  ggplot2::scale_x_continuous(name = name, ..., limits = limits, breaks = breaks)
}


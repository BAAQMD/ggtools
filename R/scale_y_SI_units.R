#' Scale for Quantities
#'
#' So that every chart can have a simple Y scale fixed at 0 and with SI units.
#'
#' @param ... passed to [ggplot2::scale_y_continuous()]
#' @param limits (numeric) axis range
#' @param labels see [ggplot2::scale_y_continuous()]
#'
#' @export
scale_y_SI_units <- function(..., limits = c(0, NA), labels = format_SI) {
  ggplot2::scale_y_continuous(..., limits = limits, labels = labels)
}

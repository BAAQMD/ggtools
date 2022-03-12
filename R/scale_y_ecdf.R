#' scale_y_ecdf
#'
#' @param name
#' @param trans (character) useful choices are "logit" and "identity" (the default).
#' @param digits (numeric) passed to [scale_y_percentage()]
#' @param limits (numeric) passed to [scale_y_percentage()]
#' @param breaks (numeric) passed to [scale_y_percentage()]
#' @param expand (numeric) passed to [scale_y_percentage()]
#' @param ... (numeric) passed to [scale_y_percentage()]
#'
#' @export
scale_y_ecdf <- function (
  name = "Cumulative distribution",
  ...,
  trans = "identity",
  digits = 1,
  limits = c(0, 1),
  breaks = waiver(),
  expand = expansion(0, 0)
) {

  if (trans == "logit") {
    breaks <- c(0.001, 0.003, 0.01, 0.03, 0.1, 0.25, 0.5, 0.75, 0.9, 0.97, 0.99, 0.997, 0.999)
    limits <- c(0.001, 0.999)
    expand <- expansion(mult = c(0.001, 0.001))
    try(name <- str_c(name, " (logit scale)"))
  }

  ggtools::scale_y_percentage(
    name = name,
    ...,
    digits = digits,
    trans = trans,
    breaks = breaks,
    limits = limits,
    expand = expand)

}

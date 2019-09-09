#' @export
scale_y_percentage <- function (..., limits = c(0, 1), breaks = seq(0, 1, by = 0.2), expand = c(0, 0)) {
  scale_y_continuous(..., labels = format_percentage, limits = limits, breaks = breaks, expand = expand)
}

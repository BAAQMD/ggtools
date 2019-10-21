#' scale_y_percentage
#'
#' @export
scale_y_percentage <- function (
  ...,
  limits = c(0, 1),
  breaks = seq(-100, 100, by = 0.2),
  labels = format_percentage,
  expand = expand_scale(mult = 0, add = 0)
) {

  scale_y_continuous(
    ...,
    labels = labels,
    limits = limits,
    breaks = breaks,
    expand = expand)

}

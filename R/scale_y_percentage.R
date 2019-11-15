#' scale_y_percentage
#'
#' @export
scale_y_percentage <- function (
  ...,
  digits = 0,
  expand = c(0, 0)
) {

  scale_y_continuous(
    ...,
    labels = partial(format_percentage, digits = digits),
    expand = expand)

}

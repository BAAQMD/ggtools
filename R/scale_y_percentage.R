#' scale_y_percentage
#'
#' @export
scale_y_percentage <- function (
  ...,
  digits = 0,
  expand = c(0, 0),
  reverse = FALSE
) {

  if (isTRUE(reverse)) {
    label_fun <- function (x) {
      format_percentage(1 - x, digits = digits)
    }
  } else {
    label_fun <- function (x) {
      format_percentage(x, digits = digits)
    }
  }

  scale_y_continuous(
    ...,
    labels = label_fun,
    expand = expand)

}

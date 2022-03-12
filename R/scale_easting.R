#' scale_easting
#'
#' @param name character
#' @param ... passed to [ggplot2::scale_x_continuous()]
#' @param unit character, like "km"
#' @param expand like that returned by `ggplot2::expansion()`
#' @param labels function that generates labels
#'
#' @return ggplot2 scale object
#' @export
#'
scale_easting <- function (
  name = "Easting",
  ...,
  unit = "km",
  expand = ggplot2::expansion(mult = 0, add = 0),
  labels = purrr::partial(qtytools::convert_qty, from = "m", to = unit)
) {
  scale_x_continuous(name = glue::glue("{name} ({unit})"), ..., labels = labels, expand = expand)
}

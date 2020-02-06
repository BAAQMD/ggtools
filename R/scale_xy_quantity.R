#' @include scale_quantity.R

#' scale_xy_quantity
#'
#' @describeIn scale_quantity
#'
#' @usage scale_xy_quantity(...)
#'
#' @export
scale_xy_quantity <- function (name, ...) {
  list(
    scale_x_quantity(first(name), ...),
    scale_y_quantity(last(name), ...),
    coord_equal())
}

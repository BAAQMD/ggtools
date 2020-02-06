#' @include scale_quantity.R

#' scale_x_quantity
#'
#' @describeIn scale_quantity
#'
#' @export
scale_x_quantity <-
  purrr::partial(
    scale_quantity,
    dimension = "x")

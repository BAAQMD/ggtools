#' @include scale_quantity.R

#' scale_y_quantity
#'
#' @describeIn scale_quantity
#'
#' @export
scale_y_quantity <-
  purrr::partial(
    scale_quantity,
    dimension = "y")

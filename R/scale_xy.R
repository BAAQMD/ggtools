scale_xy <- function (..., scale = "continuous") {

  x_scale <- get(str_c("scale_x_", scale))
  y_scale <- get(str_c("scale_y_", scale))

  # can be safely added to a ggplot-style chain of `+`s
  list(x_scale(...), y_scale(...))

}

#' @export
scale_xy_continuous <- function (...) {
  scale_xy(..., scale = "continuous")
}

#' @export
scale_xy_discrete <- function (...) {
  scale_xy(..., scale = "discrete")
}

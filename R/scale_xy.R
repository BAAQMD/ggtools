scale_xy <- function (
  name = NULL,
  ...,
  scale = "continuous",
  ratio = 1
) {

  x_scale <- get(str_c("scale_x_", scale))
  y_scale <- get(str_c("scale_y_", scale))

  if (is.null(name)) {
    x_name <- waiver()
    y_name <- waiver()
  } else {
    x_name <- name[1]
    y_name <- name[length(name)]
  }

  # can be safely added to a ggplot-style chain of `+`s
  lst <- list(x_scale(x_name, ...), y_scale(y_name, ...))

  if (is.numeric(ratio)) {
    lst <- append(lst, coord_fixed(ratio = ratio))
  }

  return(lst)

}

#' @export
scale_xy_continuous <- function (
  ...,
  limits = c(0, NA),
  expand = ggplot2::expansion(mult = c(0, 0.1), add = c(0, 0))
) {
  scale_xy(
    ...,
    scale = "continuous",
    limits = limits,
    expand = expand)
}

#' @export
scale_xy_discrete <- function (
  ...
) {
  scale_xy(
    name,
    ...,
    scale = "discrete")
}

#' @export
scale_xy_log10 <- function (
  ...,
  labels = label_log10,
  expand = ggplot2::expansion(0, 0),
  logticks = TRUE
) {

  lst <-
    scale_xy(
      ...,
      labels = labels,
      expand = expand,
      scale = "log10")

  if (isTRUE(logticks)) {
    lst <- append(lst, annotation_logticks())
  }

  return(lst)

}

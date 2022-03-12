#' fct_swatches
#'
#' Generate colors for a given factor, or something coercible to one.
#' Useful in conjunction with [ggplot2::scale_color_manual()].
#'
#' @param x (factor) or character, convertible to factor
#' @param fun (function) like [ggplot2::scale_color_discrete()]
#' @param ... further arguments to `fun`
#'
#' @value named vector, with colors as values, and levels as names
#'
#' @export
#'
#' @example
#' df <- mutate(mtcars, carb = fct_other(factor(carb), keep = 1:2))
#' p <- ggplot(df, aes(wt, mpg)) + geom_point(aes(color = carb))
#' swatches <- replace(fct_swatches(p$data$carb), "Other", gray(0.5))
#' p + scale_color_manual(values = swatches)
fct_swatches <- function (x, fun = scale_color_tableau, ...) {
  fct <- factor(x)
  pal <- fun(...)$palette
  colors <- pal(length(levels(fct)))
  swatches <- setNames(colors, levels(fct))
  return(swatches)
}

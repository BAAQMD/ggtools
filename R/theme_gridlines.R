#' theme_gridlines
#'
#' Subtle gridlines for a ggplot object.
#' Useful in conjunction with [theme_simple()].
#'
#' @param linetype (character) passed to [ggplot2::element_line()]
#' @param color (character) passed to [ggplot2::element_line()]
#' @param ... further arguments to [ggplot2::element_line()]
#'
#' @export
theme_gridlines <- function (
  linetype = "dotted",
  color = gray(0.9),
  ...
) {

  line_element <-
    ggplot2::element_line(
      linetype = linetype,
      color = color,
      ...)

  theme_object <-
    ggplot2::theme(
      panel.grid.major.x = line_element,
      panel.grid.major.y = line_element)

  return(theme_object)

}

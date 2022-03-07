theme_gridlines <- function (linetype = "dotted", color = gray(0.9), ...) {
  line_element <- element_line(linetype = linetype, color = color, ...)
  theme(
    panel.grid.major.x = line_element,
    panel.grid.major.y = line_element)
}

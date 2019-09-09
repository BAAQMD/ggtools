#' Minimalist theme
#'
#' @export
theme_simple <- function (
  ...,
  legend.position = "right",
  verbose = TRUE
) {

  thin_line <-
    partial(element_line, color = gray(0.3), size = I(0.25))

  uniform_margin <- function (x, unit) {
    margin(x, x, x, x, unit = unit)
  }

  title_styling <-
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(
        color = gray(0.3),
        lineheight = rel(1.05),
        margin = margin(t = 3, r = 0, b = 15, l = 0, unit = "pt")),
      plot.caption = element_text(
        color = gray(0.6),
        hjust = 0,
        size = rel(0.75),
        margin = margin(t = 1.75, unit = "lines")))

  panel_styling <-
    theme(
      panel.border = element_blank(),
      panel.spacing.x = unit(1, "cm"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())

  axis_styling <-
    theme(
      axis.line = thin_line(),
      axis.ticks.x = thin_line(),
      axis.title = element_text(
        size = rel(0.9)),
      axis.title.y = element_text(
        margin = uniform_margin(10, "pt")),
      axis.title.y.right = element_text(
        margin = uniform_margin(10, "pt"),
        angle = 90))

  strip_styling <-
    theme(
      strip.text = element_text(
        color = "black",
        hjust = 0,
        face = "bold"),
      strip.background = element_blank())

  legend_styling <-
    theme(
      legend.position = legend.position,
      legend.title = element_text(
        face = "bold",
        size = rel(0.95)),
      legend.text = element_text(
        size = rel(0.95)))

  theme_parts <-
    list(
      theme_linedraw(...),
      title_styling,
      panel_styling,
      axis_styling,
      strip_styling,
      legend_styling)

  return(Reduce(`+`, theme_parts))

}

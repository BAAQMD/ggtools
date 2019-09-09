#' @export
gg_theme_minimalist <- function (
  plot.title = element_text(face = "bold"),
  plot.subtitle = element_text(margin = margin(b = 24, unit = "pt")),
  plot.caption = element_text(size = rel(10/12), margin = margin(t = 12, b = 12), face = "italic", color = gray(0.5)),
  plot.margin = margin(t = 1, b = 1, l = 1.5, r = 1.5, unit = "cm"),
  ...
) {

  theme_minimal() + theme(
    plot.title = plot.title,
    plot.subtitle = plot.subtitle,
    plot.caption = plot.caption,
    plot.margin = plot.margin,
    ...)

}

#' @export
gg_theme_inventory <- function (x_axis = NULL, ...) {

  base_properties <-
    theme_minimal(...)

  legend_properties <- theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.key.height = unit(9, "pt"),
    legend.spacing.y = unit(3, "pt"),
    legend.box.margin = margin(t = 0, b = 0, unit = "pt"),
    legend.margin = margin(t = 0, b = 3, unit = "pt"),
    legend.title = element_text(size = rel(10/12), face = "bold", margin = margin(t = 0, b = 0, unit = "pt")))

  title_properties <- theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 24, unit = "pt")),
    plot.caption = element_text(size = rel(10/12), margin = margin(t = 12, b = 12), face = "italic", color = gray(0.5)))

  axis_properties <-  theme(
    #axis.line = element_line(color = gray(0.3), size = 1),
    axis.text.x = element_text(margin = margin(t = 3, b = 6, unit = "pt")),
    axis.text.y = element_text(margin = margin(l = 3, unit = "pt")),
    axis.title = element_text(size = rel(10/12)))

  #if (is.null(x_axis)) {
  #  axis_properties <- axis_properties + theme(axis.title.x = element_blank())
  #}

  misc_properties <- theme(
    #panel.grid.major = element_line(color = gray(0.8), size = 1),
    #panel.grid.minor = element_line(color = gray(0.8), size = 0.5),
    strip.text.x = element_text(face = "bold", hjust = 0),
    plot.margin = margin(t = 12, b = 12, l = 12, r = 24, unit = "pt"))

  theme_properties <-
    base_properties + legend_properties + title_properties +
    axis_properties + misc_properties

  list(theme_properties,
       ggthemes::scale_color_colorblind(),
       ggthemes::scale_fill_colorblind())

}

#' @noRd
gg_plot <- function (
  data = NULL,
  mapping = aes(),
  ...,
  theme = "minimal",
  verbose = getOption("verbose")
) {

  ggplot(data, mapping, ...) +
    gg_theme_inventory()

}

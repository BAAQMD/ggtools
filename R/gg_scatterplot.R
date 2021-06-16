#'
#' @importFrom ggplot2 coord_equal geom_abline
#'
#' @export
gg_scatterplot <- function (
  data,
  mapping = aes(),
  geom = "point",
  alpha = 0.5,
  breaks = NULL,
  limits = c(0, NA),
  scale = "continuous",
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[gg_scatterplot] ", ...)

  if (is.null(breaks)) {
    msg("setting breaks to waiver()")
    breaks <- waiver()
  } else {
    break_width <- unique(diff(breaks)) %>% ensurer::ensure(length(.) == 1)
    limits <- range(breaks) + c(-0.5, 0.5) * break_width
    msg("setting limits to: ", limits)
  }

  # make `geom` into a function
  if (is.null(geom) | is.na(geom) ) {
    geom_fun <- function () list()
  } else if (is.character(geom)) {
    geom_fun <- get(str_c("geom_", geom))
  } else {
    stop("don't know to handle that `geom` argument")
  }

  plot_obj <- gg_plot(data, mapping)
  plot_data <- plot_obj$data

  if (missing(limits)) {
    plot_aes <- plot_obj$mapping
    x_max <- max(plot_data[[rlang::quo_squash(plot_aes$x)]], na.rm = TRUE)
    y_max <- max(plot_data[[rlang::quo_squash(plot_aes$y)]], na.rm = TRUE)
    xy_max <- max(x_max, y_max)
    limits <- c(0, xy_max)
    msg("auto-assigning limits: c(", str_csv(limits), ")")
  }

  msg("scale is: ", scale)

  plot_obj +
    geom_abline(color = gray(0.9), size = 0.25) +
    scale_xy(breaks = breaks, limits = limits, scale = scale) +
    coord_equal() +
    geom_fun(aes(alpha = I(alpha)))

}

################################################################################

#' Use ggplot2 to plot X vs Y
#'
#' @export
gg_scatterplot_linear <- function (...) {
  gg_scatterplot(..., scale = "continuous")
}

#' Use ggplot2 to plot X vs Y, on a log scale
#'
#' @export
gg_scatterplot_log10 <- function (...) {
  gg_scatterplot(..., scale = "log10")
}

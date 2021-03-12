#' scale_log10
#'
#' log10 scales with pretty axis ticks and breaks
#'
#' @param ... as in [ggplot2::scale_x_log10()] or [ggplot2::scale_y_log10()]
#' @param sides as in [ggplot2::annotation_logticks()]
#'
#' @importFrom ggplot2 scale_x_log10 scale_y_log10 annotation_logticks waiver expansion
#'
#' @noRd
scale_log10 <- function (
  axis,
  ...,
  sides = NULL
) {

  if (axis == "x") {
    ggplot_scale <- ggplot2::scale_x_log10
    if (is.null(sides)) sides = "b"
  } else if (axis == "y") {
    ggplot_scale <- ggplot2::scale_y_log10
    if (is.null(sides)) sides = "l"
  } else {
    stop("axis must be \"x\" or \"y\"")
  }

  f <- function (
    ...,
    limits = waiver(),
    labels = label_log10,
    expand = expansion(mult = c(0.01, 0.01)),
    verbose = getOption("verbose", default = FALSE)
  ) {

    msg <- function (...) if(isTRUE(verbose)) message("[scale_y_log10] ", ...)

    log10_breaks <- seq(
      floor(min(log10(limits))),
      ceiling(max(log10(limits))),
      by = 1)

    minor_breaks <-
      as.numeric(c(1:9) %o% (10 ^ log10_breaks))

    scale_object <-
      ggplot_scale(
        ...,
        limits = limits,
        breaks = 10 ^ log10_breaks,
        minor_breaks = minor_breaks,
        labels = labels)

    annotation_object <-
      ggplot2::annotation_logticks(
        sides = sides)

    return(list(scale_object, annotation_object))

  }

  return(f)

}

#' scale_x_log10
#'
#' @usage scale_x_log10(...)
#' @describeIn scale_log10 x scale
#' @export
scale_x_log10 <- scale_log10(axis = "x")

#' scale_y_log10
#'
#' @usage scale_y_log10(...)
#' @describeIn scale_log10 y scale
#' @export
scale_y_log10 <- scale_log10(axis = "y")

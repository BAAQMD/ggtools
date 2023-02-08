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
  sides = NULL,
  ticks = TRUE
) {

  if (axis == "x") {
    scale_fun <- ggplot2::scale_x_log10
    if (is.null(sides)) sides = "b"
  } else if (axis == "y") {
    scale_fun <- ggplot2::scale_y_log10
    if (is.null(sides)) sides = "l"
  } else {
    stop("axis must be \"x\" or \"y\"")
  }

  f <- function (
    ...,
    limits = waiver(),
    breaks = scales::breaks_log(base = 10),
    minor_breaks = waiver(),
    labels = label_log10,
    expand = expansion(mult = c(0.01, 0.01)),
    outside = FALSE,
    scaled = TRUE,
    short = unit(0.1, "cm"),
    mid = unit(0.2, "cm"),
    long = unit(0.3, "cm"),
    color = "black",
    size = 0.5,
    linetype = 1,
    alpha = 1,
    ticks = TRUE,
    verbose = getOption("verbose", default = FALSE)
  ) {

    msg <- function (...) if(isTRUE(verbose)) message("[scale_y_log10] ", ...)

    # if (inherits(breaks, "waiver")) {
    #
    #   breaks_min <- floor(min(log10(limits)))
    #   breaks_max <- ceiling(max(log10(limits)))
    #   breaks <- seq(breaks_min, breaks_max, by = 1)
    #
    #   if (inherits(minor_breaks, "waiver")) {
    #     minor_breaks <- as.numeric(outer(1:9, 10 ^ breaks))
    #   }
    #
    # }

    scale_object <-
      scale_fun(
        ...,
        limits = limits,
        expand = expand,
        breaks = breaks,
        minor_breaks = minor_breaks,
        labels = labels)

    retval <- list(scale_object)

    if (isTRUE(ticks)) {

      ticks_object <-
        ggplot2::annotation_logticks(
          sides = sides,
          outside = outside,
          scaled = scaled,
          short = short,
          mid = mid,
          long = long,
          color = color,
          size = size,
          linetype = linetype,
          alpha = alpha)

      retval <- append(retval, list(ticks_object))

    }

    return(retval)

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

#' log10 scale together with axis ticks and breaks
#'
#' @param ... passed to [ggplot2::scale_x_log10()]
#' @param labels function
#'
#' @seealso [scale_xy_log10()]
#'
#' @export
scale_x_log10 <- function (
  ...,
  limits = waiver(),
  labels = ggtools::label_log10,
  expand = expansion(mult = c(0.01, 0.01)),
  verbose = getOption("verbose", default = FALSE)
) {

  msg <- function (...) if(isTRUE(verbose)) message("[scale_x_log10] ", ...)

  log10_breaks <- seq(
    floor(min(log10(limits))),
    ceiling(max(log10(limits))),
    by = 1)

  minor_breaks <-
    as.numeric(c(1:9) %o% (10 ^ log10_breaks))

  scale_object <-
    ggplot2::scale_x_log10(
      ...,
      limits = limits,
      breaks = 10 ^ log10_breaks,
      minor_breaks = minor_breaks,
      labels = labels)

  annotation_object <-
    ggplot2::annotation_logticks(
      sides = "b")

  return(list(scale_object, annotation_object))

}

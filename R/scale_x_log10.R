#' log10 scale together with axis ticks
#'
#' @param ... passed to [ggplot2::scale_x_log10()]
#' @param labels function
#'
#' @seealso [scale_xy_log10()]
#'
#' @export
scale_x_log10 <- function (
  ...,
  labels = format_log10
) {

  scale_object <-
    ggplot2::scale_x_log10(
      ...,
      labels = labels)

  annotation_object <-
    ggplot2::annotation_logticks(
      sides = "b")

  return(list(scale_object, annotation_object))

}

#' scale_extent
#'
#' @param object `sf`, [raster::Extent], or [terra::SpatExtent] object
#' @param ... passed to [scale_easting()] and [scale_northing()]
#' @param verbose (logical)
#'
#' @importFrom sf st_bbox
#' @importFrom ggplot2 waiver
#'
#' @export
scale_extent <- function (
  object,
  ...,
  verbose = getOption("verbose", default = TRUE)
) {

  msg <- function (...) if(isTRUE(verbose)) message("[scale_extent] ", ...)

  if (is.null(object)) {
    return(NULL)
  } else if (inherits(object, "sf")) {
    bb <- sf::st_bbox(object)
    x_limits <- c(bb["xmin"], bb["xmax"])
    y_limits <- c(bb["ymin"], bb["ymax"])
  } else if (inherits(object, "Extent")) {
    x_limits <- c(attr(object, "xmin"), attr(object, "xmax"))
    y_limits <- c(attr(object, "ymin"), attr(object, "ymax"))
  } else if (inherits(object, "SpatExtent")) {
    x_limits <- c(object$xmin, object$xmax)
    y_limits <- c(object$ymin, object$ymax)
  } else {
    x_limits <- ggplot2::waiver()
    y_limits <- ggplot2::waiver()
  }

  msg("x_limits is: ", str_csv(x_limits))
  msg("y_limits is: ", str_csv(y_limits))

  list(
    scale_easting(limits = x_limits, ...),
    scale_northing(limits = y_limits, ...))

}

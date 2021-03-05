#' size_as_mm
#'
#' @param size
#'
#' @return
#' @export
#'
size_as_mm <- function (size) {
  return(size * ggplot2:::.pt)
}

#' size_as_mm
#'
#' @param size
#'
#' @return
#' @export
#'
#' @examples
size_as_mm <- function (size) {
  return(size * ggplot2:::.pt)
}

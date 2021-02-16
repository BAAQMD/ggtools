#' scale2palette
#'
#' Extracts color values from a ggplot2 scale
#'
#' @param scale a ggplot2 scale object
#'
#' @return character
#'
#' @note Used by [scale_sepia_fill()] and [scale_sepia_color()]
#'
#' @export
scale2palette <- function (scale) {
  pal <- scale$palette
  function (n) {
    swatches <- pal(seq(0, 1, length.out = n))
    return(swatches)
  }
}

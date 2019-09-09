#' scale_fill_cubehelix
#' 
#' @details
#' starting hue (here, `start`) is:
#' - `start` in `seaborn.cubehelix_palette` 
#' - `h` in `chroma::cubehelix_map`
#'
#' chromaticity is: 
#' - `hue` in `seaborn.cubehelix_palette`
#' - `c` in `chroma::cubehelix_map` (chromaticity)
#' 
#' lightness and darkness:
#' - valid range is [0, 1] in both.
#' 
#' gamma:
#' - valid range is [0, Inf) in both
#' - default is 1.0
#' - sane values seem to be in [0.5, 3] (whitened, darkened)
#'
#' @export
scale_fill_cubehelix <- function (
  ..., 
  start = 0, 
  rot = 0.4, 
  gamma = 1.0, 
  chrom = 0.8, 
  light = 0.85, 
  dark = 0.15,
  guide = "colorbar"
) {
  
  require(ggplot2)
  
  chroma_cubehelix_scale <- 
    chroma::cubehelix_scale(
      h = start, 
      rot = rot, 
      c = chrom, 
      l = c(light, dark), 
      gamma = gamma)
  
  scale_object <-
    ggplot2::continuous_scale(
      "fill", 
      "cubehelix", 
      chroma_cubehelix_scales,
      guide = guide, ...)
  
  return(scale_object)
  
  
}

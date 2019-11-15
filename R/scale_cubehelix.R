#' scale_cubehelix
#'
#' @param name as in `ggplot2::continuous_scale()`
#' @param start see Details
#' @param rot see Details
#' @param gamma see Details
#' @param light see Details
#' @param dark see Details
#' @param ... further arguments to `ggplot2::continuous_scale()`
#'
#' @seealso
#' - `chroma::cubehelix_scale()`
#' - `chroma::cubehelix_map()`
#'
#' @details
#' starting hue (here, `start`) is:
#' - `start` in `seaborn.cubehelix_palette`
#' - `h` in `chroma::cubehelix_map()`
#'
#' chromaticity is:
#' - `hue` in `seaborn.cubehelix_palette`
#' - `c` in `chroma::cubehelix_map()`
#'
#' lightness and darkness:
#' - valid range is [0, 1] in both.
#'
#' gamma:
#' - valid range is [0, Inf) in both
#' - default is 1.0
#' - sane values seem to be in [0.5, 3] (whitened, darkened)
#'
scale_cubehelix <- function (
  aesthetics,
  ...,
  start = 0,
  rot = 0.4,
  gamma = 1.0,
  chrom = 0.8,
  light = 0.85,
  dark = 0.15
) {

  cubehelix_palette <-
    chroma::cubehelix_scale(
      h = start,
      rot = rot,
      c = chrom,
      l = c(light, dark),
      gamma = gamma)

  scale_object <-
    ggplot2::continuous_scale(
      ...,
      aesthetics = aesthetics,
      scale_name = "cubehelix",
      palette = cubehelix_palette)

  return(scale_object)

}

#' scale_fill_cubehelix
#'
#' @rdname scale_cubehelix
#'
#' @export
scale_fill_cubehelix <-
  purrr::partial(
    scale_cubehelix,
    aesthetics = "fill")

#' scale_color_cubehelix
#'
#' @rdname scale_cubehelix
#'
#' @export
scale_color_cubehelix <-
  purrr::partial(
    scale_cubehelix,
    aesthetics = "color")

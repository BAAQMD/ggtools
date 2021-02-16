#' Continuous scales for air quality
#'
#' Built to resemble ArcGIS-style "pollution" color ramps.
#'
#' @param guide as in [ggplot2::continuous_scale()]
#' @param start as in [chroma::cubehelix_scale()]
#' @param rot as in [chroma::cubehelix_scale()]
#' @param chrom  as in [chroma::cubehelix_scale()]
#' @param light  as in [chroma::cubehelix_scale()]
#' @param dark  as in [chroma::cubehelix_scale()]
#' @param gamma  as in [chroma::cubehelix_scale()]
#' @param na.value color to use for `NA` values (default transparent, but may not work)
#' @param ... passed to [ggplot2::continuous_scale()]
#' @param aesthetics for internal use
#'
#' @aliases scale_fill_sepia scale_color_sepia
#'
#' @export
scale_sepia <- function (
  ...,
  guide = "colorbar",
  start = 48,
  rot = -0.14,
  chrom = 0.95,
  light = 0.97,
  dark = 0.1,
  gamma = 0.8,
  na.value = "#00000000",
  aesthetics = c("fill", "color")
) {

  aesthetics <-
    match.arg(aesthetics)

  sepia_palette <-
    chroma::cubehelix_scale(
      h = start,
      rot = rot,
      c = chrom,
      l = c(light, dark),
      gamma = gamma,
      na.value = na.value)

  scale_object <-
    ggplot2::continuous_scale(
      ...,
      aesthetics = aesthetics,
      scale_name = "cubehelix",
      palette = sepia_palette,
      guide = guide,
      na.value = na.value)

  return(scale_object)

}

#' @describeIn scale_sepia
#' @export
scale_fill_sepia <-
  purrr::partial(
    scale_sepia,
    aesthetics = "fill")

#' @describeIn scale_sepia
#' @export
scale_color_sepia <-
  purrr::partial(
    scale_sepia,
    aesthetics = "color")

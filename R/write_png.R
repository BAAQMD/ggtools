#' @export
write_png <- function (
  chart_object,
  path,
  width = 15,
  height = NULL,
  aspect = 1920 / 1080,
  dpi = 300,
  ...,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[write_png] ", ...)

  stopifnot(
    inherits(chart_object, "ggplot"))

  stopifnot(
    str_detect(path, regex("png$", ignore_case = TRUE)))

  if (is.null(height)) {
    if (is.null(aspect)) {
      stop("must supply either `height` or `aspect`")
    } else {
      height <- width / aspect
    }
  }

  path <- glue::glue(path)
  msg("writing to ", path)

  ggplot2::ggsave(
    filename = path,
    plot = chart_object,
    width = width,
    height = height,
    dpi = dpi,
    ...)

  return(invisible(chart_object))

}

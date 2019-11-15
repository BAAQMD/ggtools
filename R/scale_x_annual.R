#' Annual scale for CY, RY, PY, etc.
#'
#' @export
scale_x_annual <- function (
  name = NULL,
  prefix = "",
  breaks = seq(1990, 2050, by = 10),
  labels = function (x) str_extract(x, "[0-9]{4}"),
  limits = waiver(),
  expand = expand_scale(add = c(0, 1)),
  ...,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[scale_x_annual] ", ...)

  stopifnot(is.function(labels))

  if (!is.null(prefix)) {
    f <- function (x) str_c(prefix, x)
    if (is.function(labels)) {
      labels <- compose(f, labels)
    } else if (is.character(prefix)) {
      labels <- f(labels)
    }
  }

  if (missing(limits)) {

    scale_x_continuous(
      name = name,
      breaks = breaks,
      labels = labels,
      expand = expand,
      ...)

  } else {

    scale_x_continuous(
      name = name,
      breaks = breaks,
      labels = labels,
      limits = limits,
      expand = expand,
      ...)

  }

}

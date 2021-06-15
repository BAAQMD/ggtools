#' Annual scale for CY, RY, PY, etc.
#'
#' @importFrom ggplot2 scale_x_continuous
#'
#' @export
scale_x_annual <- function (
  name = NULL,
  prefix = "",
  breaks = seq(1990, 2050, by = 10),
  labels = function (x) str_extract(x, "[0-9]{4}"),
  limits = NULL,
  expand = NULL,
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

  if (is.null(expand)) {
    expand <- ggplot2::expansion(mult = 0, add = 0.25)
  }

  if (is.null(limits)) {

    scale_x_continuous(
      name = name,
      breaks = breaks,
      labels = labels,
      expand = expand,
      ...)

  } else {

    expand_mult <- expand[c(1, 3)] * c(-1, 1) # left and right, respectively
    expand_add  <- expand[c(2, 4)] * c(-1, 1) # left and right, respectively

    limits <- as.numeric(yeartools::elide_year(limits))
    limits <- limits + c(-0.5, 0.5)
    msg("adjusted `limits` is: ", str_csv(limits))

    scale_x_continuous(
      name = name,
      breaks = breaks,
      labels = labels,
      limits = limits,
      expand = expand,
      ...)

  }

}

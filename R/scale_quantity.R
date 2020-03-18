#' scale_quantity
#'
#' Position scales for quantities (x & y)
#'
#' @param name (character) axis label
#' @param expand passed to `scale_continuous()`
#' @param limits passed to `scale_continuous()`
#' @param unit (character) passed to `format_qty()`
#' @param labels passed to `scale_continuous()`
#' @param format passed to `scale_continuous()`
#' @param ... passed to `scale_continuous()`
#' @param verbose (logical) display messages
#'
scale_quantity <- function (
  name = NULL,
  dimension = c("x", "y"),
  expand = ggplot2::expansion(mult = c(0, 0.1)),
  limits = c(0, NA),
  unit = NULL,
  labels = NULL,
  format = NULL,
  ...,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[scale_quantity] ", ...)

  if (is.null(labels)) {
    msg("labels defauting to format_qty")
    labels <- format_qty
  }

  dimension <- match.arg(dimension)
  scale_function_name <- str_c("scale_", dimension, "_continuous")
  scale_function <- get(scale_function_name)
  scale_object <- scale_function(
    name = name,
    expand = expand,
    labels = labels,
    limits = limits,
    ...)

  return(scale_object)

}

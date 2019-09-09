scale_quantity <- function (
  name = NULL,
  dimension = c("x", "y"),
  expand = expand_scale(mult = c(0, 0.1)),
  limits = c(0, NA),
  unit = NULL,
  labels = NULL,
  format = NULL,
  ...,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[scale_quantity] ", ...)

  if (is.null(labels)) {
    msg("labels is NULL; using format ", format)
    labels <- partial(format_qty, format = format, unit = unit)
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

#' @export
scale_y_quantity <-
  purrr::partial(
    scale_quantity,
    dimension = "y")

#' @export
scale_x_quantity <-
  purrr::partial(
    scale_quantity,
    dimension = "x")

#' @export
scale_xy_quantity <- function (name, ...) {
  list(
    scale_x_quantity(first(name), ...),
    scale_y_quantity(last(name), ...),
    coord_equal())
}

#' #' @export
#' scale_y_quantity_SI <- function (
#'   name = "",
#'   expand = expand_scale(mult = c(0, 0.1)),
#'   digits = 1,
#'   unit = NULL,
#'   labels = NULL,
#'   concise = TRUE,
#'   ...,
#'   verbose = getOption("verbose")
#' ) {
#'
#'   msg <- function (...) if(isTRUE(verbose)) message("[scale_y_quantity_SI] ", ...)
#'
#'   if (is.null(labels)) {
#'
#'     # Default formatter
#'     # (adaptation of format_SI)
#'     labels <- function (x, ...) {
#'
#'       SI_breaks <- 10 ** seq(-24, 24, by = 3)
#'       SI_prefixes <- c("y", "z", "a", "f", "p", "n", "µ", "m", "",
#'                     "k", "M", "G", "T", "P", "E", "Z", "Y")
#'
#'       # Vector with array indices according to position in intervals
#'       i <- findInterval(abs(x), SI_breaks)
#'
#'       #if (diff(range(i, na.rm = TRUE)) > 3) {
#'       #  warn_msg <- str_c("[format_qty] values span > 3 orders of magnitude; consider using `format_SI`")
#'       #  warning(warn_msg)
#'       #}
#'
#'       max_i <- max(i, na.rm = TRUE)
#'       max_prefix <- SI_prefixes[max_i]
#'
#'       rounded <- round(x / SI_breaks[max_i], digits = digits)
#'
#'       formatted <- paste0(
#'         str_trim(format(rounded, trim = TRUE, scientific = FALSE, ...)),
#'         max_prefix)
#'
#'       message("formatted: ", str_csv(formatted))
#'
#'       if (!is.null(unit)) {
#'         i <- which(!is.na(x))
#'         formatted[last(i)] <- str_c("\n", last(formatted[i]), "\n", unit)
#'       }
#'
#'       if (isTRUE(concise)) {
#'         formatted <- c(first(formatted),
#'                        rep("", length(formatted) - 2),
#'                        last(formatted))
#'       }
#'
#'       return(formatted)
#'
#'     }
#'
#'   }
#'
#'   scale_y_continuous(
#'     name = name,
#'     labels = labels,
#'     expand = expand,
#'     ...)
#'
#' }

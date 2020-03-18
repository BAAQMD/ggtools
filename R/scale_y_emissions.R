#' scale_y_emissions
#'
#' @export
scale_y_emissions <- function (
  name = c("tons/yr", "tons/day"),
  labels = NULL,
  expand = ggplot2::expansion(mult = c(0, 0.1)),
  limits = c(0, NA),
  ...,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[scale_y_emissions] ", ...)

  if (is.null(labels)) {

    msg("labels defaulting to format_qty(fixed = TRUE, engineering = TRUE)")

    labels <-
      purrr::partial(
        strtools::format_qty,
        fixed = TRUE,
        engineering = TRUE)
  }

  if (length(name) == 2) {

    convert_quantities <- function (x, from, to) {
      require(units)
      converted <- set_units(set_units(x, from, mode = "standard"), to, mode = "standard")
      as.numeric(converted)
    }

    trans_formula <-
      ~ convert_quantities(., from = name[1], to = name[2])

    secondary_y_axis <-
      sec_axis(
        name = last(name),
        labels = labels,
        trans = trans_formula)

  } else if (length(name) == 1) {

    secondary_y_axis = waiver()

  } else if(is.null(name)) {

    # pass

  } else {

    stop("`name` must be either NULL or a length 1 or 2 character vector")

  }

  scale_object <-
    scale_y_continuous(
      name = first(name),
      labels = labels,
      expand = expand,
      limits = limits,
      sec.axis = secondary_y_axis,
      ...)

}

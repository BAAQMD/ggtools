#' Google-style barchart
#'
#' @note REQUIRES AN INTERNET CONNECTION to display the visualization
#'
#' @param input_data data frame
#' @param ... arguments to \link{gvisPieChart}[googleVis]
#' @param height in pixels
#' @param width in pixels
#'
#' @importFrom googleVis gvisBarChart
#' @importFrom jsonlite toJSON
#'
#' @export
#'
google_barchart <- function (
  input_data,
  caption = comment(input_data),
  ...,
  stacked = TRUE,
  width = 800,
  height = 600,
  h_axis = list(),
  chart_area = list(left = 100, width = "60%"),
  plot = TRUE,
  verbose = FALSE
) {

  # Safety check --- don't allow barcharts spanning > 1 year of data
  if ("year" %in% names(input_data)) {
    if(!all_same(input_data$year)) warning("More than one distinct `year` in data")
  }

  ems_unit <- unique(input_data$ems_unit)
  stopifnot(length(ems_unit) == 1)

  chart_data <- dplyr::select(input_data, -year, -ems_unit)

  # TODO: make this more robust (what if there is only 1 group var??)
  label_var <- group_vars(chart_data)[[1]] %or% names(chart_data)[[1]]
  stack_var <- group_vars(chart_data)[[2]] %or% names(chart_data)[[2]]

  if (is.null(stack_var)) stacked <- FALSE

  jsonify <- . %>% jsonlite::toJSON(auto_unbox = TRUE)
  jsonified_options <- lapply(list(...), jsonify)

  if (identical(h_axis, list())) {
    if (isTRUE(stacked)) { # but not if it is "percent"
      h_axis <- update(h_axis, title = ems_unit, format = "short")
    } else if (stacked == "percent") {
      h_axis <- update(h_axis, format = "percent")
    }
  }

  chart_options <- append(
    jsonified_options,
    list(width = width,
         height = height,
         hAxis = jsonify(h_axis),
         chartArea = jsonify(chart_area),
         isStacked = stacked,
         reverseCategories = TRUE,
         title = caption))

  stopifnot(is.list(chart_options))
  if (verbose) print(chart_options)

  if (isTRUE(stacked) || stacked == "percent") {
    chart_data <- spread_(chart_data, stack_var, "ems_qty")
  }

  chart_obj <- googleVis::gvisBarChart(chart_data, options = chart_options)

  # Return result
  if (isTRUE(plot)) graphics::plot(chart_obj) else chart_obj

}

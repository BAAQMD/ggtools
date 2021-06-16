#' Google-style piechart
#'
#' @note REQUIRES AN INTERNET CONNECTION to display the visualization
#'
#' @param input_data data frame
#' @param ... arguments to \link{gvisPieChart}[googleVis]
#' @param height in pixels
#' @param width in pixels
#'
#' @importFrom googleVis gvisPieChart
#' @importFrom jsonlite toJSON
#' @importFrom tbltools print_tbl paste_csv
#' @importFrom dplyr select_ arrange mutate_at
#'
#' @export
#'
#' @examples
#' library(inventory)
#' BY2011_NOx_2015 <- BY2011_annual %>% filter(year == 2015, pol_abbr == "NOx")
#' BY2011_NOx_2015_by_cnty <- BY2011_NOx %>% annual_emissions_by(cnty_abbr)
#' BY2011_NOx_2015_by_cnty %>% dplyr::select(cnty_abbr, ems_qty) %>% arrange(ems_qty) %>% google_piechart(caption = "2015 NOx emissions (Source: BY2011)")
google_piechart <- function (input_data, caption, colors = NULL, ..., width = 800, height = 600, plot = TRUE, verbose = FALSE) {

  # Safety check --- don't allow piecharts spanning > 1 year of data
  if ("year" %in% names(input_data)) {
    if(!all_same(input_data$year)) stop("More than one distinct `year` in data")
  }

  label_var <- group_var(input_data)
  if (is.null(label_var) || is.ordered(input_data[[label_var]])) {
    chart_data <- input_data
  } else {
    chart_data <-
      ungroup(total_emissions(input_data)) %>%
      select_(label_var, "ems_qty") %>%
      arrange(ems_qty)
  }

  jsonify <- jsonlite::toJSON
  jsonified_options <- lapply(list(...), jsonify)

  chart_options <- c(
    jsonified_options,
    reverseCategories = TRUE,
    width = width, height = height,
    title = caption)

  if (length(colors) == 1) {
    group_names <- unique(chart_data[[label_var]])
    chart_colors <- rev(monochrome(first(colors), color_names = group_names))
  } else {
    chart_colors <- colors
  }

  if (!is.null(chart_colors)) {
    if (verbose) message("Colors: ", paste_csv(chart_colors))
    chart_options$colors <- jsonify(chart_colors)
  }

  if (verbose) {
    chart_tbl <- chart_data %>% print_tbl("Piechart data.", column_totals = TRUE)
    print(chart_tbl)
    print(chart_options)
  }

  chart_obj <- googleVis::gvisPieChart(
    chart_data, options = chart_options)

  # Return result
  if (isTRUE(plot)) graphics::plot(chart_obj) else chart_obj

}

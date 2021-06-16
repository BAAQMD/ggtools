#' @rdname gg_chart
#'
#' @examples
#' test_yrs <- RY(2014:2016)
#' chart_data <- data_frame(year = test_yrs, ems_qty = seq(100, 200, length.out = length(test_yrs)), ems_unit = "tons/yr")
#' gg_annual_chart(chart_data) + geom_point()
#'
#' @importFrom ggplot2 facet_wrap aes_
#'
#' @export
gg_annual_chart <- function (chart_data, qty_var = NULL, format = NULL, ..., verbose = TRUE) {

  msg <- function (...) if (isTRUE(verbose)) message("[gg_annual_chart] ", ...)

  if ("cat_id" %in% names(chart_data)) {
    cat_ids <- chart_data[["cat_id"]]
    format_id <- function (x) str_c("#", x)
    chart_data[["cat_id"]] <- factor(format_id(cat_ids), levels = format_id(sort(unique(cat_ids))))
  }

  if (is.null(qty_var)) {

    qty_var <-
      vartools::find_qty_var(
        chart_data,
        verbose = verbose)

    msg("qty_var is: ", qty_var)

  }

  unit_var <-
    str_remove_all(qty_var, "_qty") %>%
    str_c("_unit")

  year_var <-
    select_vars(names(chart_data), "year")

  year_prefix <- local({
    prefix_pattern <- "^[A-Z]Y"
    prefixes <- str_extract(pull(chart_data, year_var), prefix_pattern)
    if (all_same(prefixes)) {
      unique(prefixes)
    } else {
      NULL
    }
  })

  if ("pol_abbr" %in% names(chart_data)) {
    chart_data <- chart_data %>% group_by(pol_abbr, add = TRUE)
  }

  chart_base <- ggplot(chart_data)

  y_unit <-
    unique(chart_data[[unit_var]]) %>%
    ensurer::ensure(length(.) == 1) %>%
    str_remove_all("/yr$")

  if ("pol_abbr" %in% names(chart_data)) {
    chart_base <-
      chart_base +
      facet_wrap(~ pol_abbr, scales = "free_y")
  }

  chart_theme <-
    theme(strip.text.x = element_text(hjust = 0, face = "bold"),
          strip.background = element_blank())

  #year_breaks <- local({
  #  year_lvls <- sort(unique(parse_number(pull(chart_data, year_var))))
  #  scales::cbreaks(range(year_lvls), breaks = year_lvls, labels = sprintf("%s%04d", year_prefix, as.integer(year_lvls)))
  #})

  chart_obj <-
    chart_base +
    aes_(x = as.formula(str_c("~ parse_number(", year_var, ")"))) +
    aes_(y = as.name(qty_var)) +
    scale_x_annual(prefix = year_prefix, labels = parse_number) +
    #scale_x_annual(breaks = year_breaks) +
    scale_y_quantity(unit = y_unit, format = format) +
    chart_theme

}


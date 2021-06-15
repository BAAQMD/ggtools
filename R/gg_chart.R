#' Use ggplot2 to plot some quantity by year
#'
#' @description Use gg_annual_chart(), not gg_chart() (the latter is deprecated).
#' You must add `geom_area(...)` or `geom_line(...)` to the result. See example below.
#'
#' @examples
#' library(inventory)
#'
#' airport_cats <-
#'   tibble(cat_id = 441:708)
#'
#' archived_emissions <-
#'   BY2011_annual %>%
#'   semi_join(airport_cats)
#'
#' computed_emissions <-
#'   BY(2011) %>%
#'   area_source_projections() %>%
#'   semi_join(airport_cats) %>%
#'   speciate_PM() %>%
#'   speciate_TOG()
#'
#' chart1_data <-
#'   archived_emissions %>%
#'   annual_emissions_by(cnty_abbr, pol_abbr, cat_id)
#'
#' chart1_labels <- labs(
#'   title = "Airport Emissions",
#'   subtitle = "As archived",
#'   caption = "Basis: BY2011_annual")
#'
#' chart1_data %>%
#'   group_by(cnty_abbr, cat_id) %>%
#'   gg_annual_chart() %>%
#'   add(geom_area(aes(fill = cnty_abbr))) %>%
#'   add(chart1_labels) %>%
#'   show()
#'
#' chart2_data <-
#'   computed_emissions %>%
#'   arrange(cnty_abbr) %>%
#'   annual_emissions_by(cnty_abbr, cat_id, pol_abbr)
#'
#' chart2_labels <- labs(
#'   title = "Airport Emissions",
#'   subtitle = "As computed today",
#'   caption = "Basis: BY(2011) %>% area_source_projections()")
#'
#' chart2_data  %>%
#'   group_by(cnty_abbr, cat_id) %>%
#'   gg_annual_chart() %>%
#'   add(geom_area(aes(fill = cnty_abbr))) %>%
#'   add(chart2_labels) %>%
#'   show()
#'
gg_chart <- function (chart_data, ..., verbose = TRUE) {
  .Deprecated("gg_annual_chart")
  gg_annual_chart(chart_data, qty_var = NULL, ..., verbose = verbose)
}

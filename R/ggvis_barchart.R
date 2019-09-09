#' ggvis-style barchart
#'
#' @param input_data data frame
#' @param ... further arguments to \link{ggvis}[ggvis]
#'
#' @examples
#' library(inventory)
#' BY2011_NOx_2015 <- BY2011_annual %>% filter(year == 2015, pol_abbr == "NOx")
#' BY2011_NOx_2015_by_cnty <- BY2011_NOx %>% annual_emissions_by(cnty_abbr)
#' BY2011_NOx_2015_by_cnty %>% select(cnty_abbr, ems_qty) %>% arrange(ems_qty) %>% google_piechart(caption = "2015 NOx emissions (Source: BY2011)")
#'
ggvis_barchart <- function (input_data, ..., y = ~ems_qty) {

  require(ggvis)
  ggvis(input_data) %>% layer_bars(..., y = y)

}

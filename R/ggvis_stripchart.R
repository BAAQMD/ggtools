#' Plot emissions over time, using ggvis
#'
#' @param input_data data frame
#' @param ... arguments to \code{ggvis}, including one of the form \code{y = ~var}
#'
#' @examples
#' BY2011_NOx_data <- filter(BY2011_annual, pol_abbr == "NOx", year %in% 1990:2030)
#' BY2011_NOx_by_cnty <- BY2011_NOx_data %>% annual_emissions_by(cnty_abbr)
#' BY2011_NOx_by_cnty %>% ggvis_stripchart(stroke = ~cnty_abbr)
#' BY2011_NOx_by_cnty %>% ggvis_stripchart(fill = ~cnty_abbr)
#'
#' @export
ggvis_stripchart <- function (
  input_data,
  caption = comment(input_data),
  x = ~year,
  y = ~ems_qty,
  stroke = NULL,
  fill = NULL,
  width = 800,
  height = 400,
  ...,
  y_axis_title = NULL,
  y_axis_format = "s",
  verbose = FALSE
) {

  require(ggvis)

  # TODO: implement these options
  # if (is.null(colors)) colors <- names(SAFE_COLORS)
  # if (is.null(palette)) palette <- safe_colors(colors)

  if (is.null(y_axis_title)) {
    if ("ems_unit" %in% names(input_data)) {
      y_axis_title <- unique(input_data$ems_unit) %>% ensure(length(.) == 1)
    } else {
      y_axis_title <- as.character(y) %>% str_replace("~", "")
    }
  }

  if (!is.null(fill)) {
    fill_var <- all.vars(fill)
    input_data[[fill_var]] <- factor(input_data[[fill_var]])
    fill_levels <- rev(levels(input_data[[fill_var]])) # FIXME: use levels() not unique()
    #message("Fill levels: ", paste_csv(fill_levels))
    fig <-
      ggvis(input_data, x = x, y = y) %>%
      group_by_(fill) %>%
      compute_stack(stack_var = y, group_var = x) %>%
      add_legend("fill", values = fill_levels) %>%
      layer_ribbons(y = ~stack_lwr_, y2 = ~stack_upr_, fill = fill, ...)
  } else if (!is.null(stroke)) {
    fig <- ggvis(input_data, x = x, y = y, stroke = stroke) %>%
      layer_lines(...)
  } else {
    fig <- ggvis(input_data, x = x, y = y) %>%
      layer_lines(...)
  }

  fig %>%
    scale_numeric("x", expand = 0) %>%
    add_axis("x", title = caption, format = "04d") %>%
    scale_numeric("y", expand = 0, zero = TRUE, clamp = FALSE) %>%
    add_axis("y", title = y_axis_title, format = y_axis_format, title_offset = 50) %>%
    set_options(width = width, height = height, resizable = TRUE, keep_aspect = TRUE)

}

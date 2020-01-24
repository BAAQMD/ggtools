#' ggplot2-style piechart
#'
#' @param input_data data frame
#' @param ... further arguments (optional)
#' @param colors named character
#' @param digits digits after decimal
#' @param sorted logical
#'
#' @examples
#' BY2011_NOx_2015 <- BY2011_annual %>% filter(year == 2015, pol_abbr == "NOx")
#' BY2011_NOx_2015_by_cnty <- BY2011_NOx %>% annual_emissions_by(cnty_abbr)
#' BY2011_NOx_2015_by_cnty %>% dplyr::select(cnty_abbr, ems_qty) %>% arrange(ems_qty) %>% ggplot2_piechart()
ggplot2_piechart <- function (input_data, ..., colors = NULL, digits = 0, sorted = TRUE, theme = NULL) {

  require(ggplot2)
  if (is.null(theme)) {
    theme <- ggplot2::theme_classic(base_size = 14) +
      ggplot2::theme(panel.background = element_blank(), axis.line = element_blank())
  }

  label_var <- names(input_data)[[1]]
  value_var <- names(input_data)[[2]]

  n <- nrow(input_data)

  chart_data <- data_frame(
    label = input_data[[label_var]] %>% as.factor(),
    share = input_data[[value_var]] %>% {. / sum(.)}, # normalize to [0, 1]
    start = c(0, cumsum(share)[-n]),
    end = c(cumsum(share)[-n], 1),
    mid = (end + start) / 2)

  chart_aes <- aes_string(
    xmin = I(0), xmax = I(1),
    ymin = "start", ymax = "end",
    fill = "label")

  scale_theta <- scale_y_continuous(
    breaks = chart_data$mid,
    labels = chart_data$share %>% round(digits = 2 + digits) %>% scales::percent(.),
    minor_breaks = NULL)

  if (isTRUE(sorted)) {
    chart_data <- ungroup(chart_data) %>%
      arrange(desc(share)) %>%
      mutate(label = factor(label, levels = label))
  }

  chart_labels <- levels(chart_data$label)

  if (is.null(colors)) {
    chart_colors <- setNames(SAFE_COLORS[1:n], chart_labels)
  } else {
    chart_colors <- colors[chart_labels]
    unmatched <- setdiff(chart_labels, names(colors))
    if (any(unmatched)) {
      warning("No color(s) specified for: ", unmatched)
    }
  }

  ggplot(chart_data, chart_aes) +
    geom_rect(stat = "identity") +
    coord_polar(theta = "y", direction = -1) +
    scale_theta +
    scale_x_continuous(breaks = NULL) +
    scale_fill_manual(values = chart_colors) +
    theme


}

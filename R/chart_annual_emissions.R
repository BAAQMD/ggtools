#' chart_annual_emissions_by
#'
#' @export
chart_annual_emissions_by <- function (
  annual_emission_data,
  ...,
  geom = NULL,
  flag_years = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  verbose = TRUE
) {

  msg <- function (...) if(isTRUE(verbose)) message("[plot_annual_emissions] ", ...)

  msg("WARNING: experimental --- do not use in production!")

  pol_abbrs_in_data <-
    annual_emission_data %>%
    pull(pol_abbr) %>%
    unique()

  pol_facet_list <-
    list(
      c("TSP", "PM10", "PM2.5"),
      c("TOG", "ROG")) %>%
    map(
      ~ intersect(., pol_abbrs_in_data)) %>%
    compact() %>%
    set_names(
      map_chr(., str_csv))

  chart_data <-
    annual_emission_data %>%
    annual_emissions_by(
      pol_abbr, ...) %>%
    mutate(
      pol_facet = fct_collapse(
        pol_abbr,
        !!!pol_facet_list))

  chart_x_scale <-
    scale_x_annual(
      limits = c(1990, 2030),
      breaks = seq(1990, 2050, by = 10),
      expand = c(0, 0))

  chart_y_scale <-
    scale_y_emissions(
      c("tons/yr", "tons/yr"),
      expand = expand_scale(mult = c(0, 0.3)))

  if (is.null(caption)) {
    caption <-
      glue::glue(
        'DRAFT {format(Sys.Date(), "%Y-%m-%d")}',
        # ...
        .sep = "\n")
  }

  chart_description <-
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption)

  chart_aes <-
    aes(
      x = elide_year(year),
      y = ems_qty)

  if (!is.null(geom)) {

    chart_layers <-
      get(str_c("geom_", geom))()

  } else {

    chart_layers <-
      list(
        geom_area(
          aes(group = pol_abbr),
          position = position_identity(),
          alpha = I(0.2),
          fill = gray(0.7)),
        geom_line(
          aes(group = pol_abbr),
          color = "black"))

  }

  chart_theme <-
    theme_simple() +
    theme(
      plot.subtitle = element_text(
        size = rel(0.9)))

  chart_faceting <-
    lemon::facet_rep_wrap(
      ~ pol_facet,
      ncol = 1,
      scales = "free_y",
      repeat.tick.labels = TRUE)

  chart_object <-
    ggplot(chart_data) +
    chart_theme +
    chart_aes +
    chart_layers +
    chart_faceting +
    chart_x_scale +
    chart_y_scale +
    chart_description

  if (!is.null(flag_years)) {

    flag_digits <- 4

    flag_data <-
      chart_data %>%
      filter(
        elide_year(year) %in% elide_year(flag_years)) %>%
      select(
        -pol_abbr) %>%
      distinct()

    flag_nudge_y <-
      flag_data %>%
      pull(ems_qty) %>%
      min(na.rm = TRUE) %>%
      { . * 0.1 }

    #msg("flag_nudge_y is: ", flag_nudge_y)

    flag_layer <-
      list(
        geom_point(
          data = flag_data),
        geom_text_repel(
          aes(
            #y = ems_qty * 0.50,
            label = glue::glue(
              "{signif(ems_qty, digits = flag_digits)} ",
              "{unique(flag_data$ems_unit)}")),
          segment.alpha = 0,
          force = 1,
          max.iter = 3000,
          #min.segment.length = unit(3.0, "lines"),
          point.padding = unit(0.25, "cm"),
          #box.padding = 3.0,
          direction = "y",
          nudge_y = flag_nudge_y,
          size = I(3.5),
          data = flag_data))

    chart_object <-
      chart_object +
      flag_layer

  }

  return(chart_object)

}

#' chart_annual_emissions_by
#'
#' Experimental support for easy charting of annual emissions.
#'
#' @note **Please don't use in production code! Thank you!**
#'
#' @param annual_emission_data tabular, with at least `year`, `pol_abbr`, `ems_qty`, and `ems_unit`
#' @param ... not used
#' @param geom supply "line" or "point" if you don't want filled areas
#' @param flag_years if provided, will display numeric values above these
#' @param title passed to `labs()`
#' @param subtitle passed to `labs()`
#' @param caption passed to `labs()`
#' @param verbose display messages
#'
#' @rdname chart_annual_emissions
#'
#' @export
chart_annual_emissions_by <- function (
  annual_emission_data,
  ...,
  geom = NULL,
  flag_years = NULL,
  year_limits = c(1990, 2035),
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  verbose = TRUE
) {

  msg <- function (...) if(isTRUE(verbose)) message("[chart_annual_emissions_by] ", ...)

  msg("WARNING: experimental --- do not use in production!")

  grp_vars <-
    annual_emission_data %>%
    group_by(pol_abbr, add = TRUE) %>%
    dplyr::group_vars()

  msg("grouping by: ", str_csv(grp_vars))

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
      !!grp_vars, ...) %>%
    mutate(
      pol_facet = fct_collapse(
        pol_abbr,
        !!!pol_facet_list)) %>%
    unite(
      series,
      !!grp_vars,
      remove = FALSE)

  chart_x_scale <-
    scale_x_annual(
      limits = year_limits,
      breaks = seq(1990, 2050, by = 10),
      expand = c(0, 0))

  chart_y_scale <-
    scale_y_emissions(
      "tons/yr",
      #c("tons/yr", "tons/yr"),
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

  chart_aes <-
    list(
      aes(x = elide_year(year)),
      aes(y = ems_qty))

  chart_color_scale <-
    scale_color_excel_new()

  directlabel_data <-
    chart_data %>%
    drop_vars(
      series,
      pol_facet) %>%
    filter(
      elide_year(year) == max(elide_year(year))) %>%
    nest(
      pol_abbr = starts_with("pol_")) %>%
    mutate(
      pol_abbr = map_chr(
        pol_abbr,
        ~ str_csv(.$pol_abbr)))

  directlabel_layer <-
    geom_label(
      aes(label = pol_abbr),
      data = directlabel_data)

  if (!is.null(geom)) {

    chart_geom <-
      get(str_c("geom_", geom))

    chart_layers <-
      list(
        #directlabel_layer,
        chart_geom(
          aes(group = series)))


  } else {

    chart_layers <-
      list(
        #directlabel_layer,
        geom_area(
          aes(group = series),
          position = position_identity(),
          alpha = I(0.2),
          fill = gray(0.7)),
        geom_line(
          aes(group = series),
          color = "black"))

  }

  chart_object <-
    ggplot(chart_data) +
    chart_theme +
    chart_aes +
    chart_layers +
    chart_faceting +
    chart_x_scale +
    chart_y_scale +
    chart_color_scale +
    chart_description

  if (!is.null(flag_years)) {

    flag_digits <- 4

    flag_data <-
      chart_data %>%
      drop_vars(
        series,
        pol_abbr) %>%
      distinct() %>%
      filter(
        elide_year(year) %in% elide_year(flag_years))

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
        ggrepel::geom_text_repel(
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
          show.legend = FALSE,
          data = flag_data))

    chart_object <-
      chart_object +
      flag_layer

  }

  return(chart_object)

}

#'-----------------------------------------------------------------------------

#' chart_annual_emissions
#'
#' @export
chart_annual_emissions <-
  chart_annual_emissions_by

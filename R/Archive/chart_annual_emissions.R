#' chart_annual_emissions_by
#'
#' Experimental support for easy charting of annual emissions.
#'
#' @note **Please don't use in production code! Thank you!**
#'
#' @param data tabular, with at least `year`, `pol_abbr`, `ems_qty`, and `ems_unit`
#' @param ... variables to chart by
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
  data = NULL,
  ...,
  mapping = aes(),
  geom = NULL,
  flag_years = NULL,
  year_limits = c(1990, NA),
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  verbose = TRUE
) {

  msg <- function (...) if(isTRUE(verbose)) message("[chart_annual_emissions_by] ", ...)

  msg("WARNING: experimental --- do not use in production!")

  by_vars <-
    tidyselect::vars_select(
      names(data),
      ...)

  msg("charting by: ", names(by_vars))

  if (is.null(data)) {

    chart_data <- NULL
    chart_y_unit <- "tons/yr"

  } else {

    chart_data <- data

    id_vars <-
      tidyselect::vars_select(
        names(chart_data),
        ends_with("_id"))

    if (length(id_vars) > 0) {

      recode_id <- function (x, prefix = "#") {
        if (is.character(x)) return(x)
        str_prefix <- function (x) str_c(prefix, x)
        factor(
          str_prefix(x),
          levels = str_prefix(sort(unique(x))))
      }

      msg("treating id vars as factors: ", str_csv(id_vars))

      chart_data <-
        chart_data %>%
        mutate_at(
          vars(id_vars),
          ~ recode_id(.))

    }

    chart_data <-
      chart_data %>%
      group_by_at(
        vars(pol_abbr, by_vars),
        .add = TRUE)

    grp_vars <-
      chart_data %>%
      dplyr::group_vars()

    msg("grouping by: ", str_csv(grp_vars))

    chart_data <-
      chart_data %>%
      annual_emissions_by(
        !!grp_vars) %>%
      # mutate(
      #   pol_facet = fct_collapse(
      #     pol_abbr,
      #     !!!pol_facet_list)) %>%
      unite(
        series,
        !!grp_vars,
        remove = FALSE)

    chart_y_unit <-
      chart_data %>%
      pull_distinct(
        ems_unit) %>%
      ensurer::ensure(
        length(.) == 1)

  }

  chart_y_scale <-
    scale_y_emissions(
      chart_y_unit,
      labels = format_SI,
      expand = expand_scale(mult = c(0, 0.3)))

  chart_theme <-
    theme_simple() +
    theme(
      plot.subtitle = element_text(
        size = rel(0.9)))

  chart_faceting <-
    lemon::facet_rep_wrap(
      ~ pol_abbr,
      ncol = 1,
      scales = "free_y",
      repeat.tick.labels = TRUE)

  chart_color_scale <-
    scale_color_excel_new()

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
        # geom_area(
        #   aes(group = series),
        #   position = position_identity(),
        #   alpha = I(0.2),
        #   fill = gray(0.7)),
        geom_line(
          aes(group = series)))

  }

  chart_aes <- local({

    auto_mappings <-
      c(y = "ems_qty")

    if ("color" %in% names(by_vars)) {
      auto_mappings <- c(
        auto_mappings,
        fill = by_vars[["color"]],
        color = by_vars[["color"]])
    }

    if ("linetype" %in% names(by_vars)) {
      auto_mappings <- c(
        auto_mappings,
        linetype = by_vars[["linetype"]])
    }

    chart_aes <-
      do.call(
        aes_string,
        as.list(auto_mappings))

  })

  chart_object <-
    chart_annual_quantities(
      data = chart_data,
      mapping = mapping,
      year_limits = year_limits,
      title = title,
      subtitle = subtitle,
      caption = caption,
      verbose = verbose) +
    chart_aes +
    chart_theme +
    chart_layers +
    chart_faceting +
    chart_y_scale

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

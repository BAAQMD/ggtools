#' chart_annual_quantities_by
#'
#' @usage chart_annual_quantities_by(data, ...)
#' @describeIn chart_annual_by Chart an arbitrary annual quantity.
#'
#' @importFrom lemon facet_rep_wrap
#'
#' @export
chart_annual_quantities_by <- function (
  data = NULL,
  ...,
  mapping = aes(),
  qty_var = NULL,
  chart_y_scale = NULL,
  chart_y_unit = NULL,
  geom = NULL,
  facet_rows = NULL,
  facet_cols = NULL,
  facet_scales = "fixed",
  year_limits = NULL,
  year_breaks = NULL,
  year_expand = NULL,
  flag_years = NULL,
  flag_labels = NULL,
  base_year = NULL,
  base_year_shape = 3,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[chart_annual_quantities_by] ", ...)

  if (is.null(year_limits)) {
    year_limits <-
      range(elide_year(pull(data, year)))
  }

  if (is.null(year_breaks)) {
    year_breaks <-
      seq(1990, 2050, by = 10)
  }

  if (is.null(year_expand)) {
    year_expand <-
      ggplot2::expand_scale(
        add = c(1, 1),
        mult = c(1.5/50, 1.5/50))
  }

  msg("WARNING: experimental --- do not use in production!")

  #
  # Autodetect `qty_var`, if not specified.
  #

  if (is.null(qty_var)) {

    qty_var <-
      vartools::find_var(
        data,
        suffix = "_qty")

  }

  msg("qty_var is: ", qty_var)

  #
  # Downgrade `year_limits` to a numeric vector.
  #

  if (!is.null(year_limits)) {

    year_limits <-
      yeartools::elide_year(
        year_limits)

  }

  #
  # Let `by_vars` be the `...`, plus whatever is used for faceting.
  #
  by_vars <-
    tidyselect::vars_select(
      names(data),
      ...)

  if (!is.null(facet_rows) || !is.null(facet_cols)) {

    facet_vars <-
      c(facet_rows, facet_cols) %>%
      set_names()

    by_vars <-
      c(by_vars, facet_vars)

  }

  msg("by_vars is: ", strtools::str_csv(names(by_vars)))

  #
  # Optional: faceting.
  #

  if (is.null(facet_rows) && is.null(facet_cols)) {

    chart_faceting <-
      facet_null()

  } else if (is.null(facet_cols)) {

    chart_faceting <-
      lemon::facet_rep_wrap(
        as.formula(str_c("~ ", str_c(facet_rows, collapse = " + "))),
        ncol = 1,
        scales = facet_scales,
        repeat.tick.labels = TRUE)

  } else if (is.null(facet_rows)) {

    chart_faceting <-
      lemon::facet_rep_wrap(
        as.formula(str_c("~ ", str_c(facet_cols, collapse = " + "))),
        nrow = 1,
        scales = facet_scales,
        repeat.tick.labels = TRUE)

  } else {

    chart_faceting <-
      lemon::facet_rep_grid(
        as.formula(str_c(facet_rows, " ~ ", facet_cols)),
        scales = facet_scales,
        repeat.tick.labels = TRUE)

  }

  #
  # Create `chart_x_scale`.
  #
  # `data` is either NULL --- in which case we can't determine `year_breaks` --- or it
  # has been supplied, in which case we can.
  #

  if (is.null(data)) {

    chart_x_scale <-
      scale_x_annual(
        limits = year_limits,
        expand = year_expand)

  } else {

    year_prefix <-
      pull_distinct(data, year) %>%
      str_extract("^[A-Z]Y") %>%
      unique()

    msg("year_prefix is: ", year_prefix)

    stopifnot(length(year_prefix) == 1)

    chart_x_scale <-
      scale_x_annual(
        prefix = year_prefix,
        limits = year_limits,
        breaks = year_breaks,
        expand = year_expand)

  }

  #
  # If `chart_y_scale` was not supplied, then let it be `scale_y_quantity()`.
  #
  # Functions like `chart_annual_emissions()`, `chart_annual_growth()`, etc.
  # should supply a specific `chart_y_scale`.
  #
  if (is.null(chart_y_scale)) {

    chart_y_scale <-
      scale_y_quantity()

  }

  #
  # Let `chart_data` be the aggregated form of `data`.
  #
  if (is.null(data)) {

    chart_data <- NULL

  } else {

    grouped_data <-
      data %>%
      group_by_at(
        vars(by_vars),
        .add = TRUE)

    grp_vars <-
      grouped_data %>%
      dplyr::group_vars()

    if (length(grp_vars) > 0) {

      msg("grouping by: ", str_csv(grp_vars))

      chart_data <-
        grouped_data %>%
        ungroup() %>%
        humanize_id_vars(
          verbose = verbose) %>%
        annual_quantities_by(
          !!grp_vars) %>%
        unite(
          grp_id,
          !!grp_vars,
          remove = FALSE) %>%
        group_by_at(
          all_of(grp_vars)) %>%
        mutate(
          year_break = if_else(
            elide_year(year) - lag(elide_year(year)) >= 2,
            true = TRUE, false = FALSE, missing = FALSE)) %>%
        mutate(
          series = str_c(
            grp_id,
            cumsum(year_break),
            sep = "-")) %>%
        ungroup()

    } else {

      chart_data <-
        mutate(
          data,
          series = 1)

    }

  }

  #
  # Assemble `chart_aes`.
  #
  # - if `linetype = ` was supplied, then add an aesthetic for that.
  # - if `color = ` was supplied, then add an aesthetic for that.
  # - if `fill = ` was not supplied, but `color = ` was, then use that
  #   for both color *and* fill.
  #

  if (is.null(data)) {

    chart_aes <- NULL

  } else {

    auto_mappings <-
      c(y = unname(qty_var))

    if ("linetype" %in% names(by_vars)) {
      auto_mappings <- c(
        auto_mappings,
        linetype = by_vars[["linetype"]])
    }

    if ("color" %in% names(by_vars)) {

      auto_mappings <- c(
        auto_mappings,
        color = by_vars[["color"]])

      if ("fill" %not_in% names(by_vars)) {
        auto_mappings <- c(
          auto_mappings,
          fill = by_vars[["color"]])
      }

    }

    if ("fill" %in% names(by_vars)) {

      msg("filling by ", by_vars[["fill"]])

      auto_mappings <- c(
        auto_mappings,
        fill = by_vars[["fill"]])

      if (is.null(geom)) {
        msg("setting geom to area")
        geom <- "area"
      }

    }

    chart_aes <-
      do.call(
        aes_string,
        as.list(auto_mappings))

  }

  #
  # Assemble `chart_layers` --- a list of geoms.
  #

  if (is.null(data)) {

    chart_layers <- NULL

  } else {

    msg("geom is: ", geom)

    if (is.null(geom)) {
      msg("setting geom to line")
      geom <- "line"
    }

    chart_geom <-
      get(str_c("geom_", geom))

    chart_layers <-
      list(
        chart_geom(
          aes(group = series)))

  }


  #
  # Assemble `chart_description`.
  #
  # The default chart caption is "DRAFT YYYY-mm-dd".
  #

  chart_description <- local({

    if (is.null(caption)) {
      caption <-
        glue::glue(
          'DRAFT {strtools::str_date()}',
          .sep = "\n")
    }

    chart_description <-
      labs(
        title = title,
        subtitle = subtitle,
        caption = caption)

  })

  #
  # Scales for color and fill.
  #

  chart_color_scale <-
    scale_color_tableau()

  chart_fill_scale <-
    scale_fill_tableau()

  #
  # Basic chart theme.
  #

  chart_theme <-
    theme_simple() +
    theme(
      plot.subtitle = element_text(size = rel(0.9)))

  chart_object <-
    ggplot(
      data = chart_data,
      mapping = mapping) +
    aes(x = yeartools::elide_year(year)) +
    chart_aes +
    chart_theme +
    chart_x_scale +
    chart_y_scale +
    chart_color_scale +
    chart_fill_scale +
    chart_layers +
    chart_description +
    chart_faceting

  if (!is.null(flag_years) && !is.null(data)) {

    flag_digits <- 4

    flag_data <-
      chart_data %>%
      drop_vars(
        series) %>%
      distinct() %>%
      filter(
        elide_year(year) %in% elide_year(flag_years))

    flag_nudge_y <-
      flag_data %>%
      pull(!!qty_var) %>%
      min(na.rm = TRUE) %>%
      { . * 0.1 }

    flag_layer <-
      list(
        geom_point(
          data = flag_data),
        ggrepel::geom_label_repel(
          aes(
            label = glue::glue(flag_labels, chart_y_unit = chart_y_unit)),
          fill = "white",
          segment.alpha = 0,
          force = 1,
          max.iter = 3000,
          point.padding = unit(0.1, "lines"),
          direction = "y",
          vjust = -0.5,
          #nudge_y = flag_nudge_y,
          size = I(3.5),
          show.legend = FALSE,
          data = flag_data))

    chart_object <-
      chart_object +
      flag_layer

  }

  if (!is.null(base_year)) {

    base_year_data <-
      chart_data %>%
      filter(
        elide_year(year) == elide_year(base_year))

    base_year_layers <-
      base_year_shape %>%
      map(
        ~ geom_point(
          shape = .,
          size = 4,
          position = if_else(geom == "area", "stack", "identity"),
          data = base_year_data))

    chart_object <-
      chart_object +
      base_year_layers

  }

  return(chart_object)

}

#' chart_annual_quantities
#'
#' @noRd
#'
#' @export
chart_annual_quantities <-
  chart_annual_quantities_by

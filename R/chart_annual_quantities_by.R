#' chart_annual_quantities_by
#'
#' @usage chart_annual_quantities_by(data, ...)
#' @describeIn chart_annual_by Chart an arbitrary annual quantity.
#'
#' @export
chart_annual_quantities_by <- function (
  data = NULL,
  ...,
  mapping = aes(),
  qty_var = NULL,
  chart_y_scale = NULL,
  geom = "line",
  year_limits = CY(1990, 2040),
  year_breaks = seq(1990, 2050, by = 10),
  year_expand = expand_scale(add = c(0, 1), mult = c(0, 0)),
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
  # Let `by_vars` be the `...`.
  #
  by_vars <-
    tidyselect::vars_select(
      names(data),
      ...)

  msg("by_vars is: ", strtools::str_csv(names(by_vars)))

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
          series,
          !!grp_vars,
          remove = FALSE)

    } else {

      chart_data <-
        mutate(
          data,
          series = 1)

    }

  }

  #
  # Assemble `chart_layers` --- a list of geoms.
  #

  if (is.null(data)) {

    chart_layers <- NULL

  } else {

    if (is.null(geom)) {
      msg("geom is defaulting to: ")
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

    chart_aes <- local({

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

      chart_aes <-
        do.call(
          aes_string,
          as.list(auto_mappings))

    })

  }

  chart_color_scale <-
    scale_color_tableau()

  chart_fill_scale <-
    scale_fill_tableau()

  #chart_color_scale <-
  #  ggthemes::scale_color_few(
  #    palette = "Dark")
  #
  # chart_fill_scale <-
  #  ggthemes::scale_fill_few(
  #    palette = "Dark")

  chart_theme <-
    theme_simple() +
    theme(
      plot.subtitle = element_text(size = rel(0.9)))

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
    chart_description

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
            label = glue::glue(flag_labels)),
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
        ~ geom_point(shape = ., size = 4, data = base_year_data))

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
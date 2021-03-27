#' chart_annual_quantities_by
#'
#' @usage chart_annual_quantities_by(data, ...)
#' @describeIn chart_annual_by Chart an arbitrary annual quantity.
#'
#' @importFrom lemon facet_rep_wrap facet_rep_grid
#' @importFrom ggplot2 expansion facet_null
#' @importFrom vartools find_qty_var
#' @importFrom tidyselect vars_select
#' @importFrom strtools str_csv
#' @importFrom yeartools timeline elide_year
#' @importFrom stringr str_c
#' @importFrom dplyr group_by_at group_vars ungroup mutate pull filter if_else distinct
#' @importFrom qtytools annual_quantities_by
#' @importFrom tidyr unite
#' @importFrom rlang set_names
#' @importFrom ggplot2 theme ggplot aes geom_point scale_alpha scale_alpha_manual
#' @importFrom ggthemes scale_color_tableau scale_fill_tableau
#' @importFrom glue glue
#' @importFrom ggrepel geom_label_repel
#'
#' @export
#'
#' @aliases chart_annual_quantities
chart_annual_quantities_by <- function(
  data = NULL,
  ...,
  mapping = aes(),
  geom = NULL,
  qty_var = NULL,
  chart_y_scale = NULL,
  chart_y_labels = NULL,
  chart_y_title = waiver(),
  chart_y_unit = NULL,
  chart_gridlines = TRUE,
  chart_geom = NULL,
  facet_rows = NULL,
  facet_cols = NULL,
  facet_scales = "fixed",
  year_limits = NULL,
  year_breaks = NULL,
  year_expand = NULL,
  flag_years = NULL,
  flag_labels = NULL,
  flag_unique = NULL,
  base_year = NULL,
  base_year_shape = 3,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[chart_annual_quantities_by] ", ...)

  msg("names(data) is: ", str_csv(names(data)))

  if (is.null(chart_geom)) {
    chart_geom <- geom
  }

  if (is.null(year_limits)) {
    year_limits <-
      range(as.numeric(elide_year(pull(data, year))))
  }

  if (is.null(year_breaks)) {
    year_breaks <-
      seq(1990, 2050, by = 10)
  }

  msg("WARNING: experimental --- do not use in production!!")

  #
  # Autodetect `qty_var`, if not specified.
  #
  if (is.null(qty_var)) {
    qty_var <-
      vartools::find_qty_var(
        data,
        verbose = verbose)
  }
  msg("qty_var is: ", qty_var)

  #
  # Downgrade `year_limits` to a numeric vector.
  #
  if (isFALSE(is.null(year_limits))) {
    year_limits <-
      as.numeric(yeartools::elide_year(year_limits))
  }

  #
  # Let `by_vars` be the `...`, plus whatever is used for faceting.
  #
  by_vars <-
    tidyselect::vars_select(
      names(data),
      ...)

  if (isFALSE(is.null(facet_rows)) || isFALSE(is.null(facet_cols))) {

    facet_vars <-
      set_names(
        c(facet_rows, facet_cols))

    by_vars <-
      c(by_vars, facet_vars)

  }
  msg("by_vars is: ", strtools::str_csv(names(by_vars)))
  msg("names(by_vars) is: ", strtools::str_csv(names(by_vars)))

  #
  # Optional: faceting.
  #
  if (is.null(facet_rows) && is.null(facet_cols)) {

    chart_faceting <-
      ggplot2::facet_null()

  } else if (is.null(facet_cols)) {

    chart_faceting <-
      lemon::facet_rep_wrap(
        as.formula(str_c("~ ", stringr::str_c(facet_rows, collapse = " + "))),
        ncol = 1,
        scales = facet_scales,
        repeat.tick.labels = TRUE)

  } else if (is.null(facet_rows)) {

    chart_faceting <-
      lemon::facet_rep_wrap(
        as.formula(str_c("~ ", stringr::str_c(facet_cols, collapse = " + "))),
        nrow = 1,
        scales = facet_scales,
        repeat.tick.labels = TRUE)

  } else {

    chart_faceting <-
      lemon::facet_rep_grid(
        as.formula(stringr::str_c(facet_rows, " ~ ", facet_cols)),
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
      yeartools::timeline(
        pull_distinct(data, year))

    msg("year_prefix is: ", year_prefix)

    stopifnot(length(year_prefix) < 2)

    if ((length(year_prefix) == 0) || is.na(year_prefix)) {
      warn_msg <- "[chart_annual_quantities_by] there's no prefix on `year` (CY, RY, FY, etc.)"
      warning(warn_msg)
      year_prefix <- ""
    }

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

    if (is.null(chart_y_labels)) {
      msg("defaulting: labels = format_qty")
      chart_y_labels <- format_qty
    }

    msg("defaulting: chart_y_scale = scale_y_quantity")
    chart_y_scale <-
      scale_y_quantity(
        name = chart_y_title,
        labels = chart_y_labels)

  }

  #
  # Let `chart_data` be the aggregated form of `data`.
  #
  if (is.null(data)) {

    chart_data <- NULL

  } else {

    msg("grouping by `by_vars`: ", strtools::str_csv(by_vars))

    grouped_data <-
      dplyr::group_by_at(
        data,
        all_of(unname(by_vars)),
        .add = TRUE)

    grp_vars <-
      dplyr::group_vars(grouped_data)

    msg("grp_vars is: ", str_csv(grp_vars))
    msg("names(grp_vars) is: ", str_csv(names(grp_vars)))

    aggregated_data <-
      dplyr::ungroup(grouped_data) %>%
      humanize_id_vars(verbose = verbose) %>%
      qtytools::annual_quantities_by(
        all_of(grp_vars))

    #all_of(unique(c(grp_vars, by_vars))))

    if (length(grp_vars) > 0) {

      msg("grouping by: ", str_csv(grp_vars))

      chart_data <-
        aggregated_data %>%
        tidyr::unite(
          grp_id,
          !!grp_vars,
          remove = FALSE) %>%
        dplyr::group_by_at(
          all_of(grp_vars)) %>%
        dplyr::mutate(
          year_break = dplyr::if_else(
            as.numeric(elide_year(year)) - lag(as.numeric(elide_year(year))) >= 2,
            true = TRUE, false = FALSE, missing = FALSE)) %>%
        dplyr::mutate(
          series = stringr::str_c(
            grp_id,
            cumsum(year_break),
            sep = "-")) %>%
        dplyr::ungroup()

    } else {

      chart_data <-
        dplyr::mutate(
          aggregated_data,
          series = 1)

    }

  }

  msg("nrow(chart_data) = ", nrow(chart_data))

  #
  # Assemble `chart_aes`.
  # - if `shape` was supplied, then add an aesthetic for that.
  # - if `linetype` was supplied, then add an aesthetic for that.
  # - if `color` was supplied, then add an aesthetic for that.
  # - if `fill` was not supplied, but `color` was, then use that
  #   for both `fill` _and_ `color`.
  #
  # Also:
  # - if `shape` was supplied, then set the geom to "point".
  # - if `fill` was supplied, then set the geom to "area".
  #

  if (is.null(data)) {

    chart_aes <- NULL

  } else {

    auto_mappings <-
      c(y = unname(qty_var))

    # Shape.
    if ("shape" %in% names(by_vars)) {

      auto_mappings <- c(
        auto_mappings,
        shape = by_vars[["shape"]])

      if (is.null(chart_geom)) {
        msg("setting chart_geom to point")
        chart_geom <- "point"
      }

    }


    # Linetype (dashed, dotted, etc.)
    if ("linetype" %in% names(by_vars)) {
      auto_mappings <- c(
        auto_mappings,
        linetype = by_vars[["linetype"]])
    }

    # Color.
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

    #
    # Treat `fill` like `color`.
    #
    # If `fill` is present, but `chart_geom` is not, then set `chart_geom` to "area".
    #
    if ("fill" %in% names(by_vars)) {

      msg("filling by ", by_vars[["fill"]])

      auto_mappings <- c(
        auto_mappings,
        fill = by_vars[["fill"]])

      if (is.null(chart_geom)) {
        msg("setting chart_geom to area")
        chart_geom <- "area"
      }

      # We have to fiddle a bit with `series` to get area wedges to stack in
      # the desired order
      chart_data <-
        chart_data %>%
        arrange(
          by_vars[["fill"]]) %>%
        mutate(
          series = fct_inorder(series))

    }

    #
    # Alpha, aka opacity.
    #
    alpha_var <-
      intersect(
        names(by_vars),
        c("alpha", "opacity"))

    if (isTRUE(length(alpha_var) > 0)) {

      auto_mappings <- c(
        auto_mappings,
        alpha = by_vars[[alpha_var]])

      alpha_set <-
        levels(dplyr::pull(chart_data, by_vars[[alpha_var]]))

      alpha_levels <-
        set_names(
          (1 / seq_along(alpha_set)) ^ 1.2, # 1.2 acts as gamma
          alpha_set)

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

    msg("chart_geom is: ", chart_geom)

    if (is.null(chart_geom)) {
      msg("setting chart_geom to line")
      chart_geom <- "line"
    }

    # Helpful auto-correction.
    if (chart_geom == "bar") {
      chart_geom <- "col"
    }

    chart_geom_constructor <-
      get(str_c("geom_", chart_geom))

    chart_layers <-
      list(
        chart_geom_constructor(
          aes(group = series)))

  }

  #
  # Assemble title, subtitle, and caption.
  #
  chart_description <- local({

    # Try to supply a helpful title, if `category` is in the data & is unique.
    if (isTRUE(is.null(title))) {
      if ("category" %in% names(data)) {
        category <- pull_distinct(data, category)
        if (length(category) == 1) {
          title <- category
        }
      }
    }

    # Default caption is "DRAFT YYYY-mm-dd".
    if (is.null(caption)) {
      caption <-
        glue::glue(
          'DRAFT {strtools::str_date()}',
          .sep = "\n")
    }

    chart_description <-
      ggplot2::labs(
        title = title,
        subtitle = subtitle,
        caption = caption)

  })

  #
  # Scales for color and fill.
  #

  chart_color_scale <-
    ggthemes::scale_color_tableau()

  chart_fill_scale <- list(
    ggthemes::scale_fill_tableau(),
    guides(fill = guide_legend(reverse = FALSE)))

  if (exists("alpha_levels")) {
    chart_alpha_scale <-
      ggplot2::scale_alpha_manual(
        values = alpha_levels)
  } else {
    chart_alpha_scale <-
      ggplot2::scale_alpha()
  }

  chart_shape_scale <- NULL
  shape_var <- pluck(by_vars, "shape")
  if (isFALSE(is.null(shape_var)) && (shape_var == "inventory")) {
    if (setequal(pull_distinct(data, inventory), c("PY", "RY"))) {
      msg("using manual scale for shape")
      chart_shape_scale <- scale_shape_manual(
        values = c("RY" = 16, "PY" = 5))
    }
  }

  chart_scales <-
    list(
      chart_x_scale,
      chart_y_scale,
      chart_shape_scale,
      chart_color_scale,
      chart_fill_scale,
      chart_alpha_scale)

  #
  # Basic chart theme.
  #

  if (isTRUE(chart_gridlines)) {
    chart_gridlines <- element_line(linetype = "dotted", color = gray(0.7))
  }

  chart_theme <-
    theme_simple() +
    ggplot2::theme(
      panel.grid.major.y = chart_gridlines,
      plot.subtitle = element_text(size = rel(0.9)))

  chart_object <-
    ggplot2::ggplot(
      data = chart_data,
      mapping = mapping) +
    ggplot2::aes(x = as.numeric(yeartools::elide_year(year))) +
    chart_aes +
    chart_theme +
    chart_scales +
    chart_layers +
    chart_description +
    chart_faceting

  if (!is.null(flag_years) && !is.null(data)) {

    flag_data <-
      chart_data %>%
      vartools::drop_vars(series) %>%
      dplyr::distinct() %>%
      dplyr::filter(
        yeartools::elide_year(year) %in% yeartools::elide_year(flag_years)) %>%
      dplyr::mutate(
        label = as.character(
          glue::glue(
            flag_labels,
            chart_y_unit = chart_y_unit)))

    if (isTRUE(flag_unique)) {
      flag_data <-
        flag_data %>%
        group_by_at(
          vars(year, label)) %>%
        summarize_at(
          vars(everything()),
          first) %>%
        ungroup()
    }

    flag_nudge_y <-
      flag_data %>%
      pull(!!qty_var) %>%
      min(na.rm = TRUE) %>%
      { . * 0.1 }

    flag_layer <-
      list(
        ggplot2::geom_point(
          data = flag_data),
        ggrepel::geom_label_repel(
          aes(
            label = label),
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
      dplyr::filter(
        chart_data,
        elide_year(year) == elide_year(base_year))

    base_year_layers <-
      purrr::map(
        base_year_shape,
        ~ geom_point(
          shape = .,
          size = 4,
          position = if_else(chart_geom == "area", "stack", "identity"),
          data = base_year_data))

    chart_object <-
      chart_object +
      base_year_layers

  }

  attr(chart_object, "data") <- chart_data
  return(chart_object)

}

#' chart_annual_quantities
#'
#' @export
chart_annual_quantities <-
  chart_annual_quantities_by

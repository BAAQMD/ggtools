###############################################################################
#
#  DMH (2018-01-18)
#  + Removed dependency on `ggtools` package
#
###############################################################################

library(inventory)

PY_PM_data <-
  PY(2000:2016) %>% point_source_emissions() %>%
  filter(pol_abbr == "PM")

RY_PM_data <-
  RY(2007:2016) %>% point_source_emissions() %>%
  filter(pol_abbr == "PM")

combined_PM_data <-
  list("Reporting Year" = RY_PM_data, "Permit Year" = PY_PM_data) %>%
  bind_rows(.id = "timeline")

SELECTED_REFINING_FACILITIES <-
  inventory::DST_REFINING_FACILITIES %>%
  .[c("Chevron", "Shell", "Tesoro",
      # "Martinez Cogen", "P66 Carbon", # don't include these (secondary) facilities
      "Valero", "Phillips 66")]

combined_refining_PM_data <-
  combined_PM_data %>%
  filter_facilities(SELECTED_REFINING_FACILITIES)

###############################################################################

PM25_chart_description <-
  labs(title = "Apparent Trends in PM2.5 Emissions, 2000-2016",
       subtitle = "Major refining facilities, as inventoried by BAAQMD\n",
       caption = "Basis: 'PM' x (PM2.5 / 'PM', by category)")

PM25_chart_data <-
  combined_refining_PM_data %>%
  speciate_PM(into = "PM2.5") %>%
  mutate(fac_name = factor(fac_name, names(SELECTED_REFINING_FACILITIES))) %>%
  arrange(fac_name, year) # combined with factor() above, forces a certain order of names

PM25_chart_theme <-
  theme_minimal() +
  theme(plot.subtitle = element_text(margin = margin(b = 1, unit = "pt")),
        axis.text.x = element_text(margin = margin(t = 3, b = 6, unit = "pt")),
        axis.text.y = element_text(margin = margin(l = 3, unit = "pt")),
        axis.title = element_text(size = rel(10/12)),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.key.height = unit(9, "pt"),
        legend.spacing.y = unit(3, "pt"),
        legend.box.margin = margin(t = 0, b = 0, unit = "pt"),
        legend.margin = margin(t = 0, b = 3, unit = "pt"),
        legend.title = element_text(size = rel(10/12), face = "bold", margin = margin(t = 0, b = 0, unit = "pt")),
        plot.caption = element_text(size = rel(10/12), margin = margin(t = 6), face = "italic", color = gray(0.5)),
        strip.text.x = element_text(face = "bold", hjust = 0))

PM25_chart <-
  PM25_chart_data %>%
  annual_emissions_by(timeline, fac_name, pol_abbr) %>%
  ggplot(aes(x = parse_number(year), y = ems_qty)) +
  geom_line(aes(linetype = timeline)) +
  scale_y_continuous(name = "PM2.5 (tons)", limits = c(0, 650), labels = format_SI) +
  scale_x_continuous(name = "") +
  scale_linetype_manual(name = "Inventory",
                        values = c("Reporting Year" = "solid", "Permit Year" = "dashed")) +
  facet_wrap(~ fac_name, scales = "free_x") + # Note: "free" forces axis labels on every facet
  PM25_chart_description +
  PM25_chart_theme +
  theme(axis.title.y = element_text(hjust = 0.9))

show(PM25_chart)

###############################################################################

# To look up facility ID by facility name
P_ids <- function (x) {
  DST_FACILITY_IDS <-
    list(DST_REFINING_FACILITIES, DST_LANDFILL_FACILITIES, DST_POWER_FACILITIES) %>%
    reduce(append)
  unname(unlist(DST_FACILITY_IDS[x]))
}

tag_ccu_equipment <- function (input_data) {

  ccu_equipment_set <- bind_rows(
    expand_grid(fac_id = P_ids("Chevron"), src_id = 4285),
    expand_grid(fac_id = P_ids("Shell"), src_id = 1426),
    expand_grid(fac_id = P_ids("Tesoro"), src_id = c(98, 99, 802, 806, 901)),
    expand_grid(fac_id = P_ids("Valero"), src_id = c(3, 4, 5, 6, 13, 50, 173, 1059, 1060)))

  ccu_equipment_set %>%
    mutate(is_ccu = TRUE) %>%
    left_join(input_data, .)

}

tag_cooling_tower_equipment <- function (input_data) {

  cool_twr_equipment_set <- bind_rows(
    expand_grid(fac_id = P_ids("Chevron"), src_id = c(4073, 4076, 4172, 4173, 4187, 4191, 4329, 6051)),
    expand_grid(fac_id = P_ids("Shell"), src_id = c(1456, 1457, 1460, 1778, 4210)),
    expand_grid(fac_id = P_ids("Phillips 66"), src_id = c(452, 453, 455, 456, 457, 458, 500)),
    expand_grid(fac_id = P_ids("Tesoro"), src_id = c(846, 975, 976, 977, 978, 979, 980, 981, 982, 983, 985, 987, 988)),
    expand_grid(fac_id = P_ids("Valero"), src_id = c(29)))

  cool_twr_equipment_set %>%
    mutate(is_cool_twr = TRUE) %>%
    left_join(input_data, .)

}

tagged_PY_PM25_data <-
  PM25_chart_data %>%
  filter(year %>% str_begins("PY")) %>%
  tag_ccu_equipment() %>%
  tag_cooling_tower_equipment() %>%
  mutate(src_tag = case_when(is_ccu ~ "Catalytic Cracker",
                             is_cool_twr ~ "Cooling Tower"))

tagged_layer_data <-
  tagged_PY_PM25_data  %>%
  annual_emissions_by(timeline, fac_name, pol_abbr, src_tag) %>%
  mutate(src_tag = factor(src_tag, c("Cooling Tower", "Catalytic Cracker"))) %>%
  filter(!is.na(src_tag))

gg_overlay_subset <- function (what, ..., geom = "col", width = 0.8, alpha = 0.9) {

  print(what)

  if (inherits(what, "data.frame")) {
    layer_data <- what
  } else if (inherits(what, "formula")) {
    stop("support for formulae not yet implemented")
  } else {
    err_msg <- str_c("support for ", class(what), " not yet implemented")
    stop(err_msg)
  }

  make_layer <- get(str_c("geom_", geom))
  make_layer(..., data = layer_data)

}

PM25_chart +
  gg_overlay_subset(what = tagged_layer_data, mapping = aes(fill = src_tag)) +
  scale_fill_manual("Equipment", values = c("Cooling Tower" = rgb(0.35, 0.70, 0.90),
                                            "Catalytic Cracker" = rgb(0.90, 0.60, 0.00)))


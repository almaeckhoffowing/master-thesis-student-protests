# Descriptive statistics for:
# Master thesis project: "Are Student-Led Protests a Force for Democracy?"
# By: Alma Eckhoff Owing
# Date: May, 15th, 2026

# This script consists of the descriptive figures and tables presented in the thesis. 
# R version 4.5.1 (2025-06-13)

#-------------------------------------------------------------------------------
# Loading relevant packages:
library(tidyverse)
library(scales)
library(ggrepel)
library(RColorBrewer)
library(kableExtra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggnewscale)

#-------------------------------------------------------------------------------
#load data
omg <- readRDS("/Users/almaowing/Documents/MAthesis/data/R/omg.rds")
vdem <- readRDS("/Users/almaowing/Documents/MAthesis/data/R/V-Dem-CY-Full+Others-v15.rds")

#-------------------------------------------------------------------------------

#create dependent and independent variables:

#student_protest variable (dominate only)
omg$student_protest <- as.numeric(omg$dominate_students == 1)
omg$student_protest[is.na(omg$student_protest)] <- 0

# student index (sum of participation + origination + domination)
omg <- omg %>%
  mutate(
    student_index = as.integer(atleast_students == 1) +
      as.integer(originate_students == 1) +
      as.integer(dominate_students == 1)
  )
omg$student_index[is.na(omg$student_index)] <- 0

#democratic demand
omg$dem_demand <- as.numeric(
  omg$demand_civilrights == 1 |
    omg$demand_free_expression == 1 |
    omg$demand_election == 1 |
    omg$demand_executive == 1 |
    omg$demand_main_institutional == 1 |
    omg$demand_demo == 1
)
omg$dem_demand[is.na(omg$dem_demand)] <- 0

#lets see:
table(omg$dem_demand)
table(omg$student_protest)
table(omg$student_index)

#-------------------------------------------------------------------------------

#Figure 1.1 (main): Number of protest camapaigns by dominant social group:

x_label_start <- 2020

#Long-format with 10-year bins (excluding religious/ethnic):
omg_long <- omg %>%
  mutate(period = floor(start_year / 10) * 10) %>%
  select(period, dominate_students, dominate_workers_general, dominate_indwork,
         dominate_nonindurban, dominate_peasant, dominate_rural,
         dominate_intellectuals, dominate_professionals, dominate_urb_middle_class,
         dominate_business, dominate_agrarianelites, dominate_pubemp, dominate_milemp) %>%
  pivot_longer(cols = -period, names_to = "group", values_to = "dominate") %>%
  filter(dominate == 1) %>%
  mutate(group = case_match(group,
                            "dominate_students"          ~ "Students",
                            "dominate_workers_general"   ~ "Workers in general",
                            "dominate_indwork"           ~ "Industrial workers",
                            "dominate_nonindurban"       ~ "Non-industrial urban workers",
                            "dominate_peasant"           ~ "Peasants",
                            "dominate_rural"             ~ "Rural groups",
                            "dominate_intellectuals"     ~ "Intellectuals",
                            "dominate_professionals"     ~ "Professionals",
                            "dominate_urb_middle_class"  ~ "Urban middle class",
                            "dominate_business"          ~ "Business elites",
                            "dominate_agrarianelites"    ~ "Agrarian elites",
                            "dominate_pubemp"            ~ "Public employees",
                            "dominate_milemp"            ~ "Military",
                            .default = group
  ))


#Counts per period:
omg_counts <- omg_long %>%
  count(period, group) %>%
  complete(period, group, fill = list(n = 0))


#Labels at last period (right side of plot):
label_data <- omg_counts %>%
  filter(period == max(period))


#Plot:
fig1_1 <- ggplot(omg_counts, aes(x = period, y = n, color = group)) +
  # background lines for all non-student groups (faded)
  geom_line(
    data = omg_counts %>% filter(group != "Students"),
    aes(color = group), linewidth = 0.6, alpha = 0.35
  ) +
  # highlighted line for students (wine-colored, bold)
  geom_line(
    data = omg_counts %>% filter(group == "Students"),
    color = "#741b47", linewidth = 1.5
  ) +
  # labels for non-student groups
  geom_text_repel(
    data = label_data %>% filter(group != "Students"),
    inherit.aes = FALSE,
    aes(x = period, y = n, label = group, color = group),
    nudge_x            = 5,
    direction          = "y",
    hjust              = 0,
    size               = 4,
    alpha              = 0.85,
    box.padding        = 0.3,
    segment.color      = "grey70",
    segment.size       = 0.25,
    segment.alpha      = 0.6,
    min.segment.length = 0,
    max.overlaps       = Inf,
    show.legend        = FALSE
  ) +
  # bold label for students
  geom_text_repel(
    data = label_data %>% filter(group == "Students"),
    inherit.aes = FALSE,
    aes(x = period, y = n, label = group),
    color              = "#741b47",
    nudge_x            = 5,
    nudge_y            = 2,
    direction          = "y",
    hjust              = 0,
    size               = 5,
    fontface           = "bold",
    box.padding        = 0.3,
    segment.color      = "grey70",
    segment.size       = 0.25,
    min.segment.length = 0,
    max.overlaps       = Inf,
    force              = 0.3,
    force_pull         = 1,
    xlim               = c(x_label_start, NA),
    show.legend        = FALSE
  ) +
  scale_color_manual(values = c(
    "Students"                     = "#741b47",
    "Military"                     = "#E68310",
    "Workers in general"           = "#4A4A4A",
    "Intellectuals"                = "#F2B701",
    "Professionals"                = "#7F3C8D",
    "Urban middle class"           = "#3969AC",
    "Industrial workers"           = "#1A1A1A",
    "Non-industrial urban workers" = "#66C2A5",
    "Peasants"                     = "#80BA5A",
    "Rural groups"                 = "#1B3A57",
    "Public employees"             = "#B22222",
    "Business elites"              = "#11A579",
    "Agrarian elites"              = "#CF1C90"
  )) +
  scale_x_continuous(
    breaks = seq(1800, 2010, by = 30),
    limits = c(min(omg_counts$period), x_label_start + 40)
  ) +
  scale_y_continuous(breaks = seq(0, 30, by = 5), limits = c(NA, 33)) +
  labs(
    x = "Year (ten-year intervals)",
    y = "Number of protest campaigns by dominant group"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "none",
    plot.margin      = margin(5.5, 30, 5.5, 5.5)
  )

fig1_1


#Save figure:
ggsave(
  "/Users/almaowing/Documents/MAthesis/figures/fig1_1.pdf",
  plot   = fig1_1,
  width  = 15,
  height = 7,
  scale  = 0.85,
  units  = "in"
)

#-------------------------------------------------------------------------------

#Figure 1.1 (appendix): with religious/ethnic groups included:

omg_long_app <- omg %>%
  mutate(period = floor(start_year / 10) * 10) %>%
  select(period, dominate_students, dominate_workers_general, dominate_indwork,
         dominate_nonindurban, dominate_peasant, dominate_rural,
         dominate_intellectuals, dominate_professionals, dominate_urb_middle_class,
         dominate_business, dominate_agrarianelites, dominate_pubemp, dominate_milemp,
         dominate_relethnic) %>%
  pivot_longer(cols = -period, names_to = "group", values_to = "dominate") %>%
  filter(dominate == 1) %>%
  mutate(group = case_match(group,
                            "dominate_students"          ~ "Students",
                            "dominate_workers_general"   ~ "Workers in general",
                            "dominate_indwork"           ~ "Industrial workers",
                            "dominate_nonindurban"       ~ "Non-industrial urban workers",
                            "dominate_peasant"           ~ "Peasants",
                            "dominate_rural"             ~ "Rural groups",
                            "dominate_intellectuals"     ~ "Intellectuals",
                            "dominate_professionals"     ~ "Professionals",
                            "dominate_urb_middle_class"  ~ "Urban middle class",
                            "dominate_business"          ~ "Business elites",
                            "dominate_agrarianelites"    ~ "Agrarian elites",
                            "dominate_pubemp"            ~ "Public employees",
                            "dominate_milemp"            ~ "Military",
                            "dominate_relethnic"         ~ "Religious/ethnic groups",
                            .default = group
  ))

omg_counts_app <- omg_long_app %>%
  count(period, group) %>%
  complete(period, group, fill = list(n = 0))

label_data_app <- omg_counts_app %>%
  filter(period == max(period))


fig1_app <- ggplot(omg_counts_app, aes(x = period, y = n, color = group)) +
  geom_line(
    data = omg_counts_app %>% filter(group != "Students"),
    aes(color = group), linewidth = 0.6, alpha = 0.35
  ) +
  geom_line(
    data = omg_counts_app %>% filter(group == "Students"),
    color = "#741b47", linewidth = 1.5
  ) +
  geom_text_repel(
    data = label_data_app %>% filter(group != "Students"),
    inherit.aes = FALSE,
    aes(x = period, y = n, label = group, color = group),
    nudge_x            = 5,
    direction          = "y",
    hjust              = 0,
    size               = 4,
    alpha              = 0.85,
    box.padding        = 0.3,
    segment.color      = "grey70",
    segment.size       = 0.25,
    segment.alpha      = 0.6,
    min.segment.length = 0,
    max.overlaps       = Inf,
    show.legend        = FALSE
  ) +
  geom_text_repel(
    data = label_data_app %>% filter(group == "Students"),
    inherit.aes = FALSE,
    aes(x = period, y = n, label = group),
    color              = "#741b47",
    nudge_x            = 5,
    nudge_y            = 2,
    direction          = "y",
    hjust              = 0,
    size               = 5,
    fontface           = "bold",
    box.padding        = 0.3,
    segment.color      = "grey70",
    segment.size       = 0.25,
    min.segment.length = 0,
    max.overlaps       = Inf,
    force              = 0.3,
    force_pull         = 1,
    xlim               = c(x_label_start, NA),
    show.legend        = FALSE
  ) +
  scale_color_manual(values = c(
    "Students"                     = "#741b47",
    "Military"                     = "#E68310",
    "Workers in general"           = "#4A4A4A",
    "Intellectuals"                = "#F2B701",
    "Professionals"                = "#7F3C8D",
    "Urban middle class"           = "#3969AC",
    "Industrial workers"           = "#1A1A1A",
    "Non-industrial urban workers" = "#66C2A5",
    "Peasants"                     = "#80BA5A",
    "Rural groups"                 = "#1B3A57",
    "Public employees"             = "#B22222",
    "Business elites"              = "#11A579",
    "Agrarian elites"              = "#CF1C90",
    "Religious/ethnic groups"      = "#8C6D31"
  )) +
  scale_x_continuous(
    breaks = seq(1790, 2020, by = 40),
    limits = c(min(omg_counts_app$period), x_label_start + 40)
  ) +
  scale_y_continuous(breaks = seq(0, 80, by = 10), limits = c(NA, NA)) +
  labs(
    x = "Year (ten-year intervals)",
    y = "Number of protest campaigns by dominant group"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "none",
    plot.margin      = margin(5.5, 30, 5.5, 5.5)
  )

fig1_app

ggsave(
  "/Users/almaowing/Documents/MAthesis/images/fig1_app.pdf",
  plot   = fig1_app,
  width  = 15,
  height = 7,
  scale  = 0.85,
  units  = "in"
  )

#-------------------------------------------------------------------------------

# Figure 4.1: share of campaigns with democratic demands over time:
  
#Compute share per 10-year period:
all_camps <- omg %>%
  filter(!is.na(start_year)) %>%
  mutate(period = floor(start_year / 10) * 10) %>%
  group_by(period) %>%
  summarise(share = mean(dem_demand, na.rm = TRUE), .groups = "drop") %>%
  mutate(series = "All campaigns")


# plot
fig4_1 <- ggplot(all_camps, aes(x = period, y = share)) +
  geom_line(linewidth = 0.9, color = "#2C3E50") +
  geom_point(size = 1.5, color = "#2C3E50") +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1)
  ) +
  scale_x_continuous(
    breaks = seq(1790, 2020, by = 20),
    limits = c(1780, 2020)
  ) +
  labs(
    x = "Year (ten-year intervals)",
    y = "Share of campaigns with democratic demands"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.title.x       = element_text(margin = margin(t = 10)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3)
  )

fig4_1


#Save figure:
ggsave(
  "/Users/almaowing/Documents/MAthesis/figures/fig4_1.pdf",
  plot   = fig4_1,
  width  = 9,
  height = 5,
  units  = "in"
)

#-------------------------------------------------------------------------------

#Figrue 4.2: Regional V-Dem electoral democracy (1789-2019):
  
region_colors <- c(
  "Africa"   = "#e41a1c",
  "Americas" = "#377eb8",
  "Asia"     = "#4daf4a",
  "Europe"   = "#984ea3",
  "MENA"     = "#ff7f00",
  "Oceania"  = "#e6ab02"
)

vdem6 <- vdem %>%
  mutate(
    e_regiongeo_code = as.integer(e_regiongeo),
    region6 = case_when(
      e_regiongeo_code %in% 1:4 ~ "Europe",
      e_regiongeo_code %in% 6:9 ~ "Africa",
      e_regiongeo_code %in% c(5, 10) ~ "MENA",
      e_regiongeo_code %in% 11:14 ~ "Asia",
      e_regiongeo_code == 15 ~ "Oceania",
      e_regiongeo_code %in% 16:19 ~ "Americas",
      TRUE ~ NA_character_
    ),
    region6 = factor(region6, levels = c("Africa", "Americas", "Asia", "Europe", "MENA", "Oceania"))
  )

poly_region_year <- vdem6 %>%
  filter(year >= 1780, year <= 2020, !is.na(region6), !is.na(v2x_polyarchy)) %>%
  group_by(region6, year) %>%
  summarise(poly_mean = mean(v2x_polyarchy, na.rm = TRUE), .groups = "drop")

#plot:
fig4_2 <- ggplot(poly_region_year, aes(x = year, y = poly_mean, color = region6)) +
  geom_line(linewidth = 0.35) +
  facet_wrap(~ region6, ncol = 3) +
  scale_color_manual(values = region_colors) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(
    limits = c(1780, 2020),
    breaks = seq(1780, 2020, by = 40)
  ) +
  labs(
    title = NULL,
    x = "Year",
    y = "Electoral democracy"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      margin = margin(t = 2)
    ),
    strip.text = element_text(face = "bold", color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.25),
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(face = "bold")
  )

fig4_2

ggsave(
  "/Users/almaowing/Documents/MAthesis/images/vdem_polyarchy_by_region1.pdf",
  plot   = fig4_2,
  width  = 11,
  height = 7,
  units  = "in"
)

#-------------------------------------------------------------------------------


# Figure 4.3: Share of student protests per country:

#recode country names to match world map:
recode_countries <- function(df) {
  df %>%
    mutate(country_name = case_match(country_name,
                                     "Burma/Myanmar"              ~ "Myanmar",
                                     "Ivory Coast"                ~ "Côte d'Ivoire",
                                     "North Korea"                ~ "Dem. Rep. Korea",
                                     "South Korea"                ~ "Republic of Korea",
                                     "Palestine/Gaza"             ~ "Palestine",
                                     "Republic of Vietnam"        ~ "Vietnam",
                                     "Russia"                     ~ "Russian Federation",
                                     "United States of America"   ~ "United States",
                                     "German Democratic Republic" ~ "Germany",
                                     "Palestine/British Mandate"  ~ "Palestine",
                                     "Palestine/West Bank"        ~ "Palestine",
                                     "Piedmont-Sardinia"          ~ "Italy",
                                     "South Yemen"                ~ "Yemen",
                                     "Zanzibar"                   ~ "Tanzania",
                                     .default = country_name
    ))
}


#Count all campaigns per country:
all_counts <- omg %>%
  group_by(country_name) %>%
  summarise(n_all = n_distinct(id), .groups = "drop")


#Count student-led campaigns per country:
student_counts <- omg %>%
  filter(student_protest == 1) %>%
  group_by(country_name) %>%
  summarise(n_student = n_distinct(id), .groups = "drop")


#Compute share of student campaigns:
share_counts <- all_counts %>%
  left_join(student_counts, by = "country_name") %>%
  mutate(
    n_student = replace_na(n_student, 0),
    share     = n_student / n_all
  )


#Harmonize country names for map matching:
share_counts_fixed       <- recode_countries(share_counts)
countries_with_data_fixed <- recode_countries(
  omg %>% distinct(country_name)
)


#Load and prepare world map:
sf::sf_use_s2(FALSE)
world_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_make_valid() %>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) %>%
  filter(continent != "Antarctica")


#Join data onto map:
map_df_share <- world_map %>%
  left_join(share_counts_fixed, by = c("name_long" = "country_name")) %>%
  mutate(
    no_data    = ifelse(
      !(name_long %in% countries_with_data_fixed$country_name),
      "No data", NA
    ),
    share_plot = pmin(share, 0.5)
  )


#Plot:
map_share <- ggplot() +
  # countries with no data: light grey
  geom_sf(
    data = subset(map_df_share, !is.na(no_data)),
    aes(fill = no_data),
    color = "grey35", linewidth = 0.12
  ) +
  scale_fill_manual(
    values = c("No data" = "grey85"),
    breaks = "No data",
    name   = NULL,
    guide  = guide_legend(
      label.position = "top",
      keywidth       = unit(0.87, "cm"),
      keyheight      = unit(0.45, "cm"),
      order          = 1
    )
  ) +
  ggnewscale::new_scale_fill() +
  # countries with data: colored by share
  geom_sf(
    data = subset(map_df_share, is.na(no_data)),
    aes(fill = share_plot),
    color = "grey35", linewidth = 0.12
  ) +
  scale_fill_gradientn(
    colours = c("#2c7bb6", "#abdda4", "#ffffbf", "#fdae61", "#d7191c"),
    limits  = c(0, 0.5),
    labels  = percent_format(accuracy = 1),
    name    = NULL,
    guide   = guide_colorbar(
      direction      = "horizontal",
      label.position = "top",
      barwidth       = unit(0.8, "npc"),
      barheight      = unit(0.45, "cm"),
      ticks          = TRUE,
      ticks.colour   = "grey35",
      frame.colour   = "grey35",
      order          = 2
    )
  ) +
  theme_void() +
  theme(
    plot.margin          = margin(12, 12, 12, 12),
    legend.position      = "bottom",
    legend.box           = "horizontal",
    legend.box.just      = "center",
    legend.justification = "center",
    legend.spacing.x     = unit(0.6, "cm"),
    legend.text          = element_text(size = 11)
  )

map_share


#Save figure:
ggsave(
  "/Users/almaowing/Documents/MAthesis/figures/map_share.pdf",
  plot   = map_share,
  width  = 10,
  height = 6,
  units  = "in"
)

#-------------------------------------------------------------------------------

#Figure 4.4: student involvement figure:

#Build distribution data:
fig_index_dist_data <- omg %>%
  filter(!is.na(student_index)) %>%
  mutate(index_f = factor(student_index, levels = 0:3)) %>%
  count(index_f) %>%
  mutate(
    pct       = n / sum(n),
    label_top = scales::comma(n)
  )


#Plot:
fig_index_dist <- ggplot(fig_index_dist_data,
                         aes(x = index_f, y = n, fill = index_f)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = label_top),
            vjust = -0.7, size = 3.8, fontface = "bold", color = "grey20") +
  scale_x_discrete(labels = c(
    "0" = "No involvement",
    "1" = "Low\n(1 criterion)",
    "2" = "Moderate\n(2 criteria)",
    "3" = "High\n(3 criteria)"
  )) +
  scale_fill_manual(values = c(
    "0" = "#D5D8DC",
    "1" = "#A8C4D4",
    "2" = "#4A7FA5",
    "3" = "#1B3A6B"
  )) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    x = "Student involvement level",
    y = "Number of campaigns"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.title.x       = element_text(margin = margin(t = 12), color = "grey30"),
    axis.title.y       = element_text(margin = margin(r = 10), color = "grey30"),
    axis.text.x        = element_text(color = "grey20", lineheight = 1.3),
    axis.text.y        = element_text(color = "grey50"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    axis.line.x        = element_line(color = "grey80", linewidth = 0.4),
    plot.margin        = margin(10, 15, 10, 10)
  )

fig_index_dist


#Save figure:
ggsave(
  "/Users/almaowing/Documents/MAthesis/figures/fig_index_dist.pdf",
  plot = fig_index_dist,
  width = 7, height = 5, units = "in"
)

#-------------------------------------------------------------------------------

# Table: campaigns by dominant social group (1990-2019) (appendix):

#Mapping of group dummies to display labels:
dom_vars <- c(
  "Students"                     = "dominate_students",
  "Religious/ethnic groups"      = "dominate_relethnic",
  "Intellectuals"                = "dominate_intellectuals",
  "Workers in general"           = "dominate_workers_general",
  "Professionals"                = "dominate_professionals",
  "Urban middle class"           = "dominate_urb_middle_class",
  "Military"                     = "dominate_milemp",
  "Peasants"                     = "dominate_peasant",
  "Industrial workers"           = "dominate_indwork",
  "Non-industrial urban workers" = "dominate_nonindurban",
  "Public employees"             = "dominate_pubemp",
  "Business elites"              = "dominate_business",
  "Rural groups"                 = "dominate_rural",
  "Agrarian elites"              = "dominate_agrarianelites"
)

#restrict to 1990-2019:
omg_9019 <- omg %>% filter(start_year >= 1990 & start_year <= 2019)

#build table:
dom_table <- imap_dfr(dom_vars, ~ omg_9019 %>%
  filter(.data[[.x]] == 1) %>%
  summarise(Group = .y, Campaigns = n_distinct(id), Countries = n_distinct(country_name))
) %>% arrange(desc(Campaigns))

print(dom_table)

#Save table:
kable(dom_table, format = "latex", booktabs = TRUE, align = "lcc",
      caption = "Dominant social groups in mass mobilization campaigns, 1990--2019",
      label  = "dom_groups_9019") %>%
  kable_styling(latex_options = "hold_position") %>%
  as.character() %>%
  writeLines("/Users/almaowing/Documents/MAthesis/tables/table_dom_groups_9019.tex")



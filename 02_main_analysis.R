# Master thesis project: "Are Student-Led Protests a Force for Democracy?"
# By: Alma Eckhoff Owing
# Date: May, 15th, 2026

# This script consists of the analyses presented in the thesis. 
# R version 4.5.1 (2025-06-13)

#-------------------------------------------------------------------------------

# Loading relevant packages:
library(tidyverse)
library(zoo)
library(scales)
library(ggrepel)
library(fixest)
library(plm)
library(lmtest)
library(sandwich)
library(car)
library(marginaleffects)
library(robomit)
library(modelsummary)
library(kableExtra)
library(xtable)

#-------------------------------------------------------------------------------

# Load the data (set to your own working directory):
omg    <- readRDS("/Users/almaowing/Documents/MAthesis/data/R/omg.rds")
cy_omg <- readRDS("/Users/almaowing/Documents/MAthesis/data/R/cy_omg.rds")
vdem   <- readRDS("/Users/almaowing/Documents/MAthesis/data/R/V-Dem-CY-Full+Others-v15.rds")
wb_urban <- read.csv(
  "/Users/almaowing/Documents/MAthesis/data/R/API_SP-2/API_SP.URB.TOTL.IN.ZS_DS2_en_csv_v2_249.csv",
  skip = 4
)
wb_growth <- read.csv(
  "/Users/almaowing/Documents/MAthesis/data/R/API_NY_growth/API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2_260.csv",
  skip = 4
)
owid_educ_raw <- read.csv(
  "/Users/almaowing/Documents/MAthesis/data/R/mean-years-of-schooling-long-run.csv"
)
wb_resources_raw <- read.csv(
  "/Users/almaowing/Documents/MAthesis/data/R/API_NY/API_NY.GDP.TOTL.RT.ZS_DS2_en_csv_v2_1083.csv",
  skip = 4
)
#-------------------------------------------------------------------------------

# Creating independent and dependent variables:

# democratic demand (binary)
omg$dem_demand_any <- as.numeric(
  omg$demand_civilrights == 1 |
    omg$demand_free_expression == 1 |
    omg$demand_election == 1 |
    omg$demand_executive == 1 |
    omg$demand_main_institutional == 1 |
    omg$demand_demo == 1
)
omg$dem_demand_any[is.na(omg$dem_demand_any)] <- 0

# student protest (dominate only)
omg$student_protest <- as.numeric(omg$dominate_students == 1)
omg$student_protest[is.na(omg$student_protest)] <- 0

# student index (0-3)
omg <- omg %>%
  mutate(
    student_index = as.integer(atleast_students == 1) +
      as.integer(originate_students == 1) +
      as.integer(dominate_students == 1)
  )
omg$student_index[is.na(omg$student_index)] <- 0

# check
table(omg$student_protest)
table(omg$student_index)
table(omg$dem_demand_any)

#-------------------------------------------------------------------------------

#DATA PREP

#Data prep for H1:
# Merge V-Dem into OMG (campaign-level):
vdem_small <- vdem %>%
  select(country_id, year,
         v2x_polyarchy, e_gdppc, 
         v2csreprss, v2x_corr)   

omg <- omg %>%
  left_join(vdem_small, by = c("country_id", "start_year" = "year"))


# Collapse to campaign-level:
agg_any1 <- function(x) as.integer(any(x == 1, na.rm = TRUE))

omg_campaign <- omg %>%
  group_by(id) %>%
  summarise(
    # identifiers
    country        = first(country_name),
    country_id     = first(country_id),
    start_year     = min(start_year, na.rm = TRUE),
    
    # dependent variable
    dem_demand_any = max(dem_demand_any, na.rm = TRUE),
    
    # independent variables
    student_protest = max(student_protest, na.rm = TRUE),
    student_index   = max(student_index,   na.rm = TRUE),
    
    # regime context
    regime_context = first(v2x_polyarchy),
    e_gdppc        = first(e_gdppc),
    
    # social group controls
    across(starts_with("dominate_"), agg_any1),
    
    # campaign size
    peak_size  = suppressWarnings(max(peak_participant_size, na.rm = TRUE)),
    
    # campaign tactic (potential mediator)
    nonviolent = max(campaign_strategy_nonviolent, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    decade    = floor(start_year / 10) * 10,
    peak_size = ifelse(is.infinite(peak_size), NA, peak_size),
    log_gdppc = log(e_gdppc),
    
    working_group = as.integer(
      dominate_peasant == 1 | dominate_rural == 1 |
        dominate_indwork == 1 | dominate_nonindurban == 1 |
        dominate_workers_general == 1),
    
    educated_group = as.integer(
      dominate_professionals == 1 | dominate_intellectuals == 1 |
        dominate_urb_middle_class == 1 | dominate_pubemp == 1),
    
    elite_group = as.integer(
      dominate_business == 1 | dominate_agrarianelites == 1),
    
    identity_group = as.integer(dominate_relethnic == 1)
  ) %>%
  filter(!is.na(regime_context))


# check
nrow(omg_campaign)
table(omg_campaign$student_protest)
table(omg_campaign$student_index)
table(omg_campaign$dem_demand_any)


# Data prep for H2:

# Wrangle and merge WB into cy_omg:
wb_urban_long <- wb_urban %>%
  select(Country.Code, starts_with("X")) %>%
  select(-X) %>%
  pivot_longer(
    cols      = starts_with("X"),
    names_to  = "year",
    values_to = "wb_urbanization"
  ) %>%
  mutate(year = as.numeric(gsub("X", "", year))) %>%
  rename(country_text_id = Country.Code)

cy_omg <- cy_omg %>%
  ungroup() %>%
  select(-any_of("wb_urbanization")) %>%
  left_join(wb_urban_long, by = c("country_text_id", "year"))


# Wrangle and merge wb_growth 
# WB reports GDP growth in percentages (e.g., 3.5 for 3.5%). Divide by 100
# so it matches the decimal scale of the V-Dem-gdp_growth variable.
wb_growth_long <- wb_growth %>%
  select(Country.Code, starts_with("X")) %>%
  select(-X) %>%
  pivot_longer(
    cols      = starts_with("X"),
    names_to  = "year",
    values_to = "wb_gdp_growth"
  ) %>%
  mutate(
    year          = as.numeric(gsub("X", "", year)),
    wb_gdp_growth = wb_gdp_growth / 100
  ) %>%
  rename(country_text_id = Country.Code)

cy_omg <- cy_omg %>%
  ungroup() %>%
  select(-any_of("wb_gdp_growth")) %>%
  left_join(wb_growth_long, by = c("country_text_id", "year"))


# create variables:
cy_omg$dominate_students_count[is.na(cy_omg$dominate_students_count)]   <- 0
cy_omg$atleast_students_count[is.na(cy_omg$atleast_students_count)]     <- 0
cy_omg$originate_students_count[is.na(cy_omg$originate_students_count)] <- 0

cy_omg <- cy_omg %>%
  ungroup() %>%
  mutate(
    # main independent variable: student dominates
    student_protest = as.integer(dominate_students_count > 0),
    any_campaign    = as.integer(count_movements > 0),
    campaign_type   = case_when(
      student_protest == 1                     ~ "student",
      any_campaign == 1 & student_protest == 0 ~ "other_campaign",
      TRUE                                     ~ "no_campaign"
    ),
    campaign_type = relevel(factor(campaign_type), ref = "no_campaign"),
    
    # student involvement index (0-3)
    student_index_cy = as.integer(atleast_students_count > 0) +
      as.integer(originate_students_count > 0) +
      as.integer(dominate_students_count > 0),
    
    # economic controls
    log_gdppc = log(e_gdppc),
    log_pop   = log(e_pop)
  )


# create lag and lead variables:
cy_omg <- cy_omg %>%
  arrange(country_id, year) %>%
  group_by(country_id) %>%
  mutate(
    gdp_growth             = (e_gdppc - dplyr::lag(e_gdppc)) / dplyr::lag(e_gdppc),
    electoral_democracy_t1 = dplyr::lead(v2x_polyarchy, 1),
    electoral_democracy_t5 = dplyr::lead(v2x_polyarchy, 5)
  ) %>%
  ungroup()


# lets see:
table(cy_omg$campaign_type)
table(cy_omg$student_index_cy)
summary(cy_omg$gdp_growth)


#-------------------------------------------------------------------------------
# share of missing values by regime-type:

#helper function: compute missing % per regime + total:
compute_missing <- function(data, vars, regime_var) {
  data_with_regime <- data %>%
    mutate(regime_type = case_when(
      .data[[regime_var]] <  0.2 ~ "Autocracy",
      .data[[regime_var]] >= 0.2 & .data[[regime_var]] <= 0.5 ~ "Hybrid",
      .data[[regime_var]] >  0.5 ~ "Democracy",
      TRUE ~ NA_character_
    ))
  
  per_regime <- data_with_regime %>%
    filter(!is.na(regime_type)) %>%
    group_by(regime_type) %>%
    summarise(across(all_of(unname(vars)), ~ round(mean(is.na(.x)) * 100, 1)),
              .groups = "drop") %>%
    pivot_longer(-regime_type, names_to = "var", values_to = "pct") %>%
    pivot_wider(names_from = regime_type, values_from = pct)
  
  total <- data %>%
    summarise(across(all_of(unname(vars)), ~ round(mean(is.na(.x)) * 100, 1))) %>%
    pivot_longer(everything(), names_to = "var", values_to = "Total")
  
  per_regime %>%
    left_join(total, by = "var") %>%
    mutate(Variable = names(vars)[match(var, unname(vars))]) %>%
    select(Variable, Autocracy, Hybrid, Democracy, Total)
}


#H1 variables (campaign-level):
h1_vars <- c(
  "Democratic demand (any)"        = "dem_demand_any",
  "Student-led protest"            = "student_protest",
  "Student involvement index"      = "student_index",
  "Electoral democracy (V-Dem)"    = "regime_context",
  "GDP per capita (log)"           = "log_gdppc",
  "Campaign size"                  = "peak_size",
  "Nonviolent campaign"            = "nonviolent",
  "Working groups dominate"        = "working_group",
  "Educated urban groups dominate" = "educated_group",
  "Elite groups dominate"          = "elite_group",
  "Identity groups dominate"       = "identity_group"
)


#H2 variables (country-year):
h2_vars <- c(
  "Electoral democracy t+1 (DV)"   = "electoral_democracy_t1",
  "Electoral democracy (lagged)"   = "v2x_polyarchy",
  "Student-led protest"            = "student_protest",
  "Student involvement index"      = "student_index_cy",
  "GDP per capita (log)"           = "log_gdppc",
  "GDP growth"                     = "gdp_growth",
  "Population (log)"               = "log_pop",
  "Urbanization (V-Dem)"           = "e_miurbani"
)


#compute missingness:
missing_h1 <- compute_missing(omg_campaign, h1_vars, "regime_context")
missing_h2 <- compute_missing(cy_omg,       h2_vars, "v2x_polyarchy")

cat("\n--- H1: Campaign-level missingness ---\n")
print(missing_h1)
cat("\n--- H2: Country-year missingness ---\n")
print(missing_h2)


#build combined table for export:
miss_combined <- bind_rows(
  tibble(Variable = "\\textbf{H1: Campaign level}",
         Autocracy = NA_real_, Hybrid = NA_real_, Democracy = NA_real_, Total = NA_real_),
  missing_h1 %>% mutate(Variable = paste0("\\quad ", Variable)),
  tibble(Variable = "\\textbf{H2: Country-year panel}",
         Autocracy = NA_real_, Hybrid = NA_real_, Democracy = NA_real_, Total = NA_real_),
  missing_h2 %>% mutate(Variable = paste0("\\quad ", Variable))
)

# convert NA to empty string for LaTeX
miss_combined_export <- miss_combined %>%
  mutate(across(c(Autocracy, Hybrid, Democracy, Total),
                ~ ifelse(is.na(.x), "", sprintf("%.1f", .x))))


#export to LaTeX:
kbl(
  miss_combined_export,
  format    = "latex",
  booktabs  = TRUE,
  align     = "lrrrr",
  escape    = FALSE,
  caption   = "Share of Missing Observations by Regime Type (\\%)",
  label     = "missing_by_regime",
  col.names = c("Variable", "Autocracy", "Hybrid", "Democracy", "Total")
) %>%
  kable_styling(latex_options = "hold_position") %>%
  footnote(
    general = "Share of missing observations (in percent) for each variable, broken down by regime type. Regime type is defined by V-Dem polyarchy score: autocracy ($<0.2$), hybrid ($0.2$--$0.5$), and democracy ($>0.5$). Missing data are not random but concentrated in autocracies, particularly for control variables drawn from V-Dem (urbanization, GDP per capita, population). For the limitation discussion, see Section 7.3.1.",
    general_title     = "Note:",
    footnote_as_chunk = TRUE,
    threeparttable    = TRUE,
    escape            = FALSE
  ) %>%
  save_kable("/Users/almaowing/Documents/MAthesis/tables/table_missing_by_regime_1.tex")


#-------------------------------------------------------------------------------

# H1 ANALYSIS (campaign-level)

#Descriptive statistics:

var_info_h1 <- data.frame(
  var_name = c(
    "dem_demand_any", "student_protest", "student_index",
    "regime_context", "working_group", "educated_group",
    "elite_group", "identity_group", "log_gdppc",
    "peak_size", "nonviolent"
  ),
  label = c(
    "Democratic demand (any)", "Student-led protest",
    "Student involvement index (0--3)",
    "Electoral democracy (V-Dem)", "Working groups dominate",
    "Educated urban groups dominate",
    "Elite groups dominate", "Identity groups dominate",
    "GDP per capita (log)",
    "Campaign size", "Nonviolent campaign"
  ),
  type = c(
    "Binary", "Binary", "Count",
    "Continuous", "Binary", "Binary",
    "Binary", "Binary", "Continuous",
    "Continuous", "Binary"
  ),
  stringsAsFactors = FALSE
)

desc_h1 <- do.call(rbind, lapply(seq_len(nrow(var_info_h1)), function(i) {
  x <- omg_campaign[[ var_info_h1$var_name[i] ]]
  x <- x[!is.na(x)]
  data.frame(
    Variable = var_info_h1$label[i],
    Type     = var_info_h1$type[i],
    Min      = round(min(x),  2),
    Mean     = round(mean(x), 2),
    Max      = round(max(x),  2),
    stringsAsFactors = FALSE
  )
}))

#export to LaTex:
kbl(
  desc_h1,
  format   = "latex",
  booktabs = TRUE,
  caption  = "Descriptive Statistics -- Campaign Level (H1)",
  label    = "desc_h1",
  linesep  = c("", "", "\\addlinespace", "", "", "", "", "", "")
) %>%
  kable_styling(latex_options = "hold_position") %>%
  save_kable("/Users/almaowing/Documents/MAthesis/tables/desc_h1.tex")

# DV frequency (LPM justification):

freq_counts <- as.integer(table(omg_campaign$dem_demand_any))

dv_freq <- data.frame(
  " "   = c("Frequency"),
  "0"   = freq_counts[1],
  "1"   = freq_counts[2],
  check.names = FALSE
)

kbl(
  dv_freq,
  format   = "latex",
  booktabs = TRUE,
  caption  = "Distribution of Dependent Variable: Democratic Demand (Any)",
  label    = "dv_freq",
  linesep  = "",
  col.names = c("Democratic demand (any)", "0", "1")
) %>%
  kable_styling(latex_options = "hold_position") %>%
  save_kable("/Users/almaowing/Documents/MAthesis/tables/dv_freq.tex")

#-------------------------------------------------------------------------------

# LPM MODELS:

#lpm-models: binary student-led (M1-M4):

# m1: baseline, no controls or fixed effects
model1 <- lm(dem_demand_any ~ student_protest,
             data = omg_campaign)

# m2: + group controls + fixed effects
model2 <- lm(dem_demand_any ~ student_protest +
               working_group + educated_group +
               elite_group + identity_group +
               factor(country) + factor(decade),
             data = omg_campaign)

# m3: + regime context + gdp
model3 <- lm(dem_demand_any ~ student_protest +
               working_group + educated_group +
               elite_group + identity_group +
               regime_context + log_gdppc +
               factor(country) + factor(decade),
             data = omg_campaign)

# m4: + size and nonviolent (potential mediators)
model4 <- lm(dem_demand_any ~ student_protest +
               working_group + educated_group +
               elite_group + identity_group +
               regime_context + log_gdppc + peak_size + nonviolent +
               factor(country) + factor(decade),
             data = omg_campaign)


# lpm models: student_index (M5-M8) 

# m5: baseline, no controls or fixed effects
model5_index <- lm(dem_demand_any ~ student_index,
                   data = omg_campaign)

# m6: + group controls + fixed effects
model6_index <- lm(dem_demand_any ~ student_index +
                     working_group + educated_group +
                     elite_group + identity_group +
                     factor(country) + factor(decade),
                   data = omg_campaign)

# m7: + regime context + gdp
model7_index <- lm(dem_demand_any ~ student_index +
                     working_group + educated_group +
                     elite_group + identity_group +
                     regime_context + log_gdppc +
                     factor(country) + factor(decade),
                   data = omg_campaign)

# m8: + size and nonviolent (potential mediators)
model8_index <- lm(dem_demand_any ~ student_index +
                     working_group + educated_group +
                     elite_group + identity_group +
                     regime_context + log_gdppc + peak_size + nonviolent +
                     factor(country) + factor(decade),
                   data = omg_campaign)



# clustered standard errors 
vcov1       <- vcovCL(model1,       cluster = ~ country)
vcov2       <- vcovCL(model2,       cluster = ~ country)
vcov3       <- vcovCL(model3,       cluster = ~ country)
vcov4       <- vcovCL(model4,       cluster = ~ country)
vcov5_index <- vcovCL(model5_index, cluster = ~ country)
vcov6_index <- vcovCL(model6_index, cluster = ~ country)
vcov7_index <- vcovCL(model7_index, cluster = ~ country)
vcov8_index <- vcovCL(model8_index, cluster = ~ country)


# quick results check
coeftest(model4,       vcov = vcov4)
coeftest(model8_index, vcov = vcov8_index)

# Export table:
# coefficient labels 
coef_labels_h1 <- c(
  "(Intercept)"     = "Intercept",
  "student_protest" = "Student-led protest",
  "student_index"   = "Student involvement index",
  "working_group"   = "Working groups dominate",
  "educated_group"  = "Educated urban groups dominate",
  "elite_group"     = "Elite groups dominate",
  "identity_group"  = "Identity groups dominate",
  "regime_context"  = "Electoral democracy (V-Dem)",
  "log_gdppc"       = "GDP per capita (log)",
  "peak_size"       = "Campaign size",
  "nonviolent"      = "Nonviolent campaign"
)


# fixed effects rows
fe_rows_h1 <- data.frame(
  term  = c("Country FE", "Decade FE"),
  "M1"  = c("No",  "No"),
  "M2"  = c("Yes", "Yes"),
  "M3"  = c("Yes", "Yes"),
  "M4"  = c("Yes", "Yes"),
  "M5"  = c("No",  "No"),
  "M6"  = c("Yes", "Yes"),
  "M7"  = c("Yes", "Yes"),
  "M8"  = c("Yes", "Yes")
)


# export main table:
modelsummary(
  list("M1" = model1,       "M2" = model2,
       "M3" = model3,       "M4" = model4,
       "M5" = model5_index, "M6" = model6_index,
       "M7" = model7_index, "M8" = model8_index),
  vcov      = list(vcov1, vcov2, vcov3, vcov4,
                   vcov5_index, vcov6_index, vcov7_index, vcov8_index),
  stars     = TRUE,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_omit = "factor\\(country\\)|factor\\(decade\\)",
  coef_map  = coef_labels_h1,
  gof_map   = c("nobs", "r.squared"),
  add_rows  = fe_rows_h1,
  output    = "/Users/almaowing/Documents/MAthesis/tables/table_h1_main.tex",
  title     = "Student-led protests and democratic demands (H1, LPM)",
  notes     = "Cluster-robust standard errors by country in parentheses. LPM = linear probability model. M1--M4 use student domination as the main indicator; M5--M8 use the student involvement index (0--3). "
)


# ── wrap table in scalebox så den passer på siden ──────────────────────────
h1_tex <- readLines("/Users/almaowing/Documents/MAthesis/tables/table_h1_main.tex")
h1_tex <- gsub("\\\\centering", "\\\\centering\n\\\\scalebox{0.82}{", h1_tex)
h1_tex <- gsub("\\\\end\\{talltblr\\}", "\\\\end{talltblr}\n}", h1_tex)
writeLines(h1_tex, "/Users/almaowing/Documents/MAthesis/tables/table_h1_main.tex")

#-------------------------------------------------------------------------------

#Figures (predicted probability plots):

#figure 1: student_protest (0 vs 1) 
pred_protest <- predictions(
  model3,
  newdata = datagrid(student_protest = c(0, 1)),
  vcov    = vcov3
)

fig_pred_protest <- ggplot(
  as.data.frame(pred_protest),
  aes(x = factor(student_protest,
                 levels = c(0, 1),
                 labels = c("Other campaign\n(non-student-led)", "Student-led campaign")),
      y = estimate, ymin = conf.low, ymax = conf.high)
) +
  geom_errorbar(width = 0.08, linewidth = 0.3) +
  geom_point(size = 3.5, shape = 19) +
  scale_y_continuous(breaks = seq(0.2, 0.8, by = 0.2),
                     limits = c(0.15, 0.80)) +
  labs(
    x = "Campaign type",
    y = "Predicted probability of democratic demand"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.border       = element_blank(),
    axis.line.x        = element_line(colour = "black", linewidth = 0.4),
    axis.line.y        = element_line(colour = "black", linewidth = 0.4),
    axis.title.x       = element_text(size = 10, colour = "black", margin = margin(t = 8)),
    axis.title.y       = element_text(size = 10, colour = "black", margin = margin(r = 8)),
    axis.text          = element_text(size = 10, colour = "grey50"),
    plot.margin        = margin(10, 15, 10, 10)
  )

ggsave("/Users/almaowing/Documents/MAthesis/figures/fig_pred_protest.pdf",
       fig_pred_protest, width = 4, height = 3.8)


#figure 1b: contrast version:
contrast_protest <- avg_comparisons(
  model3,
  variables = "student_protest",
  vcov      = vcov3
)

fig_pred_protest_contrast <- ggplot(
  as.data.frame(contrast_protest),
  aes(x = "Student-led vs\nnon-student-led campaign",
      y = estimate, ymin = conf.low, ymax = conf.high)
) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50", linetype = "dashed") +
  geom_errorbar(width = 0.08, linewidth = 0.3) +
  geom_point(size = 3.5, shape = 19) +
  labs(
    x = NULL,
    y = "Difference in predicted probability\nof democratic demand"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.border       = element_blank(),
    axis.line.x        = element_line(colour = "black", linewidth = 0.4),
    axis.line.y        = element_line(colour = "black", linewidth = 0.4),
    axis.title.y       = element_text(size = 10, colour = "black", margin = margin(r = 8)),
    axis.text          = element_text(size = 10, colour = "grey50"),
    plot.margin        = margin(10, 15, 10, 10)
  )

ggsave("/Users/almaowing/Documents/MAthesis/figures/fig_pred_protest_contrast.pdf",
       fig_pred_protest_contrast, width = 4, height = 3.8)


#figure 2: student_index (0-3) 
pred_index <- predictions(
  model7_index,
  newdata = datagrid(student_index = 0:3),
  vcov    = vcov7_index
)

fig_pred_index <- ggplot(
  as.data.frame(pred_index),
  aes(x = student_index, y = estimate, ymin = conf.low, ymax = conf.high)
) +
  geom_errorbar(width = 0.08, linewidth = 0.3) +
  geom_line(linewidth = 0.5, colour = "black") +
  geom_point(size = 3.5, shape = 19) +
  scale_x_continuous(breaks = 0:3) +
  labs(
    x = "Student involvement index",
    y = "Predicted probability of democratic demand"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.border       = element_blank(),
    axis.line.x        = element_line(colour = "black", linewidth = 0.4),
    axis.line.y        = element_line(colour = "black", linewidth = 0.4),
    axis.title.x       = element_text(size = 10, colour = "black", margin = margin(t = 8)),
    axis.title.y       = element_text(size = 10, colour = "black", margin = margin(r = 8)),
    axis.text          = element_text(size = 10, colour = "grey50"),
    plot.margin        = margin(10, 15, 10, 10)
  )

ggsave("/Users/almaowing/Documents/MAthesis/figures/fig_pred_index.pdf",
       fig_pred_index, width = 4, height = 3.8)

#-------------------------------------------------------------------------------

#VIF TEST:
h1_vif_protest <- lm(
  dem_demand_any ~ student_protest + working_group + educated_group +
    elite_group + identity_group + regime_context + log_gdppc +
    peak_size + nonviolent,
  data = omg_campaign
)

h1_vif_index <- lm(
  dem_demand_any ~ student_index + working_group + educated_group +
    elite_group + identity_group + regime_context + log_gdppc +
    peak_size + nonviolent,
  data = omg_campaign
)


# consoll-check:
cat("H1 VIF - student protest model:\n"); print(vif(h1_vif_protest))
cat("\nH1 VIF - student index model:\n");  print(vif(h1_vif_index))



# export to LaTex table:
v_p <- vif(h1_vif_protest)
v_i <- vif(h1_vif_index)

vif_h1_labels <- c(
  "student_protest" = "Student-led protest",
  "student_index"   = "Student involvement index",
  "working_group"   = "Working groups dominate",
  "educated_group"  = "Educated urban groups dominate",
  "elite_group"     = "Elite groups dominate",
  "identity_group"  = "Identity groups dominate",
  "regime_context"  = "Electoral democracy (V-Dem)",
  "log_gdppc"       = "GDP per capita (log)",
  "peak_size"       = "Campaign size",
  "nonviolent"      = "Nonviolent campaign"
)

vif_h1_tab <- tibble(
  Variable       = vif_h1_labels[union(names(v_p), names(v_i))],
  `M3 (protest)` = round(v_p[union(names(v_p), names(v_i))], 2),
  `M7 (index)`   = round(v_i[union(names(v_p), names(v_i))], 2)
)

kable(vif_h1_tab, format = "latex", booktabs = TRUE, align = "lcc",
      caption = "Variance inflation factors for H1 covariates",
      label   = "vif_h1") %>%
  kable_styling(latex_options = "hold_position") %>%
  footnote(general = "VIF computed from OLS specifications without country and decade fixed effects.",
           general_title = "Note:", footnote_as_chunk = TRUE, threeparttable = TRUE) %>%
  save_kable("/Users/almaowing/Documents/MAthesis/tables/table_vif_h1.tex")


#-------------------------------------------------------------------------------

# Wald test (linear hypothesis test) (H1):
h1_lh_pairs <- c("working_group", "educated_group", "elite_group", "identity_group")

h1_lh_labels <- c(
  "working_group"   = "Working groups dominate",
  "educated_group"  = "Educated urban groups dominate",
  "elite_group"     = "Elite groups dominate",
  "identity_group"  = "Identity groups dominate"
)

h1_lh_run <- function(mod, vcov_mat, iv) {
  map_dfr(h1_lh_pairs, function(other) {
    lh <- linearHypothesis(mod, paste(iv, "=", other),
                           vcov. = vcov_mat, test = "Chisq")
    tibble(
      Comparison = paste0(
        ifelse(iv == "student_protest", "Student-led protest",
               "Student involvement index"),
        " = ", h1_lh_labels[[other]]
      ),
      Chisq   = round(lh$Chisq[2], 3),
      df      = lh$Df[2],
      p_value = lh$`Pr(>Chisq)`[2]
    )
  })
}

lh_h1_m3 <- h1_lh_run(model3,       vcov3,       "student_protest")
lh_h1_m7 <- h1_lh_run(model7_index, vcov7_index, "student_index")

lh_h1_tab <- bind_rows(
  lh_h1_m3 %>% mutate(Model = "M3 (protest)"),
  lh_h1_m7 %>% mutate(Model = "M7 (index)")
) %>%
  mutate(
    stars = case_when(p_value < 0.001 ~ "***",
                      p_value < 0.01  ~ "**",
                      p_value < 0.05  ~ "*",
                      p_value < 0.10  ~ "+",
                      TRUE            ~ ""),
    `p-value` = paste0(sprintf("%.3f", p_value), stars)
  ) %>%
  select(Model, Comparison, Chisq, df, `p-value`)

print(lh_h1_tab)

kable(lh_h1_tab, format = "latex", booktabs = TRUE,
      align = "llccl",
      col.names = c("Model", "Comparison", "$\\chi^2$", "df", "$p$-value"),
      escape = FALSE,
      caption = "Linear hypothesis tests for H1: equality of student and other-group coefficients",
      label   = "lh_h1") %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  footnote(general = "Wald tests of $H_0$: $\\beta_{\\text{student}} = \\beta_{\\text{group}}$ for each social group control in Models 3 and 7. Cluster-robust covariance matrix clustered by country. Significance: $+\\,p<0.10$, $*\\,p<0.05$, $**\\,p<0.01$, $***\\,p<0.001$.",
           general_title = "Note:", footnote_as_chunk = TRUE,
           threeparttable = TRUE, escape = FALSE) %>%
  save_kable("/Users/almaowing/Documents/MAthesis/tables/table_lh_h1.tex")

#-------------------------------------------------------------------------------

# Oster sensitivity test:

#1. M0: simple model without controls or FE (gives β° and R²₀)
m0_h1 <- lm(dem_demand_any ~ student_protest, data = omg_campaign)

beta_uncontrolled <- coef(m0_h1)["student_protest"]
r2_uncontrolled   <- summary(m0_h1)$r.squared

beta_controlled   <- coef(model3)["student_protest"]
r2_controlled     <- summary(model3)$r.squared


#2. Standard Oster benchmark: Rmax = 1.3 × R² of full model, capped at 1
r2_max <- min(1.3 * r2_controlled, 0.99)

cat("\n--- Oster sensitivity: starting values ---\n")
cat(sprintf("β° (uncontrolled):  %.4f   R²₀: %.4f\n",
            beta_uncontrolled, r2_uncontrolled))
cat(sprintf("β̂ (controlled):    %.4f   R̃²: %.4f\n",
            beta_controlled,   r2_controlled))
cat(sprintf("Rmax (1.3 × R̃²):   %.4f\n", r2_max))


#3. δ, how strong must unobserved-selection bias be for β = 0?
oster_delta_h1 <- o_delta(
  y     = "dem_demand_any",
  x     = "student_protest",
  con   = "working_group + educated_group + elite_group + identity_group + regime_context + log_gdppc + factor(country) + factor(decade)",
  beta  = 0,
  R2max = r2_max,
  type  = "lm",
  data  = omg_campaign
)

cat("\n--- δ: how much unobserved-selection bias is needed for β = 0? ---\n")
print(oster_delta_h1)


#4. β*: bias-adjusted coefficient under δ = 1, Rmax = 1.3 × R²
oster_beta_h1 <- o_beta(
  y     = "dem_demand_any",
  x     = "student_protest",
  con   = "working_group + educated_group + elite_group + identity_group + regime_context + log_gdppc + factor(country) + factor(decade)",
  delta = 1,
  R2max = r2_max,
  type  = "lm",
  data  = omg_campaign
)

cat("\n--- β*: bias-adjusted coefficient (δ = 1, Rmax = 1.3 × R̃²) ---\n")
print(oster_beta_h1)


#5. Robustness interval: [β*, β̂]
beta_star_value <- oster_beta_h1$Value[1]
delta_value     <- oster_delta_h1$Value[1]

cat("\n--- Robustness interval ---\n")
cat(sprintf("[β*, β̂] = [%.4f, %.4f]\n", beta_star_value, beta_controlled))
if (sign(beta_star_value) == sign(beta_controlled)) {
  cat("→ Interval does NOT cross zero. Result is robust.\n")
} else {
  cat("→ WARNING: Interval crosses zero. Result is NOT robust.\n")
}


#6. Export table to appendix 
oster_table <- data.frame(
  Quantity = c("β° (uncontrolled)", "β̂ (controlled, M3)",
               "R²₀ (uncontrolled)", "R̃² (controlled)",
               "Rmax (1.3 × R̃²)",
               "δ (selection ratio for β = 0)",
               "β* (bias-adjusted, δ = 1)",
               "Robustness interval [β*, β̂]"),
  Value    = c(sprintf("%.4f", beta_uncontrolled),
               sprintf("%.4f", beta_controlled),
               sprintf("%.4f", r2_uncontrolled),
               sprintf("%.4f", r2_controlled),
               sprintf("%.4f", r2_max),
               sprintf("%.2f", delta_value),
               sprintf("%.4f", beta_star_value),
               sprintf("[%.4f, %.4f]", beta_star_value, beta_controlled))
)

write.csv(oster_table,
          "/Users/almaowing/Documents/MAthesis/tables/table_h1_oster.csv",
          row.names = FALSE)

cat("\n--- Table exported to tables/table_h1_oster.csv ---\n")
print(oster_table)

#-------------------------------------------------------------------------------

# H2 ANALYSIS (county-year):

# Descriptive statistics:
var_info_h2 <- data.frame(
  var_name = c(
    "electoral_democracy_t1", "student_protest", "student_index_cy",
    "v2x_polyarchy", "log_gdppc", "gdp_growth", "log_pop", "e_miurbani"
  ),
  label = c(
    "Electoral democracy $t+1$ (DV)", "Student-led protest",
    "Student involvement index (0--3)",
    "Electoral democracy (lagged)", "GDP per capita (log)",
    "GDP growth", "Population (log)", "Urbanization (V-Dem)"
  ),
  type = c(
    "Continuous", "Binary", "Count",
    "Continuous", "Continuous", "Continuous", "Continuous", "Continuous"
  ),
  stringsAsFactors = FALSE
)

desc_h2 <- do.call(rbind, lapply(seq_len(nrow(var_info_h2)), function(i) {
  x <- cy_omg[[ var_info_h2$var_name[i] ]]
  x <- x[!is.na(x)]
  data.frame(
    Variable = var_info_h2$label[i],
    Type     = var_info_h2$type[i],
    Min      = round(min(x),  2),
    Mean     = round(mean(x), 2),
    Max      = round(max(x),  2),
    stringsAsFactors = FALSE
  )
}))


# export to LaTex:
kbl(
  desc_h2,
  format   = "latex",
  booktabs = TRUE,
  caption  = "Descriptive Statistics -- Country-Year Panel (H2)",
  label    = "desc_h2",
  linesep  = c("", "", "\\addlinespace", "", "", "", "", "")
) %>%
  kable_styling(latex_options = "hold_position") %>%
  save_kable("/Users/almaowing/Documents/MAthesis/tables/desc_h2.tex")

#-------------------------------------------------------------------------------

# Serial correlation test (H2):
cy_panel <- pdata.frame(
  cy_omg %>% filter(!is.na(electoral_democracy_t1)),
  index = c("country_id", "year")
)

sc_test <- plm(
  electoral_democracy_t1 ~ campaign_type + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani,
  data   = cy_panel,
  model  = "within",
  effect = "twoways"
)

print(pbgtest(sc_test))
# -> if p < 0.05: serial correlation confirmed -> clustered SEs justified


#export to LaTeX:
sc_result <- pbgtest(sc_test)

sc_tab <- tibble(
  Test       = "Breusch--Godfrey/Wooldridge test for serial correlation",
  Statistic  = round(as.numeric(sc_result$statistic), 3),
  df         = as.numeric(sc_result$parameter),
  `p-value`  = format.pval(sc_result$p.value, digits = 3, eps = 0.001),
  Conclusion = ifelse(sc_result$p.value < 0.05,
                      "Serial correlation present",
                      "No serial correlation")
)

kable(sc_tab, format = "latex", booktabs = TRUE, align = "lcccl",
      caption = "Serial correlation test for H2 panel",
      label   = "serial_correlation_h2") %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  footnote(general = "Test of $H_0$: no serial correlation in the idiosyncratic errors of the H2 fixed-effects model. The test is applied to a two-way within transformation of the main H2 specification. Rejection of $H_0$ justifies the use of cluster-robust standard errors clustered by country.",
           general_title = "Note:", footnote_as_chunk = TRUE,
           threeparttable = TRUE, escape = FALSE) %>%
  save_kable("/Users/almaowing/Documents/MAthesis/tables/table_serial_correlation_h2.tex")

#-------------------------------------------------------------------------------

#VIF test for H2:

# correlation matrix:
h2_cor_data <- cy_omg %>%
  mutate(student_dummy = as.numeric(campaign_type == "student"),
         other_dummy   = as.numeric(campaign_type == "other_campaign")) %>%
  select(student_dummy, other_dummy, student_index_cy,
         v2x_polyarchy, log_gdppc, gdp_growth, log_pop, e_miurbani) %>%
  mutate(across(everything(), ~ifelse(is.finite(.), ., NA))) %>%
  na.omit()

cat("H2 correlation matrix (main-model regressors):\n")
print(round(cor(h2_cor_data), 2))



# VIF for M4 (binary student-led indicator) 
h2_vif_protest <- lm(
  electoral_democracy_t1 ~ student_protest + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani,
  data = cy_omg %>%
    filter(!is.na(electoral_democracy_t1)) %>%
    mutate(across(c(log_gdppc, gdp_growth, log_pop, e_miurbani),
                  ~ifelse(is.finite(.), ., NA))) %>%
    tidyr::drop_na(electoral_democracy_t1, student_protest, v2x_polyarchy,
                   log_gdppc, gdp_growth, log_pop, e_miurbani)
)


# VIF for M8 (student involvement index):
h2_vif_index <- lm(
  electoral_democracy_t1 ~ student_index_cy + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani,
  data = cy_omg %>%
    filter(!is.na(electoral_democracy_t1)) %>%
    mutate(across(c(log_gdppc, gdp_growth, log_pop, e_miurbani),
                  ~ifelse(is.finite(.), ., NA))) %>%
    tidyr::drop_na(electoral_democracy_t1, student_index_cy, v2x_polyarchy,
                   log_gdppc, gdp_growth, log_pop, e_miurbani)
)


#console check:
cat("\nVIF — H2 M4 (binary student-led indicator):\n")
print(vif(h2_vif_protest))
cat("\nVIF — H2 M8 (student involvement index):\n")
print(vif(h2_vif_index))



#export VIF table to LaTeX:
v_h2_protest <- vif(h2_vif_protest)
v_h2_index   <- vif(h2_vif_index)

vif_h2_labels <- c(
  "student_protest"  = "Student-led protest",
  "student_index_cy" = "Student involvement index",
  "v2x_polyarchy"    = "Electoral democracy (lagged)",
  "log_gdppc"        = "GDP per capita (log)",
  "gdp_growth"       = "GDP growth",
  "log_pop"          = "Population (log)",
  "e_miurbani"       = "Urbanization (V-Dem)"
)

all_vars <- names(vif_h2_labels)
vif_h2_tab <- tibble(
  Variable       = vif_h2_labels[all_vars],
  `M4 (protest)` = sapply(all_vars, function(v) {
    if (v %in% names(v_h2_protest)) sprintf("%.2f", v_h2_protest[[v]]) else "--"
  }),
  `M8 (index)`   = sapply(all_vars, function(v) {
    if (v %in% names(v_h2_index)) sprintf("%.2f", v_h2_index[[v]]) else "--"
  })
)

kable(vif_h2_tab, format = "latex", booktabs = TRUE, align = "lcc",
      caption = "Variance inflation factors for H2 covariates",
      label   = "vif_h2") %>%
  kable_styling(latex_options = "hold_position") %>%
  footnote(general = "Variance Inflation Factors (VIFs) for the covariates in Models 4 and 8 of Table 6.2, computed from OLS specifications without country and year fixed effects. Sample restricted to country-years with non-missing electoral democracy at t+1. All VIFs fall well below the conventional threshold of 5, indicating no problematic multicollinearity.",
           general_title = "Note:", footnote_as_chunk = TRUE, threeparttable = TRUE) %>%
  save_kable("/Users/almaowing/Documents/MAthesis/tables/table_vif_h2.tex")

#-------------------------------------------------------------------------------

#OLS MAIN MODELS (H2):

# M1-M4: campaign_type:

# M1: baseline - campaign type only:
m1 <- feols(
  electoral_democracy_t1 ~ campaign_type
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1))
)

# M2: + lagged democracy:
m2 <- feols(
  electoral_democracy_t1 ~ campaign_type + v2x_polyarchy
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1))
)

# M3: + full controls (full period):
m3 <- feols(
  electoral_democracy_t1 ~ campaign_type + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1))
)

# M4: + full controls (post-1960):
m4 <- feols(
  electoral_democracy_t1 ~ campaign_type + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1960)
)


#M5-M8: student_index_cy:

# M5: baseline, student index only:
m5 <- feols(
  electoral_democracy_t1 ~ student_index_cy
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1))
)

# M6: + lagged democracy:
m6 <- feols(
  electoral_democracy_t1 ~ student_index_cy + v2x_polyarchy
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1))
)

# M7: + full controls (full period):
m7 <- feols(
  electoral_democracy_t1 ~ student_index_cy + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1))
)

# M8: + full controls (post-1960):
m8 <- feols(
  electoral_democracy_t1 ~ student_index_cy + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1960)
)


# lets see:
etable(m1, m2, m3, m4, m5, m6, m7, m8)

# export table:
coef_labels_h2 <- c(
  "campaign_typestudent"        = "Student-led protest",
  "campaign_typeother_campaign" = "Other campaign",
  "student_index_cy"            = "Student involvement index",
  "v2x_polyarchy"               = "Electoral democracy (lagged)",
  "log_gdppc"                   = "GDP per capita (log)",
  "gdp_growth"                  = "GDP growth",
  "log_pop"                     = "Population (log)",
  "e_miurbani"                  = "Urbanization (V-Dem)"
)

fe_rows_h2 <- data.frame(
  term   = c("Period", "Lagged democracy", "Country FE", "Year FE"),
  "M1"   = c("Full",      "No",  "Yes", "Yes"),
  "M2"   = c("Full",      "Yes", "Yes", "Yes"),
  "M3"   = c("Full",      "Yes", "Yes", "Yes"),
  "M4"   = c("Post-1960", "Yes", "Yes", "Yes"),
  "M5"   = c("Full",      "No",  "Yes", "Yes"),
  "M6"   = c("Full",      "Yes", "Yes", "Yes"),
  "M7"   = c("Full",      "Yes", "Yes", "Yes"),
  "M8"   = c("Post-1960", "Yes", "Yes", "Yes")
)

modelsummary(
  list("M1" = m1, "M2" = m2, "M3" = m3, "M4" = m4,
       "M5" = m5, "M6" = m6, "M7" = m7, "M8" = m8),
  stars     = TRUE,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map  = coef_labels_h2,
  gof_map   = c("nobs", "r.squared"),
  add_rows  = fe_rows_h2,
  output    = "/Users/almaowing/Documents/MAthesis/tables/table_h2_main.tex",
  title     = "Student-led protests and democratization (H2)",
  notes     = "Cluster-robust standard errors by country in parentheses. DV = V-Dem electoral democracy index at t+1. Reference category: no ongoing campaign. M1--M4 use campaign type; M5--M8 use the student involvement index (0--3)."
)

h2_tex <- readLines("/Users/almaowing/Documents/MAthesis/tables/table_h2_main.tex")
h2_tex <- gsub("\\\\centering", "\\\\centering\n\\\\scalebox{0.82}{", h2_tex)
h2_tex <- gsub("\\\\end\\{talltblr\\}", "\\\\end{talltblr}\n}", h2_tex)
writeLines(h2_tex, "/Users/almaowing/Documents/MAthesis/tables/table_h2_main.tex")


#---------------------------------
# Wald test (linear hypothesis test) (H2):

# Tests H_0: β_student = β_other_campaign across H2 specifications M1-M4.
#console check:
linearHypothesis(m1, "campaign_typestudent = campaign_typeother_campaign")
linearHypothesis(m2, "campaign_typestudent = campaign_typeother_campaign")
linearHypothesis(m3, "campaign_typestudent = campaign_typeother_campaign")
linearHypothesis(m4, "campaign_typestudent = campaign_typeother_campaign")


# helper function: run test and extract chi-square + p-value:
lh_extract <- function(mod, label, spec) {
  lh <- linearHypothesis(mod, "campaign_typestudent = campaign_typeother_campaign")
  data.frame(
    Model         = label,
    Specification = spec,
    Chisq         = round(lh$Chisq[2], 2),
    df            = lh$Df[2],
    p_value       = round(lh$`Pr(>Chisq)`[2], 3),
    stringsAsFactors = FALSE
  )
}


#build table:
lh_table <- rbind(
  lh_extract(m1, "M1", "Bivariate baseline"),
  lh_extract(m2, "M2", "+ lagged democracy"),
  lh_extract(m3, "M3", "+ full controls (full period)"),
  lh_extract(m4, "M4", "M3, post-1960 (main)")
)


# sdd significance stars:
lh_table$stars <- ifelse(lh_table$p_value < 0.001, "***",
                         ifelse(lh_table$p_value < 0.01,  "**",
                                ifelse(lh_table$p_value < 0.05,  "*",
                                       ifelse(lh_table$p_value < 0.10,  "+", ""))))

lh_table$p_display <- paste0(sprintf("%.3f", lh_table$p_value), lh_table$stars)

print(lh_table)


# export to LaTeX:
lh_display <- lh_table[, c("Model", "Specification", "Chisq", "df", "p_display")]
colnames(lh_display) <- c("Model", "Specification", "$\\chi^2$", "df", "$p$-value")

lh_xtab <- xtable(
  lh_display,
  caption = "Linear hypothesis tests: student-led protest vs. other campaign (H2). Each row tests the null hypothesis that the coefficient on student-led protest equals the coefficient on other campaigns. DV = V-Dem Electoral Democracy Index at $t+1$. Significance: $+\\,p<0.10$, $*\\,p<0.05$, $**\\,p<0.01$, $***\\,p<0.001$.",
  label   = "tab:h2_linhyp_appendix",
  align   = c("l", "l", "l", "r", "r", "r")
)

print(
  lh_xtab,
  file                  = "/Users/almaowing/Documents/MAthesis/tables/LINHYP_H2_appendix.tex",
  include.rownames      = FALSE,
  sanitize.text.function = identity,
  booktabs              = TRUE,
  caption.placement     = "top"
)

cat("\nLinear hypothesis appendix table written to:\n",
    "/Users/almaowing/Documents/MAthesis/tables/LINHYP_H2_appendix.tex\n")


#-------------------------------------------------------------------------------

#Figures:

#Figure on coefficient evolution M1 => M4

#helper function: extract coef + CI from feols object:
extract_fixest_coef <- function(model, model_name, model_label) {
  ct  <- coeftable(model)
  ci  <- confint(model)
  terms_keep <- c("campaign_typestudent", "campaign_typeother_campaign")
  idx <- rownames(ct) %in% terms_keep
  data.frame(
    term      = rownames(ct)[idx],
    estimate  = ct[idx, "Estimate"],
    conf.low  = ci[idx, 1],
    conf.high = ci[idx, 2],
    model     = model_name,
    mlabel    = model_label,
    stringsAsFactors = FALSE
  )
}


#build long-format data frame for plot:
coef_evo <- bind_rows(
  extract_fixest_coef(m1, "M1", "M1"),
  extract_fixest_coef(m2, "M2", "M2"),
  extract_fixest_coef(m3, "M3", "M3"),
  extract_fixest_coef(m4, "M4", "M4")
) %>%
  mutate(
    campaign = factor(
      ifelse(term == "campaign_typestudent", "Student-led protest", "Other campaign"),
      levels = c("Student-led protest", "Other campaign")
    ),
    model = factor(model, levels = c("M1", "M2", "M3", "M4"))
  )


#plot:
fig_coef_evo <- ggplot(
  coef_evo,
  aes(x = model, y = estimate, ymin = conf.low, ymax = conf.high,
      shape = campaign, group = campaign)
) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60", linewidth = 0.4) +
  geom_errorbar(width = 0.12, linewidth = 0.5, position = position_dodge(0.35)) +
  geom_point(size = 3.5, position = position_dodge(0.35)) +
  scale_shape_manual(values = c("Student-led protest" = 19, "Other campaign" = 21)) +
  scale_y_continuous(limits = c(NA, 0.05)) +
  labs(
    x = NULL,
    y = "Coefficient (Electoral Democracy Index, t+1)",
    shape = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.border       = element_blank(),
    axis.line.x        = element_line(colour = "black", linewidth = 0.4),
    axis.line.y        = element_line(colour = "black", linewidth = 0.4),
    axis.title.y       = element_text(size = 10, colour = "black", margin = margin(r = 8)),
    axis.text          = element_text(size = 10, colour = "grey50"),
    legend.position    = "bottom",
    legend.text        = element_text(size = 10, colour = "black"),
    plot.margin        = margin(10, 15, 10, 10)
  )

ggsave("/Users/almaowing/Documents/MAthesis/figures/fig_coef_evo_h3.pdf",
       fig_coef_evo, width = 4, height = 3.8)



#Figure on marginal effects by campaign type (M4):

ct_m4 <- coeftable(m4)
ci_m4 <- confint(m4)

camp_coefs <- data.frame(
  category  = factor(
    c("No campaign\n(reference)", "Other\ncampaign", "Student-led\nprotest"),
    levels = c("No campaign\n(reference)", "Other\ncampaign", "Student-led\nprotest")
  ),
  estimate  = c(0,
                ct_m4["campaign_typeother_campaign", "Estimate"],
                ct_m4["campaign_typestudent",        "Estimate"]),
  conf.low  = c(NA,
                ci_m4["campaign_typeother_campaign", 1],
                ci_m4["campaign_typestudent",        1]),
  conf.high = c(NA,
                ci_m4["campaign_typeother_campaign", 2],
                ci_m4["campaign_typestudent",        2])
)


#plot:
fig_pred_camptype <- ggplot(
  camp_coefs,
  aes(x = category, y = estimate, ymin = conf.low, ymax = conf.high)
) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60", linewidth = 0.4) +
  geom_errorbar(width = 0.12, linewidth = 0.5, na.rm = TRUE) +
  geom_point(size = 3.5, shape = 19) +
  labs(
    x = "Campaign type",
    y = "Change in Electoral Democracy Index (t+1)\nrelative to no campaign"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.border       = element_blank(),
    axis.line.x        = element_line(colour = "black", linewidth = 0.4),
    axis.line.y        = element_line(colour = "black", linewidth = 0.4),
    axis.title.x       = element_text(size = 10, colour = "black", margin = margin(t = 8)),
    axis.title.y       = element_text(size = 10, colour = "black", margin = margin(r = 8)),
    axis.text          = element_text(size = 10, colour = "grey50"),
    plot.margin        = margin(10, 15, 10, 10)
  )

ggsave("/Users/almaowing/Documents/MAthesis/figures/fig_pred_camptype_h2.pdf",
       fig_pred_camptype, width = 4, height = 3.8)


#-------------------------------------------------------------------------------

#ROBUSTNESS CHECKS:

# ALTERNATIVE VARIABLES
# Alternative dependent variables:

# H1 alternative dependent variable (election):


#construct DV on omg, then aggregate to campaign level:
omg$dem_demand_election <- as.numeric(omg$demand_election == 1)
omg$dem_demand_election[is.na(omg$dem_demand_election)] <- 0

omg_campaign <- omg_campaign %>%
  left_join(
    omg %>%
      group_by(id) %>%
      summarise(
        dem_demand_election = max(dem_demand_election, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "id"
  )


# M1-M4: student_protest:
model1_election <- lm(dem_demand_election ~ student_protest,
                      data = omg_campaign)

model2_election <- lm(dem_demand_election ~ student_protest +
                        working_group + educated_group +
                        elite_group + identity_group +
                        factor(country) + factor(decade),
                      data = omg_campaign)

model3_election <- lm(dem_demand_election ~ student_protest +
                        working_group + educated_group +
                        elite_group + identity_group +
                        regime_context + log_gdppc +
                        factor(country) + factor(decade),
                      data = omg_campaign)

model4_election <- lm(dem_demand_election ~ student_protest +
                        working_group + educated_group +
                        elite_group + identity_group +
                        regime_context + log_gdppc + peak_size + nonviolent +
                        factor(country) + factor(decade),
                      data = omg_campaign)


#M5-M8: student_index:
model5_election <- lm(dem_demand_election ~ student_index,
                      data = omg_campaign)

model6_election <- lm(dem_demand_election ~ student_index +
                        working_group + educated_group +
                        elite_group + identity_group +
                        factor(country) + factor(decade),
                      data = omg_campaign)

model7_election <- lm(dem_demand_election ~ student_index +
                        working_group + educated_group +
                        elite_group + identity_group +
                        regime_context + log_gdppc +
                        factor(country) + factor(decade),
                      data = omg_campaign)

model8_election <- lm(dem_demand_election ~ student_index +
                        working_group + educated_group +
                        elite_group + identity_group +
                        regime_context + log_gdppc + peak_size + nonviolent +
                        factor(country) + factor(decade),
                      data = omg_campaign)


#clustered SEs:
vcov1_election <- vcovCL(model1_election, cluster = ~ country)
vcov2_election <- vcovCL(model2_election, cluster = ~ country)
vcov3_election <- vcovCL(model3_election, cluster = ~ country)
vcov4_election <- vcovCL(model4_election, cluster = ~ country)
vcov5_election <- vcovCL(model5_election, cluster = ~ country)
vcov6_election <- vcovCL(model6_election, cluster = ~ country)
vcov7_election <- vcovCL(model7_election, cluster = ~ country)
vcov8_election <- vcovCL(model8_election, cluster = ~ country)


#lets see:
coeftest(model4_election, vcov = vcov4_election)
coeftest(model8_election, vcov = vcov8_election)


#export table:
modelsummary(
  list("M1" = model1_election, "M2" = model2_election,
       "M3" = model3_election, "M4" = model4_election,
       "M5" = model5_election, "M6" = model6_election,
       "M7" = model7_election, "M8" = model8_election),
  vcov      = list(vcov1_election, vcov2_election, vcov3_election, vcov4_election,
                   vcov5_election, vcov6_election, vcov7_election, vcov8_election),
  stars     = TRUE,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_omit = "factor\\(country\\)|factor\\(decade\\)",
  coef_map  = coef_labels_h1,
  gof_map   = c("nobs", "r.squared"),
  add_rows  = fe_rows_h1,
  output    = "/Users/almaowing/Documents/MAthesis/tables/table_h1_election_robustness.tex",
  title     = "Student-led protests and electoral demands, robustness (H1)",
  notes     = "Cluster-robust standard errors by country in parentheses. DV = binary indicator for campaigns raising electoral demands. M1--M4 use student domination; M5--M8 use the student involvement index (0--3). Country and decade fixed effects in M2--M4 and M6--M8."
)


#scalebox wrap:
h1_elec_tex <- readLines("/Users/almaowing/Documents/MAthesis/tables/table_h1_election_robustness.tex")
h1_elec_tex <- gsub("\\\\centering", "\\\\centering\n\\\\scalebox{0.82}{", h1_elec_tex)
h1_elec_tex <- gsub("\\\\end\\{talltblr\\}", "\\\\end{talltblr}\n}", h1_elec_tex)
writeLines(h1_elec_tex, "/Users/almaowing/Documents/MAthesis/tables/table_h1_election_robustness.tex")


# H1 alternative dependent variable (explicit democracy):

#construct DV on omg, then aggregate to campaign level:
omg$dem_demand_demo <- as.numeric(omg$demand_demo == 1)
omg$dem_demand_demo[is.na(omg$dem_demand_demo)] <- 0

omg_campaign <- omg_campaign %>%
  left_join(
    omg %>%
      group_by(id) %>%
      summarise(
        dem_demand_demo = max(dem_demand_demo, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "id"
  )


#M1-M4: student_protest:
model1_demo <- lm(dem_demand_demo ~ student_protest,
                  data = omg_campaign)

model2_demo <- lm(dem_demand_demo ~ student_protest +
                    working_group + educated_group +
                    elite_group + identity_group +
                    factor(country) + factor(decade),
                  data = omg_campaign)

model3_demo <- lm(dem_demand_demo ~ student_protest +
                    working_group + educated_group +
                    elite_group + identity_group +
                    regime_context + log_gdppc +
                    factor(country) + factor(decade),
                  data = omg_campaign)

model4_demo <- lm(dem_demand_demo ~ student_protest +
                    working_group + educated_group +
                    elite_group + identity_group +
                    regime_context + log_gdppc + peak_size + nonviolent +
                    factor(country) + factor(decade),
                  data = omg_campaign)


#M5-M8: student_index:
model5_demo <- lm(dem_demand_demo ~ student_index,
                  data = omg_campaign)

model6_demo <- lm(dem_demand_demo ~ student_index +
                    working_group + educated_group +
                    elite_group + identity_group +
                    factor(country) + factor(decade),
                  data = omg_campaign)

model7_demo <- lm(dem_demand_demo ~ student_index +
                    working_group + educated_group +
                    elite_group + identity_group +
                    regime_context + log_gdppc +
                    factor(country) + factor(decade),
                  data = omg_campaign)

model8_demo <- lm(dem_demand_demo ~ student_index +
                    working_group + educated_group +
                    elite_group + identity_group +
                    regime_context + log_gdppc + peak_size + nonviolent +
                    factor(country) + factor(decade),
                  data = omg_campaign)


#clustered SEs:
vcov1_demo <- vcovCL(model1_demo, cluster = ~ country)
vcov2_demo <- vcovCL(model2_demo, cluster = ~ country)
vcov3_demo <- vcovCL(model3_demo, cluster = ~ country)
vcov4_demo <- vcovCL(model4_demo, cluster = ~ country)
vcov5_demo <- vcovCL(model5_demo, cluster = ~ country)
vcov6_demo <- vcovCL(model6_demo, cluster = ~ country)
vcov7_demo <- vcovCL(model7_demo, cluster = ~ country)
vcov8_demo <- vcovCL(model8_demo, cluster = ~ country)


#quick check:
coeftest(model4_demo, vcov = vcov4_demo)
coeftest(model8_demo, vcov = vcov8_demo)


#export table:
modelsummary(
  list("M1" = model1_demo, "M2" = model2_demo,
       "M3" = model3_demo, "M4" = model4_demo,
       "M5" = model5_demo, "M6" = model6_demo,
       "M7" = model7_demo, "M8" = model8_demo),
  vcov      = list(vcov1_demo, vcov2_demo, vcov3_demo, vcov4_demo,
                   vcov5_demo, vcov6_demo, vcov7_demo, vcov8_demo),
  stars     = TRUE,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_omit = "factor\\(country\\)|factor\\(decade\\)",
  coef_map  = coef_labels_h1,
  gof_map   = c("nobs", "r.squared"),
  add_rows  = fe_rows_h1,
  output    = "/Users/almaowing/Documents/MAthesis/tables/table_h1_demo_robustness.tex",
  title     = "Student-led protests and explicit democracy demands, robustness (H1)",
  notes     = "Cluster-robust standard errors by country in parentheses. DV = binary indicator for campaigns explicitly articulating democracy as a demand. M1--M4 use student domination; M5--M8 use the student involvement index (0--3). Country and decade fixed effects in M2--M4 and M6--M8."
)


#scalebox wrap:
h1_demo_tex <- readLines("/Users/almaowing/Documents/MAthesis/tables/table_h1_demo_robustness.tex")
h1_demo_tex <- gsub("\\\\centering", "\\\\centering\n\\\\scalebox{0.82}{", h1_demo_tex)
h1_demo_tex <- gsub("\\\\end\\{talltblr\\}", "\\\\end{talltblr}\n}", h1_demo_tex)
writeLines(h1_demo_tex, "/Users/almaowing/Documents/MAthesis/tables/table_h1_demo_robustness.tex")



# H2 alternative dependent variable (liberal democracy):

#construct DV: liberal_democracy_t1:
cy_omg <- cy_omg %>%
  group_by(country_id) %>%
  arrange(year) %>%
  mutate(liberal_democracy_t1 = dplyr::lead(v2x_libdem, 1)) %>%
  ungroup()


#M1-M4: campaign_type:
m1_lib <- feols(
  liberal_democracy_t1 ~ campaign_type
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(liberal_democracy_t1))
)

m2_lib <- feols(
  liberal_democracy_t1 ~ campaign_type + v2x_polyarchy
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(liberal_democracy_t1))
)

m3_lib <- feols(
  liberal_democracy_t1 ~ campaign_type + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(liberal_democracy_t1))
)

m4_lib <- feols(
  liberal_democracy_t1 ~ campaign_type + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(liberal_democracy_t1) & year >= 1960)
)


#M5-M8: student_index_cy:
m5_lib <- feols(
  liberal_democracy_t1 ~ student_index_cy
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(liberal_democracy_t1))
)

m6_lib <- feols(
  liberal_democracy_t1 ~ student_index_cy + v2x_polyarchy
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(liberal_democracy_t1))
)

m7_lib <- feols(
  liberal_democracy_t1 ~ student_index_cy + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(liberal_democracy_t1))
)

m8_lib <- feols(
  liberal_democracy_t1 ~ student_index_cy + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(liberal_democracy_t1) & year >= 1960)
)


#lets see:
summary(m4_lib)
summary(m8_lib)


#coefficient labels:
coef_labels_h2_lib <- c(
  "campaign_typestudent"        = "Student-led campaign",
  "campaign_typeother_campaign" = "Other campaign",
  "student_index_cy"            = "Student involvement index",
  "v2x_polyarchy"               = "Electoral democracy (lagged)",
  "log_gdppc"                   = "GDP per capita (log)",
  "gdp_growth"                  = "GDP growth",
  "log_pop"                     = "Population (log)",
  "e_miurbani"                  = "Urbanization (V-Dem)"
)


#fixed effects rows:
fe_rows_h2_lib <- data.frame(
  term   = c("Period", "Lagged democracy", "Country FE", "Year FE"),
  "M1"   = c("Full",      "No",  "Yes", "Yes"),
  "M2"   = c("Full",      "Yes", "Yes", "Yes"),
  "M3"   = c("Full",      "Yes", "Yes", "Yes"),
  "M4"   = c("Post-1960", "Yes", "Yes", "Yes"),
  "M5"   = c("Full",      "No",  "Yes", "Yes"),
  "M6"   = c("Full",      "Yes", "Yes", "Yes"),
  "M7"   = c("Full",      "Yes", "Yes", "Yes"),
  "M8"   = c("Post-1960", "Yes", "Yes", "Yes")
)


#export table (Table A13):
modelsummary(
  list("M1" = m1_lib, "M2" = m2_lib, "M3" = m3_lib, "M4" = m4_lib,
       "M5" = m5_lib, "M6" = m6_lib, "M7" = m7_lib, "M8" = m8_lib),
  stars     = TRUE,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map  = coef_labels_h2_lib,
  gof_map   = c("nobs", "r.squared"),
  add_rows  = fe_rows_h2_lib,
  output    = "/Users/almaowing/Documents/MAthesis/tables/table_h2_libdem_robustness_1.tex",
  title     = "Student-led protests and democratization, liberal democracy robustness (H2)",
  notes     = "Cluster-robust standard errors by country in parentheses. DV = V-Dem liberal democracy index at t+1. Lagged electoral democracy (v2x\\_polyarchy) included as control. Reference category: no ongoing campaign. M1--M4 use campaign type; M5--M8 use the student involvement index (0--3)."
)


#scalebox wrap:
h2_lib_tex <- readLines("/Users/almaowing/Documents/MAthesis/tables/table_h2_libdem_robustness_1.tex")
h2_lib_tex <- gsub("\\\\centering", "\\\\centering\n\\\\scalebox{0.72}{", h2_lib_tex)
h2_lib_tex <- gsub("\\\\end\\{talltblr\\}", "\\\\end{talltblr}\n}", h2_lib_tex)
writeLines(h2_lib_tex, "/Users/almaowing/Documents/MAthesis/tables/table_h2_libdem_robustness_1.tex")

#-------------------------------------------------------------------------------
#Alternative independent variables

#Alternative independent variables (atleast + originate students for H1):

#DVs and agregate to campaign level:
omg$atleast_protest   <- as.numeric(omg$atleast_students   == 1)
omg$originate_protest <- as.numeric(omg$originate_students == 1)
omg$atleast_protest[is.na(omg$atleast_protest)]     <- 0
omg$originate_protest[is.na(omg$originate_protest)] <- 0

omg_campaign <- omg_campaign %>%
  left_join(
    omg %>%
      group_by(id) %>%
      summarise(
        atleast_protest   = max(atleast_protest,   na.rm = TRUE),
        originate_protest = max(originate_protest, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "id"
  )


#M1-M4: atleast_protest (permissive):
model1_atleast <- lm(dem_demand_any ~ atleast_protest,
                     data = omg_campaign)

model2_atleast <- lm(dem_demand_any ~ atleast_protest +
                       working_group + educated_group +
                       elite_group + identity_group +
                       factor(country) + factor(decade),
                     data = omg_campaign)

model3_atleast <- lm(dem_demand_any ~ atleast_protest +
                       working_group + educated_group +
                       elite_group + identity_group +
                       regime_context + log_gdppc +
                       factor(country) + factor(decade),
                     data = omg_campaign)

model4_atleast <- lm(dem_demand_any ~ atleast_protest +
                       working_group + educated_group +
                       elite_group + identity_group +
                       regime_context + log_gdppc + peak_size + nonviolent +
                       factor(country) + factor(decade),
                     data = omg_campaign)


#M5-M8: originatee_protest (medium):
model5_originate <- lm(dem_demand_any ~ originate_protest,
                       data = omg_campaign)

model6_originate <- lm(dem_demand_any ~ originate_protest +
                         working_group + educated_group +
                         elite_group + identity_group +
                         factor(country) + factor(decade),
                       data = omg_campaign)

model7_originate <- lm(dem_demand_any ~ originate_protest +
                         working_group + educated_group +
                         elite_group + identity_group +
                         regime_context + log_gdppc +
                         factor(country) + factor(decade),
                       data = omg_campaign)

model8_originate <- lm(dem_demand_any ~ originate_protest +
                         working_group + educated_group +
                         elite_group + identity_group +
                         regime_context + log_gdppc + peak_size + nonviolent +
                         factor(country) + factor(decade),
                       data = omg_campaign)


#clustered SEs:
vcov1_atleast    <- vcovCL(model1_atleast,    cluster = ~ country)
vcov2_atleast    <- vcovCL(model2_atleast,    cluster = ~ country)
vcov3_atleast    <- vcovCL(model3_atleast,    cluster = ~ country)
vcov4_atleast    <- vcovCL(model4_atleast,    cluster = ~ country)
vcov5_originate  <- vcovCL(model5_originate,  cluster = ~ country)
vcov6_originate  <- vcovCL(model6_originate,  cluster = ~ country)
vcov7_originate  <- vcovCL(model7_originate,  cluster = ~ country)
vcov8_originate  <- vcovCL(model8_originate,  cluster = ~ country)


#quick check:
coeftest(model4_atleast,   vcov = vcov4_atleast)
coeftest(model8_originate, vcov = vcov8_originate)


#coef labels (different from main):
coef_labels_iv <- c(
  "atleast_protest"   = "Student-led protest (atleast)",
  "originate_protest" = "Student-led protest (originate)",
  "working_group"     = "Working groups dominate",
  "educated_group"    = "Educated urban groups dominate",
  "elite_group"       = "Elite groups dominate",
  "identity_group"    = "Identity groups dominate",
  "regime_context"    = "Electoral democracy (V-Dem)",
  "log_gdppc"         = "GDP per capita (log)",
  "peak_size"         = "Campaign size",
  "nonviolent"        = "Nonviolent campaign"
)


#fixed effects rows:
fe_rows_iv <- data.frame(
  term  = c("Country FE", "Decade FE"),
  "M1"  = c("No",  "No"),
  "M2"  = c("Yes", "Yes"),
  "M3"  = c("Yes", "Yes"),
  "M4"  = c("Yes", "Yes"),
  "M5"  = c("No",  "No"),
  "M6"  = c("Yes", "Yes"),
  "M7"  = c("Yes", "Yes"),
  "M8"  = c("Yes", "Yes")
)


#export table:
modelsummary(
  list("M1" = model1_atleast,   "M2" = model2_atleast,
       "M3" = model3_atleast,   "M4" = model4_atleast,
       "M5" = model5_originate, "M6" = model6_originate,
       "M7" = model7_originate, "M8" = model8_originate),
  vcov      = list(vcov1_atleast, vcov2_atleast, vcov3_atleast, vcov4_atleast,
                   vcov5_originate, vcov6_originate, vcov7_originate, vcov8_originate),
  stars     = TRUE,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_omit = "factor\\(country\\)|factor\\(decade\\)",
  coef_map  = coef_labels_iv,
  gof_map   = c("nobs", "r.squared"),
  add_rows  = fe_rows_iv,
  output    = "/Users/almaowing/Documents/MAthesis/tables/table_h1_iv_robustness.tex",
  title     = "Student-led protests and democratic demands, alternative IV robustness (H1)",
  notes     = "Cluster-robust standard errors by country in parentheses. DV = dem\\_demand\\_any. M1--M4 use atleast\\_students (permissive: students participate); M5--M8 use originate\\_students (medium: students initiate). Country and decade fixed effects in M2--M4 and M6--M8."
)


#scalebox wrap:
h1_iv_tex <- readLines("/Users/almaowing/Documents/MAthesis/tables/table_h1_iv_robustness.tex")
h1_iv_tex <- gsub("\\\\centering", "\\\\centering\n\\\\scalebox{0.82}{", h1_iv_tex)
h1_iv_tex <- gsub("\\\\end\\{talltblr\\}", "\\\\end{talltblr}\n}", h1_iv_tex)
writeLines(h1_iv_tex, "/Users/almaowing/Documents/MAthesis/tables/table_h1_iv_robustness.tex")

#-------------------------------------------------------------------------------

#Alternative independent variables (atleast + originate students for H2):

#construct alternative campaign_type variables:
cy_omg <- cy_omg %>%
  mutate(
    # atleast: student defined as any campaign with atleast_students_count > 0
    campaign_type_atleast = case_when(
      atleast_students_count > 0                              ~ "student",
      count_movements > 0 & atleast_students_count == 0       ~ "other_campaign",
      TRUE                                                    ~ "no_campaign"
    ),
    campaign_type_atleast = relevel(factor(campaign_type_atleast), ref = "no_campaign"),
    
    # originate: student defined as any campaign with originate_students_count > 0
    campaign_type_originate = case_when(
      originate_students_count > 0                            ~ "student",
      count_movements > 0 & originate_students_count == 0     ~ "other_campaign",
      TRUE                                                    ~ "no_campaign"
    ),
    campaign_type_originate = relevel(factor(campaign_type_originate), ref = "no_campaign")
  )


#M1-M4: campaign_type_atleast (permissive):
m1_atleast <- feols(
  electoral_democracy_t1 ~ campaign_type_atleast
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1))
)

m2_atleast <- feols(
  electoral_democracy_t1 ~ campaign_type_atleast + v2x_polyarchy
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1))
)

m3_atleast <- feols(
  electoral_democracy_t1 ~ campaign_type_atleast + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1))
)

m4_atleast <- feols(
  electoral_democracy_t1 ~ campaign_type_atleast + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1960)
)


#M5-M8: campaign_type_originate (medium):
m5_originate <- feols(
  electoral_democracy_t1 ~ campaign_type_originate
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1))
)

m6_originate <- feols(
  electoral_democracy_t1 ~ campaign_type_originate + v2x_polyarchy
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1))
)

m7_originate <- feols(
  electoral_democracy_t1 ~ campaign_type_originate + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1))
)

m8_originate <- feols(
  electoral_democracy_t1 ~ campaign_type_originate + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1960)
)


#lets see:
summary(m4_atleast)
summary(m8_originate)


#coefficient labels:
coef_labels_h2_iv <- c(
  "campaign_type_atleaststudent"          = "Student-led campaign (atleast)",
  "campaign_type_atleastother_campaign"   = "Other campaign",
  "campaign_type_originatestudent"        = "Student-led campaign (originate)",
  "campaign_type_originateother_campaign" = "Other campaign",
  "v2x_polyarchy"                         = "Electoral democracy (lagged)",
  "log_gdppc"                             = "GDP per capita (log)",
  "gdp_growth"                            = "GDP growth",
  "log_pop"                               = "Population (log)",
  "e_miurbani"                            = "Urbanization (V-Dem)"
)


#fixed effects rows:
fe_rows_h2_iv <- data.frame(
  term   = c("IV definition", "Period", "Lagged democracy", "Country FE", "Year FE"),
  "M1"   = c("Atleast",   "Full",      "No",  "Yes", "Yes"),
  "M2"   = c("Atleast",   "Full",      "Yes", "Yes", "Yes"),
  "M3"   = c("Atleast",   "Full",      "Yes", "Yes", "Yes"),
  "M4"   = c("Atleast",   "Post-1960", "Yes", "Yes", "Yes"),
  "M5"   = c("Originate", "Full",      "No",  "Yes", "Yes"),
  "M6"   = c("Originate", "Full",      "Yes", "Yes", "Yes"),
  "M7"   = c("Originate", "Full",      "Yes", "Yes", "Yes"),
  "M8"   = c("Originate", "Post-1960", "Yes", "Yes", "Yes")
)


#export table:
modelsummary(
  list("M1" = m1_atleast,   "M2" = m2_atleast,
       "M3" = m3_atleast,   "M4" = m4_atleast,
       "M5" = m5_originate, "M6" = m6_originate,
       "M7" = m7_originate, "M8" = m8_originate),
  stars     = TRUE,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map  = coef_labels_h2_iv,
  gof_map   = c("nobs", "r.squared"),
  add_rows  = fe_rows_h2_iv,
  output    = "/Users/almaowing/Documents/MAthesis/tables/table_h2_iv_robustness_1.tex",
  title     = "Student-led protests and democratization, alternative IV robustness (H2)",
  notes     = "Cluster-robust standard errors by country in parentheses. DV = V-Dem electoral democracy index at t+1. M1--M4 define student campaign as atleast\\_students (permissive); M5--M8 define as originate\\_students (medium). Reference category: no ongoing campaign."
)


# ── scalebox wrap ───────────────────────────────────────────────────────────
h2_iv_tex <- readLines("/Users/almaowing/Documents/MAthesis/tables/table_h2_iv_robustness_1.tex")
h2_iv_tex <- gsub("\\\\centering", "\\\\centering\n\\\\scalebox{0.72}{", h2_iv_tex)
h2_iv_tex <- gsub("\\\\end\\{talltblr\\}", "\\\\end{talltblr}\n}", h2_iv_tex)
writeLines(h2_iv_tex, "/Users/almaowing/Documents/MAthesis/tables/table_h2_iv_robustness_1.tex")


#-------------------------------------------------------------------------------

#Alternative control variables

#Alternative control variables for H1

#data prep:

#drop any pre-existing versions to avoid .x/.y conflicts:
omg_campaign <- omg_campaign %>%
  select(-any_of(c(
    "v2csreprss", "v2x_corr",
    "v2csreprss.x", "v2csreprss.y",
    "v2x_corr.x", "v2x_corr.y",
    "country_text_id", "country_text_id.x", "country_text_id.y",
    "start_year_5", "start_year_5.x", "start_year_5.y",
    "mean_schooling", "mean_schooling.x", "mean_schooling.y"
  )))


#join V-Dem variables (v2csreprss + v2x_corr):
vdem_extra_h1 <- vdem %>%
  select(country_id, year, v2csreprss, v2x_corr)

omg_campaign <- omg_campaign %>%
  left_join(
    vdem_extra_h1,
    by = c("country_id", "start_year" = "year")
  )


#wrangle OWID schooling:
owid_educ <- owid_educ_raw %>%
  rename(country_text_id = Code, mean_schooling = Average.years.of.schooling) %>%
  filter(!is.na(mean_schooling)) %>%
  select(country_text_id, Year, mean_schooling)


#add country_text_id to omg_campaign via vdem lookup:
id_lookup <- vdem %>%
  select(country_id, country_text_id) %>%
  distinct()

omg_campaign <- omg_campaign %>%
  left_join(id_lookup, by = "country_id")


#round campaign start_year to nearest 5-year interval and join
omg_campaign <- omg_campaign %>%
  mutate(start_year_5 = round(start_year / 5) * 5) %>%
  left_join(
    owid_educ,
    by = c("country_text_id", "start_year_5" = "Year")
  )


#clean up any residual .x/.y suffixes:
omg_campaign <- omg_campaign %>%
  rename_with(~ sub("\\.x$", "", .), ends_with(".x")) %>%
  select(-ends_with(".y"))


#check coverage
cat("Campaigns with v2csreprss data:",    sum(!is.na(omg_campaign$v2csreprss)),    "\n")
cat("Campaigns with v2x_corr data:",      sum(!is.na(omg_campaign$v2x_corr)),      "\n")
cat("Campaigns with mean_schooling data:", sum(!is.na(omg_campaign$mean_schooling)), "\n")

#Wald test:

#M4b alt controls (student_protest + extras + schooling):
h1_vif_ctrl_protest <- lm(
  dem_demand_any ~ student_protest + working_group + educated_group +
    elite_group + identity_group + regime_context + log_gdppc +
    peak_size + nonviolent +
    v2csreprss + v2x_corr + mean_schooling,
  data = omg_campaign %>%
    tidyr::drop_na(dem_demand_any, student_protest, working_group, educated_group,
                   elite_group, identity_group, regime_context, log_gdppc,
                   peak_size, nonviolent, v2csreprss, v2x_corr, mean_schooling)
)


#M8b alt controls (student_index + extras + schooling):
h1_vif_ctrl_index <- lm(
  dem_demand_any ~ student_index + working_group + educated_group +
    elite_group + identity_group + regime_context + log_gdppc +
    peak_size + nonviolent +
    v2csreprss + v2x_corr + mean_schooling,
  data = omg_campaign %>%
    tidyr::drop_na(dem_demand_any, student_index, working_group, educated_group,
                   elite_group, identity_group, regime_context, log_gdppc,
                   peak_size, nonviolent, v2csreprss, v2x_corr, mean_schooling)
)


#lets see:
cat("\nVIF — H1 M4b with alt controls + schooling:\n")
print(vif(h1_vif_ctrl_protest))

cat("\nVIF — H1 M8b with alt controls + schooling:\n")
print(vif(h1_vif_ctrl_index))


#export VIF table
v_h1_ctrl_p <- vif(h1_vif_ctrl_protest)
v_h1_ctrl_i <- vif(h1_vif_ctrl_index)

vif_h1_ctrl_labels <- c(
  "student_protest" = "Student-led protest",
  "student_index"   = "Student involvement index",
  "working_group"   = "Working groups dominate",
  "educated_group"  = "Educated urban groups dominate",
  "elite_group"     = "Elite groups dominate",
  "identity_group"  = "Identity groups dominate",
  "regime_context"  = "Electoral democracy (V-Dem)",
  "log_gdppc"       = "GDP per capita (log)",
  "peak_size"       = "Campaign size",
  "nonviolent"      = "Nonviolent campaign",
  "v2csreprss"      = "CSO repression",
  "v2x_corr"        = "Political corruption",
  "mean_schooling"  = "Mean years of schooling"
)

all_vars_h1_ctrl <- names(vif_h1_ctrl_labels)
vif_h1_ctrl_tab <- tibble(
  Variable              = vif_h1_ctrl_labels[all_vars_h1_ctrl],
  `M4b (alt controls)`  = sapply(all_vars_h1_ctrl, function(v) {
    if (v %in% names(v_h1_ctrl_p)) sprintf("%.2f", v_h1_ctrl_p[[v]]) else "--"
  }),
  `M8b (alt controls)`  = sapply(all_vars_h1_ctrl, function(v) {
    if (v %in% names(v_h1_ctrl_i)) sprintf("%.2f", v_h1_ctrl_i[[v]]) else "--"
  })
)

kable(vif_h1_ctrl_tab, format = "latex", booktabs = TRUE, align = "lcc",
      caption = "Variance inflation factors for H1 alternative control specifications",
      label   = "vif_h1_ctrl") %>%
  kable_styling(latex_options = "hold_position") %>%
  footnote(general = "VIFs for the alternative control specifications of H1 (Table A18--A19), computed from OLS without country and decade fixed effects. M4b/M8b use the most extensive control set: main controls plus CSO repression, political corruption, and OWID mean years of schooling.",
           general_title = "Note:", footnote_as_chunk = TRUE, threeparttable = TRUE) %>%
  save_kable("/Users/almaowing/Documents/MAthesis/tables/table_vif_h1_ctrl.tex")



#descriptive statistics:
var_info_h1_robust <- data.frame(
  var_name = c("v2csreprss", "v2x_corr", "mean_schooling", "wb_gdp_growth"),
  label = c(
    "CSO repression (V-Dem)",
    "Political corruption (V-Dem)",
    "Mean years of schooling (OWID)",
    "GDP growth, post-1960 (World Bank)"
  ),
  type = c("Continuous", "Continuous", "Continuous", "Continuous"),
  stringsAsFactors = FALSE
)

desc_h1_robust <- do.call(rbind, lapply(seq_len(nrow(var_info_h1_robust)), function(i) {
  x <- omg_campaign[[ var_info_h1_robust$var_name[i] ]]
  x <- x[!is.na(x)]
  data.frame(
    Variable = var_info_h1_robust$label[i],
    Type     = var_info_h1_robust$type[i],
    Min      = round(min(x),  2),
    Mean     = round(mean(x), 2),
    Max      = round(max(x),  2),
    stringsAsFactors = FALSE
  )
}))

print(desc_h1_robust)


#export to LaTex:
kbl(
  desc_h1_robust,
  format   = "latex",
  booktabs = TRUE,
  caption  = "Descriptive Statistics -- Additional Controls (H1)",
  label    = "desc_h1_robust",
  linesep  = ""
) %>%
  kable_styling(latex_options = "hold_position") %>%
  footnote(general = "Descriptive statistics for the additional control variables introduced in the H1 robustness checks (Table A18--A19). Sample: protest campaigns from the OMG dataset, 1789--2019, across 150 countries. Variables drawn from V-Dem (CSO repression, political corruption), Our World in Data (mean years of schooling), and the World Bank (GDP growth, available 1960--2019).",
           general_title = "Note:", footnote_as_chunk = TRUE, threeparttable = TRUE) %>%
  save_kable("/Users/almaowing/Documents/MAthesis/tables/desc_h1_robust.tex")



# LPM MODELS:

# M1-M4: student_protest + extra controls:
model1_ctrl <- lm(dem_demand_any ~ student_protest,
                  data = omg_campaign)

model2_ctrl <- lm(dem_demand_any ~ student_protest +
                    working_group + educated_group +
                    elite_group + identity_group +
                    factor(country) + factor(decade),
                  data = omg_campaign)

model3_ctrl <- lm(dem_demand_any ~ student_protest +
                    working_group + educated_group +
                    elite_group + identity_group +
                    regime_context + log_gdppc +
                    v2csreprss + v2x_corr +
                    factor(country) + factor(decade),
                  data = omg_campaign)

model4_ctrl <- lm(dem_demand_any ~ student_protest +
                    working_group + educated_group +
                    elite_group + identity_group +
                    regime_context + log_gdppc + peak_size + nonviolent +
                    v2csreprss + v2x_corr +
                    factor(country) + factor(decade),
                  data = omg_campaign)


#M5-M8: student_index + extra controls:

model5_ctrl <- lm(dem_demand_any ~ student_index,
                  data = omg_campaign)

model6_ctrl <- lm(dem_demand_any ~ student_index +
                    working_group + educated_group +
                    elite_group + identity_group +
                    factor(country) + factor(decade),
                  data = omg_campaign)

model7_ctrl <- lm(dem_demand_any ~ student_index +
                    working_group + educated_group +
                    elite_group + identity_group +
                    regime_context + log_gdppc +
                    v2csreprss + v2x_corr +
                    factor(country) + factor(decade),
                  data = omg_campaign)

model8_ctrl <- lm(dem_demand_any ~ student_index +
                    working_group + educated_group +
                    elite_group + identity_group +
                    regime_context + log_gdppc + peak_size + nonviolent +
                    v2csreprss + v2x_corr +
                    factor(country) + factor(decade),
                  data = omg_campaign)


#M4b / M8b: + mean_schooling (post-1870 coverage):
model4b_ctrl <- lm(dem_demand_any ~ student_protest +
                     working_group + educated_group +
                     elite_group + identity_group +
                     regime_context + log_gdppc + peak_size + nonviolent +
                     v2csreprss + v2x_corr + mean_schooling +
                     factor(country) + factor(decade),
                   data = omg_campaign)

model8b_ctrl <- lm(dem_demand_any ~ student_index +
                     working_group + educated_group +
                     elite_group + identity_group +
                     regime_context + log_gdppc + peak_size + nonviolent +
                     v2csreprss + v2x_corr + mean_schooling +
                     factor(country) + factor(decade),
                   data = omg_campaign)


#clustered SEs:
vcov1_ctrl  <- vcovCL(model1_ctrl,  cluster = ~ country)
vcov2_ctrl  <- vcovCL(model2_ctrl,  cluster = ~ country)
vcov3_ctrl  <- vcovCL(model3_ctrl,  cluster = ~ country)
vcov4_ctrl  <- vcovCL(model4_ctrl,  cluster = ~ country)
vcov5_ctrl  <- vcovCL(model5_ctrl,  cluster = ~ country)
vcov6_ctrl  <- vcovCL(model6_ctrl,  cluster = ~ country)
vcov7_ctrl  <- vcovCL(model7_ctrl,  cluster = ~ country)
vcov8_ctrl  <- vcovCL(model8_ctrl,  cluster = ~ country)
vcov4b_ctrl <- vcovCL(model4b_ctrl, cluster = ~ country)
vcov8b_ctrl <- vcovCL(model8b_ctrl, cluster = ~ country)


#lets see:
coeftest(model4_ctrl,  vcov = vcov4_ctrl)
coeftest(model8b_ctrl, vcov = vcov8b_ctrl)


#coefficient labels:
coef_labels_ctrl <- c(
  "(Intercept)"     = "Intercept",
  "student_protest" = "Student-led protest",
  "student_index"   = "Student involvement index",
  "working_group"   = "Working groups dominate",
  "educated_group"  = "Educated urban groups dominate",
  "elite_group"     = "Elite groups dominate",
  "identity_group"  = "Identity groups dominate",
  "regime_context"  = "Electoral democracy (V-Dem)",
  "log_gdppc"       = "GDP per capita (log)",
  "peak_size"       = "Campaign size",
  "nonviolent"      = "Nonviolent campaign",
  "v2csreprss"      = "CSO repression",
  "v2x_corr"        = "Political corruption",
  "mean_schooling"  = "Mean years of schooling"
)


#fixed effects rows:
fe_rows_ctrl <- data.frame(
  term  = c("Country FE", "Decade FE"),
  "M1"  = c("No",  "No"),
  "M2"  = c("Yes", "Yes"),
  "M3"  = c("Yes", "Yes"),
  "M4"  = c("Yes", "Yes"),
  "M4b" = c("Yes", "Yes"),
  "M5"  = c("No",  "No"),
  "M6"  = c("Yes", "Yes"),
  "M7"  = c("Yes", "Yes"),
  "M8"  = c("Yes", "Yes"),
  "M8b" = c("Yes", "Yes")
)


#export table:
modelsummary(
  list("M1" = model1_ctrl,  "M2" = model2_ctrl,
       "M3" = model3_ctrl,  "M4" = model4_ctrl,  "M4b" = model4b_ctrl,
       "M5" = model5_ctrl,  "M6" = model6_ctrl,
       "M7" = model7_ctrl,  "M8" = model8_ctrl,  "M8b" = model8b_ctrl),
  vcov      = list(vcov1_ctrl, vcov2_ctrl, vcov3_ctrl, vcov4_ctrl, vcov4b_ctrl,
                   vcov5_ctrl, vcov6_ctrl, vcov7_ctrl, vcov8_ctrl, vcov8b_ctrl),
  stars     = TRUE,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_omit = "factor\\(country\\)|factor\\(decade\\)",
  coef_map  = coef_labels_ctrl,
  gof_map   = c("nobs", "r.squared"),
  add_rows  = fe_rows_ctrl,
  output    = "/Users/almaowing/Documents/MAthesis/tables/table_h1_ctrl_robustness.tex",
  title     = "Student-led protests and democratic demands, alternative controls robustness (H1)",
  notes     = "Cluster-robust standard errors by country in parentheses. M1--M4 use student domination; M5--M8 use the student involvement index. M3, M4, M7, M8 add CSO repression (v2csreprss) and political corruption (v2x\\_corr). M4b, M8b additionally add OWID mean years of schooling (post-1870 coverage)."
)


#scalebox wrap:
h1_ctrl_tex <- readLines("/Users/almaowing/Documents/MAthesis/tables/table_h1_ctrl_robustness.tex")
h1_ctrl_tex <- gsub("\\\\centering", "\\\\centering\n\\\\scalebox{0.72}{", h1_ctrl_tex)
h1_ctrl_tex <- gsub("\\\\end\\{talltblr\\}", "\\\\end{talltblr}\n}", h1_ctrl_tex)
writeLines(h1_ctrl_tex, "/Users/almaowing/Documents/MAthesis/tables/table_h1_ctrl_robustness.tex")



# with GDP growth from V-Dem and from WB (post 1960):

# join both growth measures from cy_omg into omg_campaign:
omg_campaign <- omg_campaign %>%
  select(-any_of(c("wb_gdp_growth", "wb_gdp_growth.x", "wb_gdp_growth.y",
                   "gdp_growth", "gdp_growth.x", "gdp_growth.y"))) %>%
  left_join(
    cy_omg %>% select(country_id, year, wb_gdp_growth, gdp_growth),
    by = c("country_id", "start_year" = "year")
  )

# coverage checks
cat("Campaigns with wb_gdp_growth (1960-2019):",
    sum(!is.na(omg_campaign$wb_gdp_growth) &
          omg_campaign$start_year >= 1960), "\n")
cat("Campaigns with gdp_growth (V-Dem-derived, full period):",
    sum(!is.na(omg_campaign$gdp_growth)), "\n")


#WB GDP growth subset (post-1960) 
omg_campaign_wb <- omg_campaign %>% filter(start_year >= 1960)


#M1-M4: WB GDP growth (post-1960):
model3_wb <- lm(dem_demand_any ~ student_protest +
                  working_group + educated_group +
                  elite_group + identity_group +
                  regime_context + log_gdppc + wb_gdp_growth +
                  factor(country) + factor(decade),
                data = omg_campaign_wb)

model4_wb <- lm(dem_demand_any ~ student_protest +
                  working_group + educated_group +
                  elite_group + identity_group +
                  regime_context + log_gdppc + peak_size + nonviolent +
                  wb_gdp_growth +
                  factor(country) + factor(decade),
                data = omg_campaign_wb)

model7_wb <- lm(dem_demand_any ~ student_index +
                  working_group + educated_group +
                  elite_group + identity_group +
                  regime_context + log_gdppc + wb_gdp_growth +
                  factor(country) + factor(decade),
                data = omg_campaign_wb)

model8_wb <- lm(dem_demand_any ~ student_index +
                  working_group + educated_group +
                  elite_group + identity_group +
                  regime_context + log_gdppc + peak_size + nonviolent +
                  wb_gdp_growth +
                  factor(country) + factor(decade),
                data = omg_campaign_wb)


#M5-M8: V-Dem-derived GDP growth (full period):
model3_vd <- lm(dem_demand_any ~ student_protest +
                  working_group + educated_group +
                  elite_group + identity_group +
                  regime_context + log_gdppc + gdp_growth +
                  factor(country) + factor(decade),
                data = omg_campaign)

model4_vd <- lm(dem_demand_any ~ student_protest +
                  working_group + educated_group +
                  elite_group + identity_group +
                  regime_context + log_gdppc + peak_size + nonviolent +
                  gdp_growth +
                  factor(country) + factor(decade),
                data = omg_campaign)

model7_vd <- lm(dem_demand_any ~ student_index +
                  working_group + educated_group +
                  elite_group + identity_group +
                  regime_context + log_gdppc + gdp_growth +
                  factor(country) + factor(decade),
                data = omg_campaign)

model8_vd <- lm(dem_demand_any ~ student_index +
                  working_group + educated_group +
                  elite_group + identity_group +
                  regime_context + log_gdppc + peak_size + nonviolent +
                  gdp_growth +
                  factor(country) + factor(decade),
                data = omg_campaign)


#clustered SEs:
vcov3_wb <- vcovCL(model3_wb, cluster = ~ country)
vcov4_wb <- vcovCL(model4_wb, cluster = ~ country)
vcov7_wb <- vcovCL(model7_wb, cluster = ~ country)
vcov8_wb <- vcovCL(model8_wb, cluster = ~ country)
vcov3_vd <- vcovCL(model3_vd, cluster = ~ country)
vcov4_vd <- vcovCL(model4_vd, cluster = ~ country)
vcov7_vd <- vcovCL(model7_vd, cluster = ~ country)
vcov8_vd <- vcovCL(model8_vd, cluster = ~ country)


# lets see:
coeftest(model4_wb, vcov = vcov4_wb)
coeftest(model4_vd, vcov = vcov4_vd)
coeftest(model8_wb, vcov = vcov8_wb)
coeftest(model8_vd, vcov = vcov8_vd)


#coefficient labels:
coef_labels_growth <- c(
  "(Intercept)"     = "Intercept",
  "student_protest" = "Student-led protest",
  "student_index"   = "Student involvement index",
  "working_group"   = "Working groups dominate",
  "educated_group"  = "Educated urban groups dominate",
  "elite_group"     = "Elite groups dominate",
  "identity_group"  = "Identity groups dominate",
  "regime_context"  = "Electoral democracy (V-Dem)",
  "log_gdppc"       = "GDP per capita (log)",
  "peak_size"       = "Campaign size",
  "nonviolent"      = "Nonviolent campaign",
  "wb_gdp_growth"   = "GDP growth (WB)",
  "gdp_growth"      = "GDP growth (V-Dem)"
)


#fixed effects rows:
fe_rows_growth <- data.frame(
  term  = c("Growth source", "Period", "Country FE", "Decade FE"),
  "M3"  = c("WB",    "Post-1960", "Yes", "Yes"),
  "M4"  = c("WB",    "Post-1960", "Yes", "Yes"),
  "M7"  = c("WB",    "Post-1960", "Yes", "Yes"),
  "M8"  = c("WB",    "Post-1960", "Yes", "Yes"),
  "M3v" = c("V-Dem", "Full",      "Yes", "Yes"),
  "M4v" = c("V-Dem", "Full",      "Yes", "Yes"),
  "M7v" = c("V-Dem", "Full",      "Yes", "Yes"),
  "M8v" = c("V-Dem", "Full",      "Yes", "Yes")
)


#export combined table:
modelsummary(
  list("M3" = model3_wb, "M4" = model4_wb, "M7" = model7_wb, "M8" = model8_wb,
       "M3v" = model3_vd, "M4v" = model4_vd, "M7v" = model7_vd, "M8v" = model8_vd),
  vcov      = list(vcov3_wb, vcov4_wb, vcov7_wb, vcov8_wb,
                   vcov3_vd, vcov4_vd, vcov7_vd, vcov8_vd),
  stars     = TRUE,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_omit = "factor\\(country\\)|factor\\(decade\\)",
  coef_map  = coef_labels_growth,
  gof_map   = c("nobs", "r.squared"),
  add_rows  = fe_rows_growth,
  output    = "/Users/almaowing/Documents/MAthesis/tables/table_h1_gdpgrowth_robustness_1.tex",
  title     = "Student-led protests and democratic demands, GDP growth robustness (H1)",
  notes     = "Cluster-robust standard errors by country in parentheses. M3-M8 use World Bank GDP growth (post-1960 sample); M3v-M8v use V-Dem-derived GDP growth (full historical period). Student-led protest in M3-M4 and M3v-M4v; student involvement index in M7-M8 and M7v-M8v."
)

#scalebox wrap:
h1_growth_tex <- readLines("/Users/almaowing/Documents/MAthesis/tables/table_h1_gdpgrowth_robustness_1.tex")
h1_growth_tex <- gsub("\\\\centering", "\\\\centering\n\\\\scalebox{0.72}{", h1_growth_tex)
h1_growth_tex <- gsub("\\\\end\\{talltblr\\}", "\\\\end{talltblr}\n}", h1_growth_tex)
writeLines(h1_growth_tex, "/Users/almaowing/Documents/MAthesis/tables/table_h1_gdpgrowth_robustness_1.tex")

#-------------------------------------------------------------------------------

#Alternative control variables

#Alternative control variables for H2

#data prep:

#pivot and merge wb_resources into cy_omg:
wb_resources_long <- wb_resources_raw %>%
  select(Country.Code, starts_with("X")) %>%
  select(-any_of("X")) %>%
  pivot_longer(
    cols      = starts_with("X"),
    names_to  = "year",
    values_to = "resource_rents"
  ) %>%
  mutate(year = as.integer(sub("X", "", year))) %>%
  filter(!is.na(resource_rents)) %>%
  rename(country_text_id = Country.Code)

cy_omg <- cy_omg %>%
  select(-any_of(c("resource_rents", "resource_rents.x", "resource_rents.y"))) %>%
  left_join(wb_resources_long %>% select(country_text_id, year, resource_rents),
            by = c("country_text_id", "year"))


#letss see:
cat("Country-years with v2csreprss:",    sum(!is.na(cy_omg$v2csreprss)),    "\n")
cat("Country-years with v2x_corr:",      sum(!is.na(cy_omg$v2x_corr)),      "\n")
cat("Country-years with e_peaveduc:",    sum(!is.na(cy_omg$e_peaveduc)),    "\n")
cat("Country-years with resource_rents:", sum(!is.na(cy_omg$resource_rents)), "\n")


#Wald test:

#M4 alt controls (without resource_rents):
h2_vif_ctrl_protest <- lm(
  electoral_democracy_t1 ~ student_protest + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani +
    e_peaveduc + v2csreprss + v2x_corr,
  data = cy_omg %>%
    filter(!is.na(electoral_democracy_t1) & year >= 1960) %>%
    mutate(across(c(log_gdppc, gdp_growth, log_pop, e_miurbani,
                    e_peaveduc, v2csreprss, v2x_corr),
                  ~ifelse(is.finite(.), ., NA))) %>%
    tidyr::drop_na(electoral_democracy_t1, student_protest, v2x_polyarchy,
                   log_gdppc, gdp_growth, log_pop, e_miurbani,
                   e_peaveduc, v2csreprss, v2x_corr)
)


#M8b alt controls (with resource_rents):
h2_vif_ctrl_index_full <- lm(
  electoral_democracy_t1 ~ student_index_cy + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani +
    e_peaveduc + v2csreprss + v2x_corr + resource_rents,
  data = cy_omg %>%
    filter(!is.na(electoral_democracy_t1) & year >= 1960) %>%
    mutate(across(c(log_gdppc, gdp_growth, log_pop, e_miurbani,
                    e_peaveduc, v2csreprss, v2x_corr, resource_rents),
                  ~ifelse(is.finite(.), ., NA))) %>%
    tidyr::drop_na(electoral_democracy_t1, student_index_cy, v2x_polyarchy,
                   log_gdppc, gdp_growth, log_pop, e_miurbani,
                   e_peaveduc, v2csreprss, v2x_corr, resource_rents)
)


#console output:
cat("\nVIF — H2 M4 with alt controls:\n")
print(vif(h2_vif_ctrl_protest))

cat("\nVIF — H2 M8b with alt controls + resource_rents:\n")
print(vif(h2_vif_ctrl_index_full))




# ── export H2 alt-controls VIF table ────────────────────────────────────────
v_h2_ctrl_p <- vif(h2_vif_ctrl_protest)
v_h2_ctrl_i <- vif(h2_vif_ctrl_index_full)

vif_h2_ctrl_labels <- c(
  "student_protest"  = "Student-led protest",
  "student_index_cy" = "Student involvement index",
  "v2x_polyarchy"    = "Electoral democracy (lagged)",
  "log_gdppc"        = "GDP per capita (log)",
  "gdp_growth"       = "GDP growth",
  "log_pop"          = "Population (log)",
  "e_miurbani"       = "Urbanization (V-Dem)",
  "e_peaveduc"       = "Education level (V-Dem)",
  "v2csreprss"       = "CSO repression",
  "v2x_corr"         = "Political corruption",
  "resource_rents"   = "Resource rents (\\% GDP)"
)

#export H2 alt-controls VIF table:
v_h2_ctrl_p <- vif(h2_vif_ctrl_protest)
v_h2_ctrl_i <- vif(h2_vif_ctrl_index_full)

vif_h2_ctrl_labels <- c(
  "student_protest"  = "Student-led protest",
  "student_index_cy" = "Student involvement index",
  "v2x_polyarchy"    = "Electoral democracy (lagged)",
  "log_gdppc"        = "GDP per capita (log)",
  "gdp_growth"       = "GDP growth",
  "log_pop"          = "Population (log)",
  "e_miurbani"       = "Urbanization (V-Dem)",
  "e_peaveduc"       = "Education level (V-Dem)",
  "v2csreprss"       = "CSO repression",
  "v2x_corr"         = "Political corruption",
  "resource_rents"   = "Resource rents (\\% GDP)"
)

all_vars_ctrl <- names(vif_h2_ctrl_labels)
vif_h2_ctrl_tab <- tibble(
  Variable             = vif_h2_ctrl_labels[all_vars_ctrl],
  `M4 (alt controls)`  = sapply(all_vars_ctrl, function(v) {
    if (v %in% names(v_h2_ctrl_p)) sprintf("%.2f", v_h2_ctrl_p[[v]]) else "--"
  }),
  `M8b (alt controls)` = sapply(all_vars_ctrl, function(v) {
    if (v %in% names(v_h2_ctrl_i)) sprintf("%.2f", v_h2_ctrl_i[[v]]) else "--"
  })
)

kable(vif_h2_ctrl_tab, format = "latex", booktabs = TRUE, align = "lcc",
      caption = "Variance inflation factors for H2 alternative control specifications",
      label   = "vif_h2_ctrl") %>%
  kable_styling(latex_options = "hold_position") %>%
  footnote(general = "VIFs for the alternative control specifications of H2 (Table A20), computed from OLS without country and year fixed effects. Sample restricted to country-years with non-finite values dropped. Several covariates fall in the marginally problematic range (VIF 5--7), supporting the more parsimonious main specification.",
           general_title = "Note:", footnote_as_chunk = TRUE, threeparttable = TRUE) %>%
  save_kable("/Users/almaowing/Documents/MAthesis/tables/table_vif_h2_ctrl.tex")

#descritpive stats:

var_info_h2_robust <- data.frame(
  var_name = c("e_peaveduc", "v2csreprss", "v2x_corr",
               "resource_rents", "wb_urbanization", "wb_gdp_growth"),
  label = c(
    "Education level (V-Dem)",
    "CSO repression (V-Dem)",
    "Political corruption (V-Dem)",
    "Natural resource rents, \\% GDP (World Bank)",
    "Urbanization, post-1960 (World Bank)",
    "GDP growth, post-1960 (World Bank)"
  ),
  type = rep("Continuous", 6),
  stringsAsFactors = FALSE
)

desc_h2_robust <- do.call(rbind, lapply(seq_len(nrow(var_info_h2_robust)), function(i) {
  x <- cy_omg[[ var_info_h2_robust$var_name[i] ]]
  x <- x[!is.na(x)]
  data.frame(
    Variable = var_info_h2_robust$label[i],
    Type     = var_info_h2_robust$type[i],
    Min      = round(min(x),  2),
    Mean     = round(mean(x), 2),
    Max      = round(max(x),  2),
    stringsAsFactors = FALSE
  )
}))

print(desc_h2_robust)


#export to LaTeX:
kbl(
  desc_h2_robust,
  format   = "latex",
  booktabs = TRUE,
  caption  = "Descriptive Statistics -- Additional Controls (H2)",
  label    = "desc_h2_robust",
  linesep  = "",
  escape   = FALSE
) %>%
  kable_styling(latex_options = "hold_position") %>%
  footnote(general = "Descriptive statistics for the additional control variables introduced in the H2 robustness checks (Table A20). Sample: country-years from the V-Dem dataset, 1789--2019. Education level, CSO repression, and political corruption from V-Dem; natural resource rents (as percentage of GDP) from the World Bank, restricted to 1970--2019 due to data coverage.",
           general_title = "Note:", footnote_as_chunk = TRUE, threeparttable = TRUE,
           escape = FALSE) %>%
  save_kable("/Users/almaowing/Documents/MAthesis/tables/desc_h2_robust.tex")


#OLS models:

#M1-M4: campaign_type:
m1_ctrl <- feols(
  electoral_democracy_t1 ~ campaign_type
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1))
)

m2_ctrl <- feols(
  electoral_democracy_t1 ~ campaign_type + v2x_polyarchy
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1))
)

m3_ctrl <- feols(
  electoral_democracy_t1 ~ campaign_type + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani +
    e_peaveduc + v2csreprss + v2x_corr
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1))
)

m4_ctrl <- feols(
  electoral_democracy_t1 ~ campaign_type + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani +
    e_peaveduc + v2csreprss + v2x_corr
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1960)
)

m4b_ctrl <- feols(
  electoral_democracy_t1 ~ campaign_type + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani +
    e_peaveduc + v2csreprss + v2x_corr + resource_rents
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1960)
)


#M5-M8: student_index_cy:
m5_ctrl <- feols(
  electoral_democracy_t1 ~ student_index_cy
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1))
)

m6_ctrl <- feols(
  electoral_democracy_t1 ~ student_index_cy + v2x_polyarchy
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1))
)

m7_ctrl <- feols(
  electoral_democracy_t1 ~ student_index_cy + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani +
    e_peaveduc + v2csreprss + v2x_corr
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1))
)

m8_ctrl <- feols(
  electoral_democracy_t1 ~ student_index_cy + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani +
    e_peaveduc + v2csreprss + v2x_corr
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1960)
)

m8b_ctrl <- feols(
  electoral_democracy_t1 ~ student_index_cy + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani +
    e_peaveduc + v2csreprss + v2x_corr + resource_rents
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1960)
)


#lets see:
summary(m4_ctrl)
summary(m4b_ctrl)
summary(m8_ctrl)
summary(m8b_ctrl)


#coefficient labels:
coef_labels_h2_ctrl <- c(
  "campaign_typestudent"        = "Student-led protest",
  "campaign_typeother_campaign" = "Other campaign",
  "student_index_cy"            = "Student involvement index",
  "v2x_polyarchy"               = "Electoral democracy (lagged)",
  "log_gdppc"                   = "GDP per capita (log)",
  "gdp_growth"                  = "GDP growth",
  "log_pop"                     = "Population (log)",
  "e_miurbani"                  = "Urbanization (V-Dem)",
  "e_peaveduc"                  = "Education level (V-Dem)",
  "v2csreprss"                  = "CSO repression",
  "v2x_corr"                    = "Political corruption",
  "resource_rents"              = "Resource rents (\\% GDP)"
)


#fixed effects rows:
fe_rows_h2_ctrl <- data.frame(
  term   = c("Period", "Lagged democracy", "Country FE", "Year FE"),
  "M1"   = c("Full",      "No",  "Yes", "Yes"),
  "M2"   = c("Full",      "Yes", "Yes", "Yes"),
  "M3"   = c("Full",      "Yes", "Yes", "Yes"),
  "M4"   = c("Post-1960", "Yes", "Yes", "Yes"),
  "M4b"  = c("Post-1970", "Yes", "Yes", "Yes"),
  "M5"   = c("Full",      "No",  "Yes", "Yes"),
  "M6"   = c("Full",      "Yes", "Yes", "Yes"),
  "M7"   = c("Full",      "Yes", "Yes", "Yes"),
  "M8"   = c("Post-1960", "Yes", "Yes", "Yes"),
  "M8b"  = c("Post-1970", "Yes", "Yes", "Yes")
)


#export table:
modelsummary(
  list("M1" = m1_ctrl, "M2" = m2_ctrl, "M3" = m3_ctrl, "M4" = m4_ctrl, "M4b" = m4b_ctrl,
       "M5" = m5_ctrl, "M6" = m6_ctrl, "M7" = m7_ctrl, "M8" = m8_ctrl, "M8b" = m8b_ctrl),
  stars     = TRUE,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map  = coef_labels_h2_ctrl,
  gof_map   = c("nobs", "r.squared"),
  add_rows  = fe_rows_h2_ctrl,
  output    = "/Users/almaowing/Documents/MAthesis/tables/table_h2_ctrl_robustness_1.tex",
  title     = "Student-led protests and democratization, alternative controls robustness (H2)",
  notes     = "Cluster-robust standard errors by country in parentheses. DV = V-Dem electoral democracy index at t+1. M3, M4, M7, M8 add V-Dem education (e\\_peaveduc), CSO repression (v2csreprss), and political corruption (v2x\\_corr) to the main spec. M4b, M8b additionally add World Bank resource rents as \\% of GDP (post-1970)."
)


#scalebox wrap
h2_ctrl_tex <- readLines("/Users/almaowing/Documents/MAthesis/tables/table_h2_ctrl_robustness_1.tex")
h2_ctrl_tex <- gsub("\\\\centering", "\\\\centering\n\\\\scalebox{0.65}{", h2_ctrl_tex)
h2_ctrl_tex <- gsub("\\\\end\\{talltblr\\}", "\\\\end{talltblr}\n}", h2_ctrl_tex)
writeLines(h2_ctrl_tex, "/Users/almaowing/Documents/MAthesis/tables/table_h2_ctrl_robustness_1.tex")


# Testing with wb_urbanization and gdp_growth bc of coverage:

# m4 (already existing):
m4r <- feols(
  electoral_democracy_t1 ~ campaign_type + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + wb_urbanization
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1960)
)

m4r2 <- feols(
  electoral_democracy_t1 ~ campaign_type + v2x_polyarchy +
    log_gdppc + wb_gdp_growth + log_pop + wb_urbanization
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1960)
)


#M8 / M8r / M8r2: student_index_cy (post-1960):

# m8 (already exist)
m8r <- feols(
  electoral_democracy_t1 ~ student_index_cy + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + wb_urbanization
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1960)
)

m8r2 <- feols(
  electoral_democracy_t1 ~ student_index_cy + v2x_polyarchy +
    log_gdppc + wb_gdp_growth + log_pop + wb_urbanization
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1960)
)


# lets see:
summary(m4r)
summary(m4r2)
summary(m8r)
summary(m8r2)


#coefficient labels
coef_labels_h2_wb <- c(
  "campaign_typestudent"        = "Student-led protest",
  "campaign_typeother_campaign" = "Other campaign",
  "student_index_cy"            = "Student involvement index",
  "v2x_polyarchy"               = "Electoral democracy (lagged)",
  "log_gdppc"                   = "GDP per capita (log)",
  "gdp_growth"                  = "GDP growth (V-Dem)",
  "wb_gdp_growth"               = "GDP growth (World Bank)",
  "log_pop"                     = "Population (log)",
  "e_miurbani"                  = "Urbanization (V-Dem)",
  "wb_urbanization"             = "Urbanization (World Bank)"
)


#fixed effects rows:
fe_rows_h2_wb <- data.frame(
  term     = c("Period", "Urbanization", "GDP growth", "Country FE", "Year FE"),
  "M4"     = c("Post-1960", "V-Dem",      "V-Dem", "Yes", "Yes"),
  "M4r"    = c("Post-1960", "World Bank", "V-Dem", "Yes", "Yes"),
  "M4r2"   = c("Post-1960", "World Bank", "WB",    "Yes", "Yes"),
  "M8"     = c("Post-1960", "V-Dem",      "V-Dem", "Yes", "Yes"),
  "M8r"    = c("Post-1960", "World Bank", "V-Dem", "Yes", "Yes"),
  "M8r2"   = c("Post-1960", "World Bank", "WB",    "Yes", "Yes")
)


#export table:
modelsummary(
  list("M4"   = m4,   "M4r" = m4r,  "M4r2" = m4r2,
       "M8"   = m8,   "M8r" = m8r,  "M8r2" = m8r2),
  stars     = TRUE,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map  = coef_labels_h2_wb,
  gof_map   = c("nobs", "r.squared"),
  add_rows  = fe_rows_h2_wb,
  output    = "/Users/almaowing/Documents/MAthesis/tables/table_h2_urbann_1.tex",
  title     = "Student-Led Protests and Democratization, World Bank Urbanization and GDP Growth as Added Control Variables (H2, post-1960)",
  notes     = "Cluster-robust standard errors by country in parentheses. DV = V-Dem electoral democracy index at t+1. Sample restricted to 1960-2019 due to WB coverage. M4/M8 are post-1960 main models. M4r/M8r replace V-Dem urbanization with WB urbanization. M4r2/M8r2 additionally replace V-Dem-derived GDP growth with WB GDP growth."
)


#scalebox wrap:
h2_wb_tex <- readLines("/Users/almaowing/Documents/MAthesis/tables/table_h2_urbann_1.tex")
h2_wb_tex <- gsub("\\\\centering", "\\\\centering\n\\\\scalebox{0.78}{", h2_wb_tex)
h2_wb_tex <- gsub("\\\\end\\{talltblr\\}", "\\\\end{talltblr}\n}", h2_wb_tex)
writeLines(h2_wb_tex, "/Users/almaowing/Documents/MAthesis/tables/table_h2_urbann_1.tex")

#doing the wald test with the new post 1960 estimates:
linearHypothesis(m4r,  "campaign_typestudent = campaign_typeother_campaign")
linearHypothesis(m4r2, "campaign_typestudent = campaign_typeother_campaign")

#-------------------------------------------------------------------------------

#Alternative model specification

#Alternative model specificationf or H1 (logit):

#M1-M4: student_protest:
logit1 <- glm(dem_demand_any ~ student_protest,
              data = omg_campaign, family = binomial(link = "logit"))

logit2 <- glm(dem_demand_any ~ student_protest +
                working_group + educated_group +
                elite_group + identity_group +
                factor(country) + factor(decade),
              data = omg_campaign, family = binomial(link = "logit"))

logit3 <- glm(dem_demand_any ~ student_protest +
                working_group + educated_group +
                elite_group + identity_group +
                regime_context + log_gdppc +
                factor(country) + factor(decade),
              data = omg_campaign, family = binomial(link = "logit"))

logit4 <- glm(dem_demand_any ~ student_protest +
                working_group + educated_group +
                elite_group + identity_group +
                regime_context + log_gdppc + peak_size + nonviolent +
                factor(country) + factor(decade),
              data = omg_campaign, family = binomial(link = "logit"))


#M5-M8: student_index:
logit5 <- glm(dem_demand_any ~ student_index,
              data = omg_campaign, family = binomial(link = "logit"))

logit6 <- glm(dem_demand_any ~ student_index +
                working_group + educated_group +
                elite_group + identity_group +
                factor(country) + factor(decade),
              data = omg_campaign, family = binomial(link = "logit"))

logit7 <- glm(dem_demand_any ~ student_index +
                working_group + educated_group +
                elite_group + identity_group +
                regime_context + log_gdppc +
                factor(country) + factor(decade),
              data = omg_campaign, family = binomial(link = "logit"))

logit8 <- glm(dem_demand_any ~ student_index +
                working_group + educated_group +
                elite_group + identity_group +
                regime_context + log_gdppc + peak_size + nonviolent +
                factor(country) + factor(decade),
              data = omg_campaign, family = binomial(link = "logit"))


#clustered SE for each model:
vcov_logit1 <- vcovCL(logit1, cluster = ~ country)
vcov_logit2 <- vcovCL(logit2, cluster = ~ country)
vcov_logit3 <- vcovCL(logit3, cluster = ~ country)
vcov_logit4 <- vcovCL(logit4, cluster = ~ country)
vcov_logit5 <- vcovCL(logit5, cluster = ~ country)
vcov_logit6 <- vcovCL(logit6, cluster = ~ country)
vcov_logit7 <- vcovCL(logit7, cluster = ~ country)
vcov_logit8 <- vcovCL(logit8, cluster = ~ country)


#AME for M4 and M8 (repored in main text):
cat("\n--- AME: student_protest (M4 logit) ---\n")
print(avg_slopes(logit4, variables = "student_protest"))

cat("\n--- AME: student_index (M8 logit) ---\n")
print(avg_slopes(logit8, variables = "student_index"))


#coefficient labels:
coef_labels_logit <- c(
  "(Intercept)"     = "Intercept",
  "student_protest" = "Student-led protest",
  "student_index"   = "Student involvement index",
  "working_group"   = "Working groups dominate",
  "educated_group"  = "Educated urban groups dominate",
  "elite_group"     = "Elite groups dominate",
  "identity_group"  = "Identity groups dominate",
  "regime_context"  = "Electoral democracy (V-Dem)",
  "log_gdppc"       = "GDP per capita (log)",
  "peak_size"       = "Campaign size",
  "nonviolent"      = "Nonviolent campaign"
)


#fixed effects rows:
fe_rows_logit <- data.frame(
  term = c("Country FE", "Decade FE"),
  "M1" = c("No",  "No"),
  "M2" = c("Yes", "Yes"),
  "M3" = c("Yes", "Yes"),
  "M4" = c("Yes", "Yes"),
  "M5" = c("No",  "No"),
  "M6" = c("Yes", "Yes"),
  "M7" = c("Yes", "Yes"),
  "M8" = c("Yes", "Yes")
)


#export Table A22
modelsummary(
  list("M1" = logit1, "M2" = logit2, "M3" = logit3, "M4" = logit4,
       "M5" = logit5, "M6" = logit6, "M7" = logit7, "M8" = logit8),
  vcov      = list(vcov_logit1, vcov_logit2, vcov_logit3, vcov_logit4,
                   vcov_logit5, vcov_logit6, vcov_logit7, vcov_logit8),
  stars     = TRUE,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_omit = "factor\\(country\\)|factor\\(decade\\)",
  coef_map  = coef_labels_logit,
  gof_map   = c("nobs"),
  add_rows  = fe_rows_logit,
  output    = "/Users/almaowing/Documents/MAthesis/tables/table_h1_logit.tex",
  title     = "Student-led protests and democratic demands, logit estimation (H1)",
  notes     = "Cluster-robust standard errors by country in parentheses. Coefficients are log-odds; country and decade fixed effects included but not shown. M1--M4 use student domination; M5--M8 use the student involvement index (0--3). Average marginal effects for M4 and M8 are reported in the main text."
)


#scalebox wrap:
logit_tex <- readLines("/Users/almaowing/Documents/MAthesis/tables/table_h1_logit.tex")
logit_tex <- gsub("\\\\centering", "\\\\centering\n\\\\scalebox{0.82}{", logit_tex)
logit_tex <- gsub("\\\\end\\{talltblr\\}", "\\\\end{talltblr}\n}", logit_tex)
writeLines(logit_tex, "/Users/almaowing/Documents/MAthesis/tables/table_h1_logit.tex")

#-------------------------------------------------------------------------------

#alternative model specification for h2 (without electoral democracy control):

#M3/M4 spec without lagged democracy (campaign_type):
m_strict_protest_full <- feols(
  electoral_democracy_t1 ~ campaign_type +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1))
)

m_strict_protest_1960 <- feols(
  electoral_democracy_t1 ~ campaign_type +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1960)
)


#M7/M8 spec without lagged democracy (student_index_cy):
m_strict_index_full <- feols(
  electoral_democracy_t1 ~ student_index_cy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1))
)

m_strict_index_1960 <- feols(
  electoral_democracy_t1 ~ student_index_cy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1960)
)


#lets see:
summary(m_strict_protest_full)
summary(m_strict_protest_1960)
summary(m_strict_index_full)
summary(m_strict_index_1960)


#Wald test for M_strict (campaign_typestudent vs other_campaign):
linearHypothesis(m_strict_protest_full, "campaign_typestudent = campaign_typeother_campaign")


#coefficient labels:
coef_labels_strict <- c(
  "campaign_typestudent"        = "Student-led protest",
  "campaign_typeother_campaign" = "Other campaign",
  "student_index_cy"            = "Student involvement index",
  "log_gdppc"                   = "GDP per capita (log)",
  "gdp_growth"                  = "GDP growth",
  "log_pop"                     = "Population (log)",
  "e_miurbani"                  = "Urbanization (V-Dem)"
)


#fixed effects rows:
fe_rows_strict <- data.frame(
  term       = c("Period", "Lagged democracy", "Country FE", "Year FE"),
  "M3"       = c("Full",      "No", "Yes", "Yes"),
  "M4"       = c("Post-1960", "No", "Yes", "Yes"),
  "M7"       = c("Full",      "No", "Yes", "Yes"),
  "M8"       = c("Post-1960", "No", "Yes", "Yes")
)


#export table:
modelsummary(
  list("M3" = m_strict_protest_full,
       "M4" = m_strict_protest_1960,
       "M7" = m_strict_index_full,
       "M8" = m_strict_index_1960),
  stars     = TRUE,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map  = coef_labels_strict,
  gof_map   = c("nobs", "r.squared"),
  add_rows  = fe_rows_strict,
  output    = "/Users/almaowing/Documents/MAthesis/tables/table_h2_strict_robustness_10.tex",
  title     = "Student-led protests and democratization, without lagged democracy control (H2)",
  notes     = "Cluster-robust standard errors by country in parentheses. DV = V-Dem electoral democracy index at t+1. Specifications match main M3, M4, M7, M8 but exclude lagged electoral democracy. M3, M7 use full historical sample; M4, M8 post-1960."
)


#scalebox wrap:
h2_strict_tex <- readLines("/Users/almaowing/Documents/MAthesis/tables/table_h2_strict_robustness_10.tex")
h2_strict_tex <- gsub("\\\\centering", "\\\\centering\n\\\\scalebox{0.82}{", h2_strict_tex)
h2_strict_tex <- gsub("\\\\end\\{talltblr\\}", "\\\\end{talltblr}\n}", h2_strict_tex)
writeLines(h2_strict_tex, "/Users/almaowing/Documents/MAthesis/tables/table_h2_strict_robustness_10.tex")

#-------------------------------------------------------------------------------

#Theoretically motivated subsamples

# 1900, 1960 and 2000 for H1:

#M4 (student_protest) — post-1900, post-1960, post-2000:
model_1900_h1 <- lm(dem_demand_any ~ student_protest +
                      working_group + educated_group +
                      elite_group + identity_group +
                      regime_context + log_gdppc + peak_size + nonviolent +
                      factor(country) + factor(decade),
                    data = omg_campaign %>% filter(start_year >= 1900))

model_1960_h1 <- lm(dem_demand_any ~ student_protest +
                      working_group + educated_group +
                      elite_group + identity_group +
                      regime_context + log_gdppc + peak_size + nonviolent +
                      factor(country) + factor(decade),
                    data = omg_campaign %>% filter(start_year >= 1960))

model_2000_h1 <- lm(dem_demand_any ~ student_protest +
                      working_group + educated_group +
                      elite_group + identity_group +
                      regime_context + log_gdppc + peak_size + nonviolent +
                      factor(country) + factor(decade),
                    data = omg_campaign %>% filter(start_year >= 2000))


#M8 (student_index) — post-1900, post-1960, post-2000:
model_1900_h1_index <- lm(dem_demand_any ~ student_index +
                            working_group + educated_group +
                            elite_group + identity_group +
                            regime_context + log_gdppc + peak_size + nonviolent +
                            factor(country) + factor(decade),
                          data = omg_campaign %>% filter(start_year >= 1900))

model_1960_h1_index <- lm(dem_demand_any ~ student_index +
                            working_group + educated_group +
                            elite_group + identity_group +
                            regime_context + log_gdppc + peak_size + nonviolent +
                            factor(country) + factor(decade),
                          data = omg_campaign %>% filter(start_year >= 1960))

model_2000_h1_index <- lm(dem_demand_any ~ student_index +
                            working_group + educated_group +
                            elite_group + identity_group +
                            regime_context + log_gdppc + peak_size + nonviolent +
                            factor(country) + factor(decade),
                          data = omg_campaign %>% filter(start_year >= 2000))


#clustered SEs:
vcov_1900_h1       <- vcovCL(model_1900_h1,       cluster = ~ country)
vcov_1960_h1       <- vcovCL(model_1960_h1,       cluster = ~ country)
vcov_2000_h1       <- vcovCL(model_2000_h1,       cluster = ~ country)
vcov_1900_h1_index <- vcovCL(model_1900_h1_index, cluster = ~ country)
vcov_1960_h1_index <- vcovCL(model_1960_h1_index, cluster = ~ country)
vcov_2000_h1_index <- vcovCL(model_2000_h1_index, cluster = ~ country)


#lets see:
coeftest(model_2000_h1,       vcov = vcov_2000_h1)
coeftest(model_2000_h1_index, vcov = vcov_2000_h1_index)


#fixed effects rows:
fe_rows_h1_subsample <- data.frame(
  term         = c("Period", "Country FE", "Decade FE"),
  "M4 (1900+)" = c("Post-1900", "Yes", "Yes"),
  "M4 (1960+)" = c("Post-1960", "Yes", "Yes"),
  "M4 (2000+)" = c("Post-2000", "Yes", "Yes"),
  "M8 (1900+)" = c("Post-1900", "Yes", "Yes"),
  "M8 (1960+)" = c("Post-1960", "Yes", "Yes"),
  "M8 (2000+)" = c("Post-2000", "Yes", "Yes"),
  check.names = FALSE
)


#export table:
modelsummary(
  list("M4 (1900+)" = model_1900_h1, "M4 (1960+)" = model_1960_h1, "M4 (2000+)" = model_2000_h1,
       "M8 (1900+)" = model_1900_h1_index, "M8 (1960+)" = model_1960_h1_index, "M8 (2000+)" = model_2000_h1_index),
  vcov      = list(vcov_1900_h1, vcov_1960_h1, vcov_2000_h1,
                   vcov_1900_h1_index, vcov_1960_h1_index, vcov_2000_h1_index),
  stars     = TRUE,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_omit = "factor\\(country\\)|factor\\(decade\\)",
  coef_map  = coef_labels_h1,
  gof_map   = c("nobs", "r.squared"),
  add_rows  = fe_rows_h1_subsample,
  output    = "/Users/almaowing/Documents/MAthesis/tables/table_h1_subsample_robustness.tex",
  title     = "Student-led protests and democratic demands, theoretically motivated subsamples (H1)",
  notes     = "Cluster-robust standard errors by country in parentheses. Specifications match main M4 and M8 (full controls including mediators). Post-1900 captures emergence of modern universities; post-1960 the global surge in student activism; post-2000 contemporary color revolutions and Arab Spring. M4 columns use student domination; M8 columns use the student involvement index (0--3)."
)


#scalebox wrap:
h1_sub_tex <- readLines("/Users/almaowing/Documents/MAthesis/tables/table_h1_subsample_robustness.tex")
h1_sub_tex <- gsub("\\\\centering", "\\\\centering\n\\\\scalebox{0.72}{", h1_sub_tex)
h1_sub_tex <- gsub("\\\\end\\{talltblr\\}", "\\\\end{talltblr}\n}", h1_sub_tex)
writeLines(h1_sub_tex, "/Users/almaowing/Documents/MAthesis/tables/table_h1_subsample_robustness.tex")

#-------------------------------------------------------------------------------

# 1900, 1945 and 1973 for H2:

#M4 (campaign_type) — post-1900, post-1945, post-1974:
m_1900_protest <- feols(
  electoral_democracy_t1 ~ campaign_type + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1900)
)

m_1945_protest <- feols(
  electoral_democracy_t1 ~ campaign_type + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1945)
)

m_1974_protest <- feols(
  electoral_democracy_t1 ~ campaign_type + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1974)
)


#M8 (student_index_cy) — post-1900, post-1945, post-1974:
m_1900_index <- feols(
  electoral_democracy_t1 ~ student_index_cy + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1900)
)

m_1945_index <- feols(
  electoral_democracy_t1 ~ student_index_cy + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1945)
)

m_1974_index <- feols(
  electoral_democracy_t1 ~ student_index_cy + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1974)
)


# lets see:
summary(m_1900_protest)
summary(m_1945_protest)
summary(m_1974_protest)
summary(m_1900_index)
summary(m_1945_index)
summary(m_1974_index)


#coefficient labels:
coef_labels_subsample <- c(
  "campaign_typestudent"        = "Student-led protest",
  "campaign_typeother_campaign" = "Other campaign",
  "student_index_cy"            = "Student involvement index",
  "v2x_polyarchy"               = "Electoral democracy (lagged)",
  "log_gdppc"                   = "GDP per capita (log)",
  "gdp_growth"                  = "GDP growth",
  "log_pop"                     = "Population (log)",
  "e_miurbani"                  = "Urbanization (V-Dem)"
)


#fixed effects rows:
fe_rows_subsample <- data.frame(
  term            = c("Period", "Country FE", "Year FE"),
  "M4 (1900+)"    = c("Post-1900", "Yes", "Yes"),
  "M4 (1945+)"    = c("Post-1945", "Yes", "Yes"),
  "M4 (1974+)"    = c("Post-1974", "Yes", "Yes"),
  "M8 (1900+)"    = c("Post-1900", "Yes", "Yes"),
  "M8 (1945+)"    = c("Post-1945", "Yes", "Yes"),
  "M8 (1974+)"    = c("Post-1974", "Yes", "Yes"),
  check.names = FALSE
)


#export table:
modelsummary(
  list("M4 (1900+)" = m_1900_protest, "M4 (1945+)" = m_1945_protest, "M4 (1974+)" = m_1974_protest,
       "M8 (1900+)" = m_1900_index,   "M8 (1945+)" = m_1945_index,   "M8 (1974+)" = m_1974_index),
  stars     = TRUE,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map  = coef_labels_subsample,
  gof_map   = c("nobs", "r.squared"),
  add_rows  = fe_rows_subsample,
  output    = "/Users/almaowing/Documents/MAthesis/tables/table_h2_subsample_robustness.tex",
  title     = "Student-led protests and democratization, theoretically motivated subsamples (H2)",
  notes     = "Cluster-robust standard errors by country in parentheses. DV = V-Dem electoral democracy index at t+1. Post-1900 captures emergence of modern universities; post-1945 the full postwar era; post-1974 Third Wave of democratization. M4 columns use campaign type dummies; M8 columns use the student involvement index (0--3)."
)


#scalebox wrap:
h2_sub_tex <- readLines("/Users/almaowing/Documents/MAthesis/tables/table_h2_subsample_robustness.tex")
h2_sub_tex <- gsub("\\\\centering", "\\\\centering\n\\\\scalebox{0.72}{", h2_sub_tex)
h2_sub_tex <- gsub("\\\\end\\{talltblr\\}", "\\\\end{talltblr}\n}", h2_sub_tex)
writeLines(h2_sub_tex, "/Users/almaowing/Documents/MAthesis/tables/table_h2_subsample_robustness.tex")

#-------------------------------------------------------------------------------

#Time horizon test for H2:

#t+1, t+5, t+10
# construct electoral_democracy_t10
cy_omg <- cy_omg %>%
  arrange(country_id, year) %>%
  group_by(country_id) %>%
  mutate(
    electoral_democracy_t10 = dplyr::lead(v2x_polyarchy, 10)
  ) %>%
  ungroup()


#M4 spec across horizons (campaign_type):
m_t1_protest <- feols(
  electoral_democracy_t1 ~ campaign_type + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1960)
)

m_t5_protest <- feols(
  electoral_democracy_t5 ~ campaign_type + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t5) & year >= 1960)
)

m_t10_protest <- feols(
  electoral_democracy_t10 ~ campaign_type + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t10) & year >= 1960)
)


#M8 spec across horizons (student_index_cy):
m_t1_index <- feols(
  electoral_democracy_t1 ~ student_index_cy + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1960)
)

m_t5_index <- feols(
  electoral_democracy_t5 ~ student_index_cy + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t5) & year >= 1960)
)

m_t10_index <- feols(
  electoral_democracy_t10 ~ student_index_cy + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t10) & year >= 1960)
)


# lets see:
summary(m_t1_protest)
summary(m_t5_protest)
summary(m_t10_protest)
summary(m_t1_index)
summary(m_t5_index)
summary(m_t10_index)


#coefficient labels:
coef_labels_horizon <- c(
  "campaign_typestudent"        = "Student-led protest",
  "campaign_typeother_campaign" = "Other campaign",
  "student_index_cy"            = "Student involvement index",
  "v2x_polyarchy"               = "Electoral democracy (lagged)",
  "log_gdppc"                   = "GDP per capita (log)",
  "gdp_growth"                  = "GDP growth",
  "log_pop"                     = "Population (log)",
  "e_miurbani"                  = "Urbanization (V-Dem)"
)


#fixed effects rows:
fe_rows_horizon <- data.frame(
  term          = c("Horizon (DV)", "Period", "Country FE", "Year FE"),
  "M4 (t+1)"    = c("$t+1$",  "Post-1960", "Yes", "Yes"),
  "M4 (t+5)"    = c("$t+5$",  "Post-1960", "Yes", "Yes"),
  "M4 (t+10)"   = c("$t+10$", "Post-1960", "Yes", "Yes"),
  "M8 (t+1)"    = c("$t+1$",  "Post-1960", "Yes", "Yes"),
  "M8 (t+5)"    = c("$t+5$",  "Post-1960", "Yes", "Yes"),
  "M8 (t+10)"   = c("$t+10$", "Post-1960", "Yes", "Yes"),
  check.names = FALSE
)


#export table:
modelsummary(
  list("M4 (t+1)"  = m_t1_protest, "M4 (t+5)"  = m_t5_protest, "M4 (t+10)" = m_t10_protest,
       "M8 (t+1)"  = m_t1_index,   "M8 (t+5)"  = m_t5_index,   "M8 (t+10)" = m_t10_index),
  stars     = TRUE,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map  = coef_labels_horizon,
  gof_map   = c("nobs", "r.squared"),
  add_rows  = fe_rows_horizon,
  output    = "/Users/almaowing/Documents/MAthesis/tables/table_h2_horizon_robustness.tex",
  title     = "Student-led protests and democratization, alternative time horizons (H2)",
  notes     = "Cluster-robust standard errors by country in parentheses. Specifications match main M4 and M8 (post-1960, full controls) but use the V-Dem Electoral Democracy Index at t+1, t+5, or t+10 as the dependent variable. M4 columns use campaign type dummies; M8 columns use the student involvement index (0--3)."
)


#scalebox wrap
h2_horizon_tex <- readLines("/Users/almaowing/Documents/MAthesis/tables/table_h2_horizon_robustness.tex")
h2_horizon_tex <- gsub("\\\\centering", "\\\\centering\n\\\\scalebox{0.72}{", h2_horizon_tex)
h2_horizon_tex <- gsub("\\\\end\\{talltblr\\}", "\\\\end{talltblr}\n}", h2_horizon_tex)
writeLines(h2_horizon_tex, "/Users/almaowing/Documents/MAthesis/tables/table_h2_horizon_robustness.tex")

#-------------------------------------------------------------------------------
#Pre-tred test for H2

#create binary student campaign indicator + leads and lags:
cy_omg <- cy_omg %>%
  mutate(student_cy_bin = as.integer(campaign_type == "student")) %>%
  group_by(country_id) %>%
  arrange(year) %>%
  mutate(
    s_lead3 = dplyr::lead(student_cy_bin, 3),
    s_lead2 = dplyr::lead(student_cy_bin, 2),
    s_lead1 = dplyr::lead(student_cy_bin, 1),
    s_lag0  = student_cy_bin,
    s_lag1  = dplyr::lag(student_cy_bin, 1),
    s_lag2  = dplyr::lag(student_cy_bin, 2),
    s_lag3  = dplyr::lag(student_cy_bin, 3),
    s_lag4  = dplyr::lag(student_cy_bin, 4),
    s_lag5  = dplyr::lag(student_cy_bin, 5),
    v2x_polyarchy_l1 = dplyr::lag(v2x_polyarchy, 1)
  ) %>%
  ungroup()


#event study model (DV = current v2x_polyarchy):
m_event <- feols(
  v2x_polyarchy ~ s_lead3 + s_lead2 + s_lead1 +
    s_lag0 + s_lag1 + s_lag2 + s_lag3 + s_lag4 + s_lag5 +
    v2x_polyarchy_l1 + log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(year >= 1960)
)

summary(m_event)


#xtract coefficients and 95% CIs for plot:
event_terms <- c("s_lead3", "s_lead2", "s_lead1",
                 "s_lag0",  "s_lag1",  "s_lag2",
                 "s_lag3",  "s_lag4",  "s_lag5")

event_coefs <- data.frame(
  term     = event_terms,
  estimate = coef(m_event)[event_terms],
  ci_lo    = confint(m_event)[event_terms, 1],
  ci_hi    = confint(m_event)[event_terms, 2],
  time     = c(-3, -2, -1, 0, 1, 2, 3, 4, 5)
)

print(event_coefs)


#event study plot:
p_event <- ggplot(event_coefs, aes(x = time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey40", linewidth = 0.5) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, fill = "#2166ac") +
  geom_line(color = "#2166ac", linewidth = 0.8) +
  geom_point(color = "#2166ac", size = 2.5) +
  annotate("text", x = -1.7, y = max(event_coefs$ci_hi) * 0.95,
           label = "Pre-treatment", size = 3.2, color = "grey40", hjust = 1) +
  annotate("text", x = 0.3, y = max(event_coefs$ci_hi) * 0.95,
           label = "Post-treatment", size = 3.2, color = "grey40", hjust = 0) +
  scale_x_continuous(
    breaks = -3:5,
    labels = c("t-3", "t-2", "t-1", "t", "t+1", "t+2", "t+3", "t+4", "t+5")
  ) +
  labs(
    x = "Years relative to student-led campaign",
    y = "Effect on electoral democracy (V-Dem polyarchy)",
    title = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line          = element_line(color = "grey80")
  )


#save figure:
ggsave(
  "/Users/almaowing/Documents/MAthesis/figures/event_study_pretrend_1.pdf",
  p_event, width = 8, height = 4.5
)

cat("Event study plot saved.\n")
cat("Pre-trend coefficients (should be near zero):\n")
print(event_coefs[event_coefs$time < 0, c("time", "estimate", "ci_lo", "ci_hi")])





#-------------------------------------------------------------------------------

#Regime type subgroups

#first for H1:

#construct regime type variable on omg_campaign:
omg_campaign <- omg_campaign %>%
  mutate(
    regime_type = case_when(
      regime_context <  0.2 ~ "autocracy",
      regime_context >= 0.2 & regime_context <= 0.5 ~ "hybrid",
      regime_context >  0.5 ~ "democracy",
      TRUE ~ NA_character_
    )
  )


#M4 spec (student_protest) by regime type:
h1_auto_m4 <- lm(dem_demand_any ~ student_protest +
                   working_group + educated_group +
                   elite_group + identity_group +
                   regime_context + log_gdppc + peak_size + nonviolent +
                   factor(country) + factor(decade),
                 data = omg_campaign %>% filter(regime_type == "autocracy"))

h1_hyb_m4 <- lm(dem_demand_any ~ student_protest +
                  working_group + educated_group +
                  elite_group + identity_group +
                  regime_context + log_gdppc + peak_size + nonviolent +
                  factor(country) + factor(decade),
                data = omg_campaign %>% filter(regime_type == "hybrid"))

h1_dem_m4 <- lm(dem_demand_any ~ student_protest +
                  working_group + educated_group +
                  elite_group + identity_group +
                  regime_context + log_gdppc + peak_size + nonviolent +
                  factor(country) + factor(decade),
                data = omg_campaign %>% filter(regime_type == "democracy"))


#M8 spec (student_index) by regime type:
h1_auto_m8 <- lm(dem_demand_any ~ student_index +
                   working_group + educated_group +
                   elite_group + identity_group +
                   regime_context + log_gdppc + peak_size + nonviolent +
                   factor(country) + factor(decade),
                 data = omg_campaign %>% filter(regime_type == "autocracy"))

h1_hyb_m8 <- lm(dem_demand_any ~ student_index +
                  working_group + educated_group +
                  elite_group + identity_group +
                  regime_context + log_gdppc + peak_size + nonviolent +
                  factor(country) + factor(decade),
                data = omg_campaign %>% filter(regime_type == "hybrid"))

h1_dem_m8 <- lm(dem_demand_any ~ student_index +
                  working_group + educated_group +
                  elite_group + identity_group +
                  regime_context + log_gdppc + peak_size + nonviolent +
                  factor(country) + factor(decade),
                data = omg_campaign %>% filter(regime_type == "democracy"))


#clustered SEs:
vcov_h1_auto_m4 <- vcovCL(h1_auto_m4, cluster = ~ country)
vcov_h1_hyb_m4  <- vcovCL(h1_hyb_m4,  cluster = ~ country)
vcov_h1_dem_m4  <- vcovCL(h1_dem_m4,  cluster = ~ country)
vcov_h1_auto_m8 <- vcovCL(h1_auto_m8, cluster = ~ country)
vcov_h1_hyb_m8  <- vcovCL(h1_hyb_m8,  cluster = ~ country)
vcov_h1_dem_m8  <- vcovCL(h1_dem_m8,  cluster = ~ country)


#LEts see:
coeftest(h1_auto_m4, vcov = vcov_h1_auto_m4)
coeftest(h1_hyb_m4,  vcov = vcov_h1_hyb_m4)
coeftest(h1_dem_m4,  vcov = vcov_h1_dem_m4)
coeftest(h1_auto_m8, vcov = vcov_h1_auto_m8)
coeftest(h1_hyb_m8,  vcov = vcov_h1_hyb_m8)
coeftest(h1_dem_m8,  vcov = vcov_h1_dem_m8)


#fixed effects rows:
fe_rows_h1_regime <- data.frame(
  term            = c("Regime type", "Country FE", "Decade FE"),
  "M4 Autocracy"  = c("Autocracy", "Yes", "Yes"),
  "M4 Hybrid"     = c("Hybrid",    "Yes", "Yes"),
  "M4 Democracy"  = c("Democracy", "Yes", "Yes"),
  "M8 Autocracy"  = c("Autocracy", "Yes", "Yes"),
  "M8 Hybrid"     = c("Hybrid",    "Yes", "Yes"),
  "M8 Democracy"  = c("Democracy", "Yes", "Yes"),
  check.names = FALSE
)


#export table:
modelsummary(
  list("M4 Autocracy" = h1_auto_m4, "M4 Hybrid" = h1_hyb_m4, "M4 Democracy" = h1_dem_m4,
       "M8 Autocracy" = h1_auto_m8, "M8 Hybrid" = h1_hyb_m8, "M8 Democracy" = h1_dem_m8),
  vcov      = list(vcov_h1_auto_m4, vcov_h1_hyb_m4, vcov_h1_dem_m4,
                   vcov_h1_auto_m8, vcov_h1_hyb_m8, vcov_h1_dem_m8),
  stars     = TRUE,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_omit = "factor\\(country\\)|factor\\(decade\\)",
  coef_map  = coef_labels_h1,
  gof_map   = c("nobs", "r.squared"),
  add_rows  = fe_rows_h1_regime,
  output    = "/Users/almaowing/Documents/MAthesis/tables/table_h1_regime_robustness.tex",
  title     = "Student-led protests and democratic demands, regime type subgroups (H1)",
  notes     = "Cluster-robust standard errors by country in parentheses. Sample: campaigns split by regime\\_context (V-Dem polyarchy at campaign start): autocracy ($<0.2$), hybrid ($0.2$--$0.5$), democracy ($>0.5$)."
)


#scalebox wrap:
h1_reg_tex <- readLines("/Users/almaowing/Documents/MAthesis/tables/table_h1_regime_robustness.tex")
h1_reg_tex <- gsub("\\\\centering", "\\\\centering\n\\\\scalebox{0.72}{", h1_reg_tex)
h1_reg_tex <- gsub("\\\\end\\{talltblr\\}", "\\\\end{talltblr}\n}", h1_reg_tex)
writeLines(h1_reg_tex, "/Users/almaowing/Documents/MAthesis/tables/table_h1_regime_robustness.tex")



#-------------------------------------------------------------------------------
#second for H2:

#construct regime type variable:
cy_omg <- cy_omg %>%
  mutate(
    regime_type = case_when(
      v2x_polyarchy <  0.2 ~ "autocracy",
      v2x_polyarchy >= 0.2 & v2x_polyarchy <= 0.5 ~ "hybrid",
      v2x_polyarchy >  0.5 ~ "democracy",
      TRUE ~ NA_character_
    )
  )


#M4 spec by regime type (campaign_type):
h2_auto_m4 <- feols(
  electoral_democracy_t1 ~ campaign_type + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1960 & regime_type == "autocracy")
)

h2_hyb_m4 <- feols(
  electoral_democracy_t1 ~ campaign_type + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1960 & regime_type == "hybrid")
)

h2_dem_m4 <- feols(
  electoral_democracy_t1 ~ campaign_type + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1960 & regime_type == "democracy")
)


#M8 spec by regime type (student_index_cy):
h2_auto_m8 <- feols(
  electoral_democracy_t1 ~ student_index_cy + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1960 & regime_type == "autocracy")
)

h2_hyb_m8 <- feols(
  electoral_democracy_t1 ~ student_index_cy + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1960 & regime_type == "hybrid")
)

h2_dem_m8 <- feols(
  electoral_democracy_t1 ~ student_index_cy + v2x_polyarchy +
    log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1960 & regime_type == "democracy")
)


#lets see:
summary(h2_auto_m4)
summary(h2_hyb_m4)
summary(h2_dem_m4)
summary(h2_auto_m8)
summary(h2_hyb_m8)
summary(h2_dem_m8)


#coefficient labels:
coef_labels_regime <- c(
  "campaign_typestudent"        = "Student-led protest",
  "campaign_typeother_campaign" = "Other campaign",
  "student_index_cy"            = "Student involvement index",
  "v2x_polyarchy"               = "Electoral democracy (lagged)",
  "log_gdppc"                   = "GDP per capita (log)",
  "gdp_growth"                  = "GDP growth",
  "log_pop"                     = "Population (log)",
  "e_miurbani"                  = "Urbanization (V-Dem)"
)


#fixed effects rows:
fe_rows_regime <- data.frame(
  term            = c("Regime type", "Period", "Country FE", "Year FE"),
  "M4 Autocracy"  = c("Autocracy", "Post-1960", "Yes", "Yes"),
  "M4 Hybrid"     = c("Hybrid",    "Post-1960", "Yes", "Yes"),
  "M4 Democracy"  = c("Democracy", "Post-1960", "Yes", "Yes"),
  "M8 Autocracy"  = c("Autocracy", "Post-1960", "Yes", "Yes"),
  "M8 Hybrid"     = c("Hybrid",    "Post-1960", "Yes", "Yes"),
  "M8 Democracy"  = c("Democracy", "Post-1960", "Yes", "Yes"),
  check.names = FALSE
)


#export table
modelsummary(
  list("M4 Autocracy" = h2_auto_m4, "M4 Hybrid" = h2_hyb_m4, "M4 Democracy" = h2_dem_m4,
       "M8 Autocracy" = h2_auto_m8, "M8 Hybrid" = h2_hyb_m8, "M8 Democracy" = h2_dem_m8),
  stars     = TRUE,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map  = coef_labels_regime,
  gof_map   = c("nobs", "r.squared"),
  add_rows  = fe_rows_regime,
  output    = "/Users/almaowing/Documents/MAthesis/tables/table_h2_regime_robustness_x.tex",
  title     = "Student-led protests and democratization, regime type subgroups (H2)",
  notes     = "Cluster-robust standard errors by country in parentheses. DV = V-Dem electoral democracy index at t+1. Sample: post-1960 country-years split by lagged V-Dem polyarchy: autocracy ($<0.2$), hybrid ($0.2$--$0.5$), democracy ($>0.5$)."
)


#scalebox wrap:
h2_reg_tex <- readLines("/Users/almaowing/Documents/MAthesis/tables/table_h2_regime_robustness_x.tex")
h2_reg_tex <- gsub("\\\\centering", "\\\\centering\n\\\\scalebox{0.72}{", h2_reg_tex)
h2_reg_tex <- gsub("\\\\end\\{talltblr\\}", "\\\\end{talltblr}\n}", h2_reg_tex)
writeLines(h2_reg_tex, "/Users/almaowing/Documents/MAthesis/tables/table_h2_regime_robustness_x.tex")



#-------------------------------------------------------------------------------
#Conditional-on-democratic claims analysis (H1+H2)

#Expand campaigns over all active years (start_year to end_year)
if (all(c("start_year", "end_year") %in% names(omg))) {
  omg_years <- omg %>%
    filter(!is.na(start_year)) %>%
    mutate(end_year = ifelse(is.na(end_year), start_year, end_year)) %>%
    rowwise() %>%
    mutate(year = list(seq(start_year, end_year))) %>%
    tidyr::unnest(year) %>%
    ungroup() %>%
    select(country_id, year, student_protest, dem_demand_any)
} else {
  omg_years <- omg %>%
    filter(!is.na(start_year)) %>%
    mutate(year = start_year) %>%
    select(country_id, year, student_protest, dem_demand_any)
}


#Construct 4 binary indicators at country-year level
group_claim_cy <- omg_years %>%
  group_by(country_id, year) %>%
  summarize(
    student_with_claim_cy = as.integer(any(student_protest == 1 & dem_demand_any == 1, na.rm = TRUE)),
    student_no_claim_cy   = as.integer(any(student_protest == 1 & dem_demand_any == 0, na.rm = TRUE)),
    other_with_claim_cy   = as.integer(any(student_protest == 0 & dem_demand_any == 1, na.rm = TRUE)),
    other_no_claim_cy     = as.integer(any(student_protest == 0 & dem_demand_any == 0, na.rm = TRUE)),
    .groups = "drop"
  )


#Join into cy_omg anrd fill NAs with 0:
cy_omg <- cy_omg %>%
  select(-any_of(c("student_with_claim_cy", "student_no_claim_cy",
                   "other_with_claim_cy", "other_no_claim_cy",
                   "group_claim"))) %>%
  left_join(group_claim_cy, by = c("country_id", "year")) %>%
  mutate(
    student_with_claim_cy = replace_na(student_with_claim_cy, 0),
    student_no_claim_cy   = replace_na(student_no_claim_cy,   0),
    other_with_claim_cy   = replace_na(other_with_claim_cy,   0),
    other_no_claim_cy     = replace_na(other_no_claim_cy,     0)
  )


#Categorical: priority student_with > student_no > other_with > other_no:
cy_omg <- cy_omg %>%
  mutate(
    group_claim = case_when(
      student_with_claim_cy == 1 ~ "student_with_claim",
      student_no_claim_cy   == 1 ~ "student_no_claim",
      other_with_claim_cy   == 1 ~ "other_with_claim",
      other_no_claim_cy     == 1 ~ "other_no_claim",
      TRUE                       ~ "no_campaign"
    ),
    group_claim = relevel(factor(group_claim), ref = "no_campaign")
  )


#Distribution check:
table(cy_omg$group_claim)


#M4 exclusive (priority categorical):
m4_groupclaim <- feols(
  electoral_democracy_t1 ~ group_claim +
    v2x_polyarchy + log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1960)
)


#M4 marginal (4 dummies simultaneously):
m4_dummies <- feols(
  electoral_democracy_t1 ~ student_with_claim_cy + student_no_claim_cy +
    other_with_claim_cy + other_no_claim_cy +
    v2x_polyarchy + log_gdppc + gdp_growth + log_pop + e_miurbani
  | country_id + year,
  cluster = ~ country_id,
  data = cy_omg %>% filter(!is.na(electoral_democracy_t1) & year >= 1960)
)


#Lets see:
summary(m4_groupclaim)
summary(m4_dummies)


#Wald tests:
linearHypothesis(m4_groupclaim,
                 "group_claimstudent_with_claim - group_claimstudent_no_claim = 0")

linearHypothesis(m4_groupclaim,
                 "group_claimstudent_with_claim - group_claimother_with_claim = 0")


#Coefficient labels:
coef_labels_combined <- c(
  # exclusive (m4_groupclaim) — coefficients have prefix "group_claim"
  "group_claimstudent_with_claim" = "Student-led, with democratic claim",
  "group_claimstudent_no_claim"   = "Student-led, no democratic claim",
  "group_claimother_with_claim"   = "Other campaign, with democratic claim",
  "group_claimother_no_claim"     = "Other campaign, no democratic claim",
  # marginal (m4_dummies) — same labels, but no prefix
  "student_with_claim_cy"         = "Student-led, with democratic claim",
  "student_no_claim_cy"           = "Student-led, no democratic claim",
  "other_with_claim_cy"           = "Other campaign, with democratic claim",
  "other_no_claim_cy"             = "Other campaign, no democratic claim",
  # shared controls
  "v2x_polyarchy"                 = "Electoral democracy (lagged)",
  "log_gdppc"                     = "GDP per capita (log)",
  "gdp_growth"                    = "GDP growth",
  "log_pop"                       = "Population (log)",
  "e_miurbani"                    = "Urbanization (V-Dem)"
)


#Fixed effects rows:
fe_rows_combined <- data.frame(
  term            = c("Specification", "Period", "Country FE", "Year FE"),
  "Exclusive"     = c("Priority categorical", "Post-1960", "Yes", "Yes"),
  "Marginal"      = c("Simultaneous dummies", "Post-1960", "Yes", "Yes")
)


#Export combined table:
modelsummary(
  list("Exclusive" = m4_groupclaim, "Marginal" = m4_dummies),
  stars     = TRUE,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map  = coef_labels_combined,
  gof_map   = c("nobs", "r.squared"),
  add_rows  = fe_rows_combined,
  output    = "/Users/almaowing/Documents/MAthesis/tables/table_h2_groupclaim_combined_1.tex",
  title     = "Student-led protests and democratization, conditional on democratic claims (H2 robustness)",
  notes     = "Cluster-robust standard errors by country in parentheses. DV = V-Dem electoral democracy index at t+1. Post-1960 sample. Reference: country-years with no campaign. The exclusive specification classifies each country-year by the strongest theoretically relevant campaign type (priority order: student\\_with\\_claim > student\\_no\\_claim > other\\_with\\_claim > other\\_no\\_claim). The marginal specification includes the four campaign-type indicators simultaneously, allowing types to co-occur."
)


#scalebox wrap:
h2_comb_tex <- readLines("/Users/almaowing/Documents/MAthesis/tables/table_h2_groupclaim_combined_1.tex")
h2_comb_tex <- gsub("\\\\centering", "\\\\centering\n\\\\scalebox{0.82}{", h2_comb_tex)
h2_comb_tex <- gsub("\\\\end\\{talltblr\\}", "\\\\end{talltblr}\n}", h2_comb_tex)
writeLines(h2_comb_tex, "/Users/almaowing/Documents/MAthesis/tables/table_h2_groupclaim_combined_1.tex")





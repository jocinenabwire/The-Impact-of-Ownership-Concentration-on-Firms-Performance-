############################################################
# THE IMPACT OF OWNERSHIP CONCENTRATION ON FIRM PERFORMANCE
# Focused empirical script
#
# Research question:
# Does ownership concentration affect firm performance?
# Are monitoring, incentives, and private benefits of control
# relevant mechanisms?
#
# Main regressions:
# 1. Baseline model
# 2. Mechanism model with PBC intensity
# 3. Robustness model with any PBC signal
#
# Output folder:
# results2
############################################################


############################################################
# 0. Clear environment
############################################################

rm(list = ls())


############################################################
# 1. Load packages
############################################################

packages <- c(
  "tidyverse",
  "haven",
  "here",
  "janitor",
  "labelled",
  "fixest",
  "modelsummary",
  "writexl",
  "scales",
  "car"
)

installed_packages <- rownames(installed.packages())

for (p in packages) {
  if (!(p %in% installed_packages)) {
    install.packages(p)
  }
}

library(tidyverse)
library(haven)
library(here)
library(janitor)
library(labelled)
library(fixest)
library(modelsummary)
library(writexl)
library(scales)
library(car)


############################################################
# 2. Project folders and data import
############################################################

# The dataset should be inside data_raw.
# This script assumes that the correct RStudio project is open.

print(here::here())

dir.create(
  here::here("results2"),
  showWarnings = FALSE
)


raw_data <- read_dta('edata.dta') %>%
  clean_names()


############################################################
# 3. Helper functions
############################################################

# WBES datasets often use negative values as special missing codes.
# This function removes labels and replaces negative values with NA.

clean_negative_codes <- function(x) {
  x <- as.numeric(haven::zap_labels(x))
  ifelse(x < 0, NA_real_, x)
}

# Winsorise continuous variables at the 1st and 99th percentiles.

winsorise <- function(x, probs = c(0.01, 0.99)) {
  q <- quantile(x, probs = probs, na.rm = TRUE)
  x <- ifelse(x < q[1], q[1], x)
  x <- ifelse(x > q[2], q[2], x)
  return(x)
}

# Convert WBES Yes/No variables:
# 1 = Yes
# 2 = No

make_yes_no_dummy <- function(x) {
  x <- clean_negative_codes(x)
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 0,
    TRUE ~ NA_real_
  )
}


############################################################
# 4. Export simple codebook
############################################################

codebook <- tibble(
  variable_name = names(raw_data),
  
  variable_type = sapply(raw_data, function(x) {
    paste(class(x), collapse = ", ")
  }),
  
  variable_label = sapply(raw_data, function(x) {
    lab <- labelled::var_label(x)
    if (is.null(lab)) NA_character_ else as.character(lab)
  }),
  
  missing_values = sapply(raw_data, function(x) {
    sum(is.na(x))
  })
)

write_csv(
  codebook,
  here::here("results2", "codebook_variable_names_types_labels.csv")
)

write_xlsx(
  codebook,
  here::here("results2", "codebook_variable_names_types_labels.xlsx")
)


############################################################
# 5. Construct variables
############################################################

data <- raw_data %>%
  mutate(
    
    ########################################################
    # 5.1 Identifiers and fixed effects
    ########################################################
    
    firm_id = if ("idstd" %in% names(raw_data)) idstd else row_number(),
    
    country_id = as.factor(country),
    
    survey_year = if ("a14y" %in% names(raw_data)) {
      clean_negative_codes(a14y)
    } else if ("a20y" %in% names(raw_data)) {
      clean_negative_codes(a20y)
    } else {
      NA_real_
    },
    
    industry = if ("isic_v4" %in% names(raw_data)) {
      clean_negative_codes(isic_v4)
    } else {
      NA_real_
    },
    
    industry_fe = as.factor(industry),
    
    sample_final = if ("sample" %in% names(raw_data)) {
      clean_negative_codes(sample)
    } else {
      1
    },
    
    weight = if ("wt" %in% names(raw_data)) {
      clean_negative_codes(wt)
    } else {
      NA_real_
    },
    
    
    ########################################################
    # 5.2 Dependent variable: firm performance
    ########################################################
    
    sales = clean_negative_codes(d2),
    sales = ifelse(sales <= 0, NA_real_, sales),
    sales = winsorise(sales),
    
    log_sales = log(sales),
    
    
    ########################################################
    # 5.3 Main explanatory variable: ownership concentration
    ########################################################
    
    ownership = clean_negative_codes(b3),
    
    ownership = ifelse(
      ownership < 0 | ownership > 100,
      NA_real_,
      ownership
    ),
    
    # Ownership in 10 percentage points.
    # One unit = 10 percentage-point increase in ownership.
    
    ownership_10 = ownership / 10,
    
    
    ########################################################
    # 5.4 Controls
    ########################################################
    
    employees = if ("size_num" %in% names(raw_data)) {
      clean_negative_codes(size_num)
    } else if ("l1" %in% names(raw_data)) {
      clean_negative_codes(l1)
    } else {
      NA_real_
    },
    
    employees = ifelse(employees <= 0, NA_real_, employees),
    employees = winsorise(employees),
    
    log_employees = log(employees),
    
    year_started = if ("b5" %in% names(raw_data)) {
      clean_negative_codes(b5)
    } else if ("b6b" %in% names(raw_data)) {
      clean_negative_codes(b6b)
    } else {
      NA_real_
    },
    
    firm_age = survey_year - year_started,
    
    firm_age = ifelse(
      firm_age < 0 | firm_age > 150,
      NA_real_,
      firm_age
    ),
    
    firm_age = winsorise(firm_age),
    
    foreign_ownership = if ("b2b" %in% names(raw_data)) {
      clean_negative_codes(b2b)
    } else {
      NA_real_
    },
    
    foreign_ownership = ifelse(
      foreign_ownership < 0 | foreign_ownership > 100,
      NA_real_,
      foreign_ownership
    ),
    
    export_share = if ("d3c" %in% names(raw_data)) {
      clean_negative_codes(d3c)
    } else {
      NA_real_
    },
    
    export_share = ifelse(
      export_share < 0 | export_share > 100,
      NA_real_,
      export_share
    ),
    
    exporter = case_when(
      !is.na(export_share) & export_share > 0 ~ 1,
      !is.na(export_share) & export_share == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    
    
    ########################################################
    # 5.5 Mechanisms: monitoring and incentives
    ########################################################
    
    # r2 is used as a monitoring proxy.
    
    monitoring = if ("r2" %in% names(raw_data)) {
      make_yes_no_dummy(r2)
    } else {
      NA_real_
    },
    
    # r8 is used as an incentive proxy.
    
    incentives = if ("r8" %in% names(raw_data)) {
      make_yes_no_dummy(r8)
    } else {
      NA_real_
    },
    
    
    ########################################################
    # 5.6 PBC construction using only WBES variables
    ########################################################
    
    # The theory of PBC is about the possibility that controlling
    # shareholders extract private benefits or hide resource diversion.
    #
    # WBES does not have market-based control premia.
    # Therefore, we construct a proxy using two observable signals:
    #
    # 1. Low reliability / opacity of financial figures: a17
    # 2. Informal payments as a percentage of sales: j7a
    
    a17_num = if ("a17" %in% names(raw_data)) {
      clean_negative_codes(a17)
    } else {
      NA_real_
    },
    
    a17_label = if ("a17" %in% names(raw_data)) {
      tolower(as.character(labelled::to_factor(raw_data$a17, levels = "labels")))
    } else {
      NA_character_
    },
    
    low_data_reliability = case_when(
      str_detect(a17_label, "arbitrary|unreliable|not reliable|guess") ~ 1,
      str_detect(a17_label, "records|written|precision|precise|exact") ~ 0,
      a17_num == 3 ~ 1,
      a17_num %in% c(1, 2) ~ 0,
      TRUE ~ NA_real_
    ),
    
    informal_payments = if ("j7a" %in% names(raw_data)) {
      clean_negative_codes(j7a)
    } else {
      NA_real_
    },
    
    informal_payments = ifelse(
      informal_payments < 0 | informal_payments > 100,
      NA_real_,
      informal_payments
    ),
    
    informal_payments = winsorise(informal_payments),
    
    informal_payments_dummy = case_when(
      !is.na(informal_payments) & informal_payments > 0 ~ 1,
      !is.na(informal_payments) & informal_payments == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Main PBC intensity proxy:
    #
    # 0   = no PBC signal
    # 0.5 = one PBC signal
    # 1   = two PBC signals
    #
    # This is the main mechanism variable because it preserves
    # the intensity of the WBES-based PBC proxy.
    
    pbc_intensity = rowMeans(
      cbind(low_data_reliability, informal_payments_dummy),
      na.rm = FALSE
    ),
    
    # Robustness PBC variable:
    # 1 = at least one PBC signal
    # 0 = no PBC signal
    
    pbc_any = case_when(
      !is.na(pbc_intensity) & pbc_intensity > 0 ~ 1,
      !is.na(pbc_intensity) & pbc_intensity == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # PBC category for graphs.
    
    pbc_category = case_when(
      pbc_intensity == 0 ~ "No PBC signal",
      pbc_intensity == 0.5 ~ "One PBC signal",
      pbc_intensity == 1 ~ "Two PBC signals",
      TRUE ~ NA_character_
    ),
    
    pbc_category = factor(
      pbc_category,
      levels = c(
        "No PBC signal",
        "One PBC signal",
        "Two PBC signals"
      )
    )
  )


############################################################
# 6. Final estimation sample
############################################################

analysis_data <- data %>%
  filter(
    sample_final == 1,
    !is.na(log_sales),
    !is.na(ownership_10),
    !is.na(log_employees),
    !is.na(firm_age),
    !is.na(foreign_ownership),
    !is.na(exporter),
    !is.na(monitoring),
    !is.na(incentives),
    !is.na(pbc_intensity),
    !is.na(pbc_any),
    !is.na(country_id),
    !is.na(survey_year),
    !is.na(industry_fe)
  )


############################################################
# 7. Center variables to reduce multicollinearity
############################################################

# Interactions often generate high VIF because the interaction term
# is mechanically correlated with its components.
#
# Mean-centering does not change the substantive model.
# It reduces mechanical multicollinearity and improves interpretation.

analysis_data <- analysis_data %>%
  mutate(
    ownership_10_c = ownership_10 - mean(ownership_10, na.rm = TRUE),
    
    foreign_ownership_c =
      foreign_ownership - mean(foreign_ownership, na.rm = TRUE),
    
    pbc_intensity_c =
      pbc_intensity - mean(pbc_intensity, na.rm = TRUE),
    
    pbc_any_c =
      pbc_any - mean(pbc_any, na.rm = TRUE),
    
    ownership_pbc_intensity_c =
      ownership_10_c * pbc_intensity_c,
    
    ownership_pbc_any_c =
      ownership_10_c * pbc_any_c
  )


############################################################
# 8. Sample summary
############################################################

sample_summary <- tibble(
  sample = c(
    "Raw data",
    "Final estimation sample"
  ),
  observations = c(
    nrow(raw_data),
    nrow(analysis_data)
  )
)

write_csv(
  sample_summary,
  here::here("results2", "sample_summary.csv")
)


############################################################
# 9. Clean descriptive statistics
############################################################

clean_descriptive_table <- analysis_data %>%
  summarise(
    `Log sales: mean` = mean(log_sales, na.rm = TRUE),
    `Log sales: SD` = sd(log_sales, na.rm = TRUE),
    
    `Ownership concentration (%): mean` = mean(ownership, na.rm = TRUE),
    `Ownership concentration (%): median` = median(ownership, na.rm = TRUE),
    `Ownership concentration = 100%` = mean(ownership == 100, na.rm = TRUE),
    
    `Log employees: mean` = mean(log_employees, na.rm = TRUE),
    `Firm age: mean` = mean(firm_age, na.rm = TRUE),
    `Foreign ownership (%): mean` = mean(foreign_ownership, na.rm = TRUE),
    `Exporter share` = mean(exporter, na.rm = TRUE),
    
    `Monitoring share` = mean(monitoring, na.rm = TRUE),
    `Incentives share` = mean(incentives, na.rm = TRUE),
    
    `Low data reliability share` = mean(low_data_reliability, na.rm = TRUE),
    `Informal payments share` = mean(informal_payments_dummy, na.rm = TRUE),
    `Any PBC signal share` = mean(pbc_any, na.rm = TRUE),
    `PBC intensity: mean` = mean(pbc_intensity, na.rm = TRUE),
    
    `Observations` = n()
  ) %>%
  pivot_longer(
    everything(),
    names_to = "Statistic",
    values_to = "Value"
  ) %>%
  mutate(
    Value = round(Value, 3)
  )

write_csv(
  clean_descriptive_table,
  here::here("results2", "clean_descriptive_statistics.csv")
)

modelsummary::datasummary_df(
  clean_descriptive_table,
  output = here::here("results2", "clean_descriptive_statistics.docx")
)


############################################################
# 10. Diagnostics: exact values and VIF
############################################################

# 10.1 Ownership exact values

ownership_exact_values <- analysis_data %>%
  count(ownership, sort = TRUE) %>%
  mutate(share = n / sum(n))

write_csv(
  ownership_exact_values,
  here::here("results2", "diagnostic_ownership_exact_values.csv")
)

# 10.2 PBC exact values

pbc_exact_values <- analysis_data %>%
  count(pbc_intensity, sort = TRUE) %>%
  mutate(share = n / sum(n))

write_csv(
  pbc_exact_values,
  here::here("results2", "diagnostic_pbc_exact_values.csv")
)

# 10.3 Correlation matrix

correlation_data <- analysis_data %>%
  select(
    ownership_10_c,
    log_employees,
    firm_age,
    foreign_ownership_c,
    exporter,
    monitoring,
    incentives,
    pbc_intensity_c,
    ownership_pbc_intensity_c,
    pbc_any_c,
    ownership_pbc_any_c
  ) %>%
  drop_na()

correlation_matrix <- cor(correlation_data)

write_csv(
  as.data.frame(correlation_matrix) %>%
    rownames_to_column("variable"),
  here::here("results2", "diagnostic_correlation_matrix.csv")
)

# 10.4 VIF for mechanism model

vif_model_mechanism <- lm(
  log_sales ~ ownership_10_c +
    log_employees + firm_age + foreign_ownership_c + exporter +
    monitoring + incentives + pbc_intensity_c +
    ownership_pbc_intensity_c,
  data = analysis_data
)

vif_results_mechanism <- car::vif(vif_model_mechanism)

capture.output(
  vif_results_mechanism,
  file = here::here("results2", "diagnostic_vif_mechanism_model.txt")
)

# 10.5 VIF for robustness model

vif_model_robustness <- lm(
  log_sales ~ ownership_10_c +
    log_employees + firm_age + foreign_ownership_c + exporter +
    monitoring + incentives + pbc_any_c +
    ownership_pbc_any_c,
  data = analysis_data
)

vif_results_robustness <- car::vif(vif_model_robustness)

capture.output(
  vif_results_robustness,
  file = here::here("results2", "diagnostic_vif_robustness_model.txt")
)


############################################################
# 11. Improved graphs with colors
############################################################

analysis_data <- analysis_data %>%
  mutate(
    ownership_group = case_when(
      ownership < 50 ~ "Below 50%",
      ownership >= 50 & ownership < 75 ~ "50-74%",
      ownership >= 75 & ownership < 100 ~ "75-99%",
      ownership == 100 ~ "100%",
      TRUE ~ NA_character_
    ),
    
    ownership_group = factor(
      ownership_group,
      levels = c("Below 50%", "50-74%", "75-99%", "100%")
    )
  )

# Color palettes
ownership_colors <- c(
  "Below 50%" = "#9ecae1",
  "50-74%" = "#6baed6",
  "75-99%" = "#3182bd",
  "100%" = "#08519c"
)

pbc_colors <- c(
  "No PBC signal" = "#74c476",
  "One PBC signal" = "#fd8d3c",
  "Two PBC signals" = "#de2d26"
)
  

############################################################
# 11.1 Ownership concentration by groups
############################################################

fig_ownership_grouped <- analysis_data %>%
  count(ownership_group) %>%
  mutate(
    share = n / sum(n),
    label = paste0(round(share * 100, 1), "%")
  ) %>%
  ggplot(
    aes(x = ownership_group, y = share, fill = ownership_group)
  ) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(
    aes(label = label),
    vjust = -0.4,
    size = 4
  ) +
  scale_fill_manual(values = ownership_colors) +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, NA)
  ) +
  labs(
    title = "Ownership concentration is highly concentrated",
    subtitle = "Share of firms by ownership concentration group",
    x = "Ownership concentration group",
    y = "Share of firms"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  here::here("results2", "fig_01_ownership_grouped.png"),
  fig_ownership_grouped,
  width = 7,
  height = 4.5,
  dpi = 300
)


############################################################
# 11.2 Most common ownership values
############################################################

fig_top_ownership_values <- ownership_exact_values %>%
  slice_head(n = 15) %>%
  mutate(
    ownership_label = paste0(ownership, "%"),
    ownership_label = reorder(ownership_label, n)
  ) %>%
  ggplot(
    aes(x = ownership_label, y = n, fill = n)
  ) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_gradient(low = "#c6dbef", high = "#08519c") +
  labs(
    title = "Most common ownership concentration values",
    subtitle = "The data show strong heaping at round ownership values",
    x = "Ownership concentration",
    y = "Number of firms"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  here::here("results2", "fig_02_top_ownership_values.png"),
  fig_top_ownership_values,
  width = 7,
  height = 5,
  dpi = 300
)
############################################################
# 11.3 PBC distribution by category
############################################################

fig_pbc_grouped <- analysis_data %>%
  count(pbc_category) %>%
  mutate(
    share = n / sum(n),
    label = paste0(round(share * 100, 1), "%")
  ) %>%
  ggplot(
    aes(x = pbc_category, y = share, fill = pbc_category)
  ) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(
    aes(label = label),
    vjust = -0.4,
    size = 4
  ) +
  scale_fill_manual(values = pbc_colors) +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, NA)
  ) +
  labs(
    title = "Most firms show no PBC signal",
    subtitle = "PBC proxy combines low data reliability and informal payments",
    x = "PBC category",
    y = "Share of firms"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  here::here("results2", "fig_03_pbc_grouped.png"),
  fig_pbc_grouped,
  width = 7,
  height = 4.5,
  dpi = 300
)


############################################################
# 11.4 Firm performance by ownership group
############################################################

fig_sales_ownership_box <- ggplot(
  analysis_data,
  aes(x = ownership_group, y = log_sales, fill = ownership_group)
) +
  geom_boxplot(
    outlier.alpha = 0.12,
    color = "gray30",
    show.legend = FALSE
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 2.8,
    color = "black"
  ) +
  scale_fill_manual(values = ownership_colors) +
  labs(
    title = "Firm performance by ownership concentration group",
    subtitle = "Black dots show group means; boxes show the distribution of log sales",
    x = "Ownership concentration group",
    y = "Log sales"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  here::here("results2", "fig_04_sales_by_ownership_group.png"),
  fig_sales_ownership_box,
  width = 7,
  height = 4.5,
  dpi = 300
)

############################################################
# 11.5 Mean performance by ownership group and PBC category
############################################################

fig_sales_ownership_pbc <- analysis_data %>%
  group_by(ownership_group, pbc_category) %>%
  summarise(
    mean_log_sales = mean(log_sales, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 30) %>%
  ggplot(
    aes(
      x = ownership_group,
      y = mean_log_sales,
      group = pbc_category,
      color = pbc_category
    )
  ) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  scale_color_manual(values = pbc_colors) +
  labs(
    title = "Ownership, PBC and average firm performance",
    subtitle = "Mean log sales by ownership and PBC category; cells with n < 30 excluded",
    x = "Ownership concentration group",
    y = "Mean log sales",
    color = "PBC category"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  here::here("results2", "fig_05_sales_ownership_pbc_grouped.png"),
  fig_sales_ownership_pbc,
  width = 7.5,
  height = 4.8,
  dpi = 300
)
############################################################
# 12. Regression models
############################################################

# Model 1: Baseline regression
# This estimates the total association between ownership
# concentration and firm performance.

reg_baseline <- feols(
  log_sales ~ ownership_10_c +
    log_employees + firm_age + foreign_ownership_c + exporter |
    country_id + survey_year + industry_fe,
  data = analysis_data,
  cluster = ~ country_id
)


# Model 2: Main mechanism regression
# This uses PBC intensity as the main PBC measure.
#
# PBC intensity takes values:
# 0   = no PBC signal
# 0.5 = one PBC signal
# 1   = two PBC signals
#
# The interaction tests whether ownership concentration is less/more
# beneficial when PBC intensity is higher.

reg_mechanism <- feols(
  log_sales ~ ownership_10_c +
    log_employees + firm_age + foreign_ownership_c + exporter +
    monitoring + incentives +
    pbc_intensity_c + ownership_pbc_intensity_c |
    country_id + survey_year + industry_fe,
  data = analysis_data,
  cluster = ~ country_id
)


# Model 3: Robustness regression
# This uses a binary PBC variable:
# 1 = at least one PBC signal
# 0 = no PBC signal

reg_robustness <- feols(
  log_sales ~ ownership_10_c +
    log_employees + firm_age + foreign_ownership_c + exporter +
    monitoring + incentives +
    pbc_any_c + ownership_pbc_any_c |
    country_id + survey_year + industry_fe,
  data = analysis_data,
  cluster = ~ country_id
)

regression_models <- list(
  "Baseline" = reg_baseline,
  "Mechanism: PBC intensity" = reg_mechanism,
  "Robustness: any PBC signal" = reg_robustness
)


############################################################
# 13. Export regression table
############################################################

coef_labels <- c(
  "ownership_10_c" = "Ownership concentration",
  "log_employees" = "Log employees",
  "firm_age" = "Firm age",
  "foreign_ownership_c" = "Foreign ownership",
  "exporter" = "Exporter",
  "monitoring" = "Monitoring",
  "incentives" = "Incentives",
  "pbc_intensity_c" = "PBC intensity",
  "ownership_pbc_intensity_c" = "Ownership x PBC intensity",
  "pbc_any_c" = "Any PBC signal",
  "ownership_pbc_any_c" = "Ownership x Any PBC signal"
)

modelsummary(
  regression_models,
  output = here::here("results2", "main_regression_table.docx"),
  coef_map = coef_labels,
  stars = TRUE,
  gof_omit = "IC|Log|RMSE",
  notes = c(
    "Dependent variable: log sales.",
    "Ownership is measured in 10 percentage points and mean-centered in the regressions.",
    "The main PBC measure is PBC intensity, constructed from low reliability of reported figures and informal payments.",
    "PBC intensity takes values 0, 0.5 and 1.",
    "The robustness model uses a binary indicator equal to one if the firm reports at least one PBC signal.",
    "All regressions include country, survey-year, and industry fixed effects.",
    "Standard errors are clustered at the country level.",
    "Results are interpreted as conditional associations, not causal estimates."
  )
)

modelsummary(
  regression_models,
  output = here::here("results2", "main_regression_table.html"),
  coef_map = coef_labels,
  stars = TRUE,
  gof_omit = "IC|Log|RMSE"
)


############################################################
# 14. Save estimation dataset
############################################################

write_csv(
  analysis_data,
  here::here("results2", "analysis_dataset_used_in_regressions.csv")
)


############################################################
# 15. Interpretation notes
############################################################

interpretation_notes <- paste0(
  "This script estimates three focused regressions.\n\n",
  
  "Model 1 is the baseline specification. It estimates the association between ownership concentration and firm performance, controlling for firm size, firm age, foreign ownership, exporter status, country fixed effects, survey-year fixed effects, and industry fixed effects.\n\n",
  
  "Model 2 is the main mechanism specification. It adds monitoring, incentives, PBC intensity, and the interaction between ownership concentration and PBC intensity. PBC intensity takes values 0, 0.5 and 1, depending on whether the firm reports no PBC signal, one PBC signal, or two PBC signals.\n\n",
  
  "Model 3 is a robustness specification. It uses a binary PBC indicator equal to one when the firm reports at least one PBC signal: low reliability of reported financial figures or informal payments.\n\n",
  
  "Tobit is not used because the dependent variable is log sales, which is continuous. PBC is a discrete explanatory variable, not a censored dependent variable. Therefore, fixed-effects OLS is appropriate for the main specification.\n\n",
  
  "Ownership and PBC variables are mean-centered before constructing interaction terms. This reduces mechanical multicollinearity between the interaction term and its components and improves interpretation.\n\n",
  
  "Corruption and ownership-by-corruption interactions are excluded from the main specification because corruption is a broad business-environment perception and is not the preferred theoretical measure of private benefits of control in this dataset.\n\n",
  
  "The interaction between ownership and PBC is central. If the coefficient is negative, this suggests that ownership concentration is less beneficial, or more harmful, when private benefits of control are more likely.\n\n",
  
  "The results should be interpreted as conditional associations, not causal effects, because ownership concentration and governance mechanisms are not randomly assigned.\n"
)

writeLines(
  interpretation_notes,
  here::here("results2", "interpretation_notes.txt")
)


############################################################
# END
############################################################

message("Focused updated script completed successfully. Check the results2 folder.")
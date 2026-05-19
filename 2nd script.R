############################################################
# THE IMPACT OF OWNERSHIP CONCENTRATION ON FIRM PERFORMANCE
# Empirical Corporate Governance Project
#
# Research question:
# Does ownership concentration improve firm performance?
# Is this relationship associated with monitoring, incentives,
# and private benefits of control (PBC)?
#
# Data:
# World Bank Enterprise Surveys standardized dataset
#
# Main outcome:
# Log annual sales
#
# Main ownership variable:
# b3 = ownership concentration / share owned by largest owner(s)
#
# Main PBC measure:
# Constructed only from WBES variables:
#   1. a17 = reliability / quality of reported financial figures
#   2. j7a = informal payments as percentage of annual sales
#
# Preferred PBC regression measure:
# Leave-one-out country-year-industry average of firm-level PBC
#
# Results folder:
# results2
############################################################


############################################################
# 0. Clear environment
############################################################

rm(list = ls())


############################################################
# 1. Load packages
############################################################

# List of packages needed for the whole script
packages <- c(
  "tidyverse",
  "haven",
  "here",
  "janitor",
  "labelled",
  "fixest",
  "modelsummary",
  "broom",
  "car",
  "scales",
  "writexl",
  "officer",
  "flextable"
)

# Install packages that are not yet installed
installed_packages <- rownames(installed.packages())

for (p in packages) {
  if (!(p %in% installed_packages)) {
    install.packages(p)
  }
}

# Load packages
library(tidyverse)
library(haven)
library(here)
library(janitor)
library(labelled)
library(fixest)
library(modelsummary)
library(broom)
library(car)
library(scales)
library(writexl)
library(officer)
library(flextable)


############################################################
# 2. Set project structure and import data
############################################################

# This script assumes that you opened the correct RStudio project.
# The project should have this structure:
#
# Project folder
# ├── data_raw
# │   └── New_Comprehensive_April_01_2026.dta
# ├── results2
# └── script.R

# Print current project folder
print(here::here())

# Create results folder if it does not exist
dir.create(
  here::here("results2"),
  showWarnings = FALSE
)


# Import Stata dataset and clean variable names
raw_data <- read_dta("edata.dta") %>%
  clean_names()


############################################################
# 3. Export codebook from imported data
############################################################

# This creates a codebook with variable name, type, label,
# number of missing values and number of non-missing values.

codebook_imported <- tibble(
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
  }),
  
  non_missing_values = sapply(raw_data, function(x) {
    sum(!is.na(x))
  })
)

write_csv(
  codebook_imported,
  here::here("results2", "codebook_imported_variables.csv")
)

write_xlsx(
  codebook_imported,
  here::here("results2", "codebook_imported_variables.xlsx")
)


############################################################
# 4. Helper functions
############################################################

# WBES datasets usually use negative values for special missing codes.
# Examples: -9 = don't know, -8 = refused, etc.
# This function removes labels and converts all negative values to NA.

clean_negative_codes <- function(x) {
  x <- as.numeric(haven::zap_labels(x))
  ifelse(x < 0, NA_real_, x)
}

# Winsorisation reduces the influence of extreme outliers.
# Here we cap variables at the 1st and 99th percentiles.

winsorise <- function(x, probs = c(0.01, 0.99)) {
  q <- quantile(x, probs = probs, na.rm = TRUE)
  x <- ifelse(x < q[1], q[1], x)
  x <- ifelse(x > q[2], q[2], x)
  return(x)
}

# WBES yes/no variables are usually coded:
# 1 = Yes
# 2 = No
# This function converts them into:
# 1 = Yes
# 0 = No

make_yes_no_dummy <- function(x) {
  x <- clean_negative_codes(x)
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 0,
    TRUE ~ NA_real_
  )
}


############################################################
# 5. Inspect key variables and value labels
############################################################

# These are the main variables expected to be used in the project.
# The script checks which of them exist in the dataset.

key_variables <- c(
  "idstd",
  "country",
  "region",
  "sample",
  "wt",
  "stra_sector",
  "sector_ms",
  "size",
  "size_num",
  "isic_v4",
  "a14y",
  "a17",
  "b3",
  "b5",
  "b6b",
  "b7",
  "d2",
  "d3c",
  "r2",
  "r8",
  "j7a",
  "j7b",
  "j30f",
  "l1",
  "h1",
  "h5",
  "f1",
  "e2",
  "e11"
)

key_variables <- key_variables[key_variables %in% names(raw_data)]

# Export key-variable codebook
key_codebook <- codebook_imported %>%
  filter(variable_name %in% key_variables)

write_csv(
  key_codebook,
  here::here("results2", "key_variables_codebook.csv")
)

# Export value labels for key variables.
# This helps verify how each variable is coded.

capture.output(
  {
    cat("VALUE LABELS FOR KEY VARIABLES\n\n")
    
    for (v in key_variables) {
      cat("\n----------------------------------------\n")
      cat("Variable:", v, "\n\n")
      
      cat("Variable label:\n")
      print(labelled::var_label(raw_data[[v]]))
      
      cat("\nValue labels:\n")
      print(labelled::val_labels(raw_data[[v]]))
    }
  },
  file = here::here("results2", "key_variables_value_labels.txt")
)


############################################################
# 6. Construct main variables
############################################################

data <- raw_data %>%
  mutate(
    
    ########################################################
    # 6.1 Firm identifiers and survey structure
    ########################################################
    
    firm_id = if ("idstd" %in% names(raw_data)) {
      idstd
    } else {
      row_number()
    },
    
    country_id = as.factor(country),
    
    region_id = if ("region" %in% names(raw_data)) {
      as.factor(clean_negative_codes(region))
    } else {
      NA
    },
    
    # a14y is usually the survey year in the standardized WBES data.
    # If a14y does not exist, the script tries a20y.
    
    survey_year = if ("a14y" %in% names(raw_data)) {
      clean_negative_codes(a14y)
    } else if ("a20y" %in% names(raw_data)) {
      clean_negative_codes(a20y)
    } else {
      NA_real_
    },
    
    country_year = interaction(country_id, survey_year, drop = TRUE),
    
    weight = if ("wt" %in% names(raw_data)) {
      clean_negative_codes(wt)
    } else {
      NA_real_
    },
    
    # sample == 1 is usually the latest standardized survey sample.
    # If sample does not exist, all observations are kept.
    
    latest_survey_sample = if ("sample" %in% names(raw_data)) {
      clean_negative_codes(sample)
    } else {
      1
    },
    
    
    ########################################################
    # 6.2 Sector and industry variables
    ########################################################
    
    sector_strata = if ("stra_sector" %in% names(raw_data)) {
      as.factor(stra_sector)
    } else {
      NA
    },
    
    sector_main = if ("sector_ms" %in% names(raw_data)) {
      as.factor(sector_ms)
    } else {
      NA
    },
    
    industry_isic = if ("isic_v4" %in% names(raw_data)) {
      clean_negative_codes(isic_v4)
    } else {
      NA_real_
    },
    
    industry_fe = as.factor(industry_isic),
    
    
    ########################################################
    # 6.3 Ownership concentration
    ########################################################
    
    # b3 is the main ownership concentration variable.
    # It should measure the share owned by the largest owner(s), in percent.
    
    ownership = clean_negative_codes(b3),
    
    # Keep only valid percentages.
    
    ownership = ifelse(
      ownership < 0 | ownership > 100,
      NA_real_,
      ownership
    ),
    
    # Scale ownership in 10 percentage points.
    # This makes coefficients easier to interpret.
    # Example: coefficient on ownership_10 means the effect of a 10 p.p.
    # increase in ownership concentration.
    
    ownership_10 = ownership / 10,
    
    # Squared term for non-linear specification.
    
    ownership_sq = ownership_10^2,
    
    # High ownership dummy based on sample median.
    
    high_ownership = case_when(
      !is.na(ownership) &
        ownership >= median(ownership, na.rm = TRUE) ~ 1,
      !is.na(ownership) &
        ownership < median(ownership, na.rm = TRUE) ~ 0,
      TRUE ~ NA_real_
    ),
    
    
    ########################################################
    # 6.4 Firm performance: sales
    ########################################################
    
    # d2 is annual sales.
    # We require positive sales before taking logs.
    
    sales_total = clean_negative_codes(d2),
    
    sales_total = ifelse(
      sales_total <= 0,
      NA_real_,
      sales_total
    ),
    
    sales_total = winsorise(sales_total),
    
    ln_sales = log(sales_total),
    
    
    ########################################################
    # 6.5 Firm scale
    ########################################################
    
    # The preferred scale control is size_num.
    # We do NOT use size categories as controls in the regressions.
    # This avoids mechanically controlling away too much firm variation.
    
    employees = if ("size_num" %in% names(raw_data)) {
      clean_negative_codes(size_num)
    } else if ("l1" %in% names(raw_data)) {
      clean_negative_codes(l1)
    } else {
      NA_real_
    },
    
    employees = ifelse(
      employees <= 0,
      NA_real_,
      employees
    ),
    
    employees = winsorise(employees),
    
    ln_employees = log(employees),
    
    sales_per_worker = sales_total / employees,
    
    sales_per_worker = ifelse(
      sales_per_worker <= 0,
      NA_real_,
      sales_per_worker
    ),
    
    sales_per_worker = winsorise(sales_per_worker),
    
    ln_sales_per_worker = log(sales_per_worker),
    
    
    ########################################################
    # 6.6 Firm age and manager experience
    ########################################################
    
    # b5 is usually the year when the establishment began operations.
    # If b5 is not available, try b6b.
    
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
    
    # b7 is usually manager experience in years.
    
    manager_experience = if ("b7" %in% names(raw_data)) {
      clean_negative_codes(b7)
    } else {
      NA_real_
    },
    
    manager_experience = ifelse(
      manager_experience < 0 | manager_experience > 80,
      NA_real_,
      manager_experience
    ),
    
    manager_experience = winsorise(manager_experience),
    
    
    ########################################################
    # 6.7 Monitoring and incentives
    ########################################################
    
    # r2: whether the establishment monitors production performance indicators.
    # This is interpreted as a monitoring mechanism.
    
    monitoring = if ("r2" %in% names(raw_data)) {
      make_yes_no_dummy(r2)
    } else {
      NA_real_
    },
    
    # r8: whether managers/workers receive bonuses based on production targets.
    # This is interpreted as an incentive mechanism.
    
    incentives = if ("r8" %in% names(raw_data)) {
      make_yes_no_dummy(r8)
    } else {
      NA_real_
    },
    
    
    ########################################################
    # 6.8 Main PBC component 1:
    #     Low reliability / opacity of reported figures
    ########################################################
    
    # a17 captures the reliability of answers about financial figures.
    # The theoretical idea is that weak or unreliable reporting increases
    # opacity and makes it easier for controlling insiders to extract
    # private benefits.
    #
    # We use both numeric codes and value labels because coding may differ
    # slightly across WBES versions.
    
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
    
    # Main dummy:
    # 1 = low reliability / high opacity
    # 0 = more reliable reporting
    #
    # The label-based rule is used first.
    # The numeric rule is a backup.
    
    low_data_reliability = case_when(
      str_detect(a17_label, "arbitrary|unreliable|not reliable|guess") ~ 1,
      str_detect(a17_label, "records|written|precision|precise|exact") ~ 0,
      a17_num == 3 ~ 1,
      a17_num %in% c(1, 2) ~ 0,
      TRUE ~ NA_real_
    ),
    
    
    ########################################################
    # 6.9 Main PBC component 2:
    #     Informal payments
    ########################################################
    
    # j7a measures informal payments as a percentage of annual sales.
    # This is used as a proxy for extraction / diversion of firm resources
    # in a weak governance environment.
    
    informal_payment_share = if ("j7a" %in% names(raw_data)) {
      clean_negative_codes(j7a)
    } else {
      NA_real_
    },
    
    # Keep valid percentage values only.
    
    informal_payment_share = ifelse(
      informal_payment_share < 0 | informal_payment_share > 100,
      NA_real_,
      informal_payment_share
    ),
    
    informal_payment_share = winsorise(informal_payment_share),
    
    # Convert informal payment share into a dummy.
    # 1 = firm reports positive informal payments
    # 0 = firm reports zero informal payments
    
    informal_payment_dummy = case_when(
      !is.na(informal_payment_share) &
        informal_payment_share > 0 ~ 1,
      !is.na(informal_payment_share) &
        informal_payment_share == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    
    
    ########################################################
    # 6.10 Main firm-level PBC proxy
    ########################################################
    
    # Main PBC proxy using only WBES information:
    #
    # PBC_core = average of:
    #   1. low_data_reliability
    #   2. informal_payment_dummy
    #
    # Values:
    #   0   = no sign of PBC risk from these two components
    #   0.5 = one component indicates PBC risk
    #   1   = both components indicate PBC risk
    
    pbc_core = rowMeans(
      cbind(low_data_reliability, informal_payment_dummy),
      na.rm = FALSE
    ),
    
    # Binary version:
    # 1 if at least one component indicates PBC risk.
    
    pbc_core_binary = case_when(
      !is.na(pbc_core) & pbc_core > 0 ~ 1,
      !is.na(pbc_core) & pbc_core == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    
    
    ########################################################
    # 6.11 Alternative PBC measure for robustness
    ########################################################
    
    # j30f measures whether corruption is an obstacle.
    # This is not the main PBC measure because it is a perception
    # of the business environment, not a direct extraction measure.
    # However, it is useful as a robustness check.
    
    corruption_obstacle = if ("j30f" %in% names(raw_data)) {
      case_when(
        clean_negative_codes(j30f) >= 3 ~ 1,
        clean_negative_codes(j30f) %in% c(0, 1, 2) ~ 0,
        TRUE ~ NA_real_
      )
    } else {
      NA_real_
    },
    
    # Alternative PBC proxy:
    # average of low reliability, informal payments, and corruption obstacle.
    
    pbc_alt = rowMeans(
      cbind(
        low_data_reliability,
        informal_payment_dummy,
        corruption_obstacle
      ),
      na.rm = FALSE
    ),
    
    
    ########################################################
    # 6.12 Additional outcomes and controls
    ########################################################
    
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
    
    product_innovation = if ("h1" %in% names(raw_data)) {
      make_yes_no_dummy(h1)
    } else {
      NA_real_
    },
    
    process_innovation = if ("h5" %in% names(raw_data)) {
      make_yes_no_dummy(h5)
    } else {
      NA_real_
    },
    
    capacity_utilization = if ("f1" %in% names(raw_data)) {
      clean_negative_codes(f1)
    } else {
      NA_real_
    },
    
    capacity_utilization = ifelse(
      capacity_utilization < 0 | capacity_utilization > 100,
      NA_real_,
      capacity_utilization
    ),
    
    number_competitors = if ("e2" %in% names(raw_data)) {
      clean_negative_codes(e2)
    } else {
      NA_real_
    },
    
    number_competitors = ifelse(
      number_competitors < 0 | number_competitors > 10000,
      NA_real_,
      number_competitors
    ),
    
    informal_competition = if ("e11" %in% names(raw_data)) {
      make_yes_no_dummy(e11)
    } else {
      NA_real_
    }
  )


############################################################
# 7. Construct leave-one-out PBC environment
############################################################

# Why do this?
#
# Firm-level PBC may be endogenous:
#   - poorly performing firms may report more corruption;
#   - firms with worse governance may report less reliable figures;
#   - unobserved managerial quality may affect both sales and PBC.
#
# To reduce this problem, we construct a local PBC environment measure:
# average PBC among other firms in the same country-year-industry cell.
#
# This is called leave-one-out because the firm's own PBC value is excluded.

data <- data %>%
  group_by(country_year, industry_fe) %>%
  mutate(
    
    # Number of firms with non-missing PBC in the cell
    
    n_pbc_group = sum(!is.na(pbc_core)),
    
    # Leave-one-out average of core PBC
    
    pbc_environment = case_when(
      n_pbc_group > 1 ~
        (sum(pbc_core, na.rm = TRUE) - pbc_core) / (n_pbc_group - 1),
      TRUE ~ NA_real_
    ),
    
    # Number of firms with non-missing alternative PBC in the cell
    
    n_pbc_alt_group = sum(!is.na(pbc_alt)),
    
    # Leave-one-out average of alternative PBC
    
    pbc_environment_alt = case_when(
      n_pbc_alt_group > 1 ~
        (sum(pbc_alt, na.rm = TRUE) - pbc_alt) / (n_pbc_alt_group - 1),
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup() %>%
  mutate(
    
    # High PBC environment dummy.
    # This is useful for easier graphical interpretation.
    
    high_pbc_environment = case_when(
      !is.na(pbc_environment) &
        pbc_environment >= median(pbc_environment, na.rm = TRUE) ~ 1,
      !is.na(pbc_environment) &
        pbc_environment < median(pbc_environment, na.rm = TRUE) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Interaction variables for diagnostics and VIF
    
    ownership_x_pbc_environment = ownership_10 * pbc_environment,
    
    ownership_x_high_pbc_environment =
      ownership_10 * high_pbc_environment
  )


############################################################
# 8. Construct analysis samples
############################################################

# Main sample for sales regressions
analysis_sales <- data %>%
  filter(
    latest_survey_sample == 1,
    !is.na(country_id),
    !is.na(survey_year),
    !is.na(country_year),
    !is.na(industry_fe),
    !is.na(ln_sales),
    !is.na(ownership_10),
    !is.na(pbc_core),
    !is.na(pbc_environment),
    !is.na(monitoring),
    !is.na(incentives),
    !is.na(ln_employees),
    !is.na(firm_age),
    !is.na(manager_experience)
  )

# Alternative outcome samples

analysis_productivity <- analysis_sales %>%
  filter(!is.na(ln_sales_per_worker))

analysis_export <- analysis_sales %>%
  filter(!is.na(exporter))

analysis_innovation <- analysis_sales %>%
  filter(
    !is.na(product_innovation),
    !is.na(process_innovation)
  )

analysis_capacity <- analysis_sales %>%
  filter(!is.na(capacity_utilization))

analysis_weighted <- analysis_sales %>%
  filter(!is.na(weight), weight > 0)


############################################################
# 9. Sample summary
############################################################

sample_summary <- tibble(
  sample = c(
    "Raw data",
    "Latest survey sample",
    "Main sales regression sample",
    "Sales per worker sample",
    "Export sample",
    "Innovation sample",
    "Capacity utilization sample",
    "Weighted sample"
  ),
  
  observations = c(
    nrow(raw_data),
    nrow(data %>% filter(latest_survey_sample == 1)),
    nrow(analysis_sales),
    nrow(analysis_productivity),
    nrow(analysis_export),
    nrow(analysis_innovation),
    nrow(analysis_capacity),
    nrow(analysis_weighted)
  )
)

write_csv(
  sample_summary,
  here::here("results2", "table_00_sample_summary.csv")
)


############################################################
# 10. Missing values summary
############################################################

missing_summary <- data %>%
  summarise(
    across(
      c(
        ln_sales,
        ownership_10,
        monitoring,
        incentives,
        low_data_reliability,
        informal_payment_dummy,
        pbc_core,
        pbc_environment,
        pbc_alt,
        pbc_environment_alt,
        ln_employees,
        firm_age,
        manager_experience,
        exporter,
        product_innovation,
        process_innovation,
        capacity_utilization
      ),
      ~ sum(is.na(.)),
      .names = "missing_{.col}"
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "missing_count"
  )

write_csv(
  missing_summary,
  here::here("results2", "table_00_missing_summary.csv")
)


############################################################
# 11. Descriptive statistics
############################################################

descriptive_data <- analysis_sales %>%
  select(
    ln_sales,
    sales_total,
    ownership,
    ownership_10,
    monitoring,
    incentives,
    low_data_reliability,
    informal_payment_dummy,
    informal_payment_share,
    pbc_core,
    pbc_core_binary,
    pbc_environment,
    high_pbc_environment,
    pbc_alt,
    pbc_environment_alt,
    ln_employees,
    employees,
    firm_age,
    manager_experience,
    exporter,
    product_innovation,
    process_innovation,
    capacity_utilization,
    number_competitors,
    informal_competition
  )

# Word table
datasummary_skim(
  descriptive_data,
  output = here::here("results2", "table_01_descriptive_statistics.docx")
)

# CSV version
descriptive_csv <- descriptive_data %>%
  summarise(
    across(
      everything(),
      list(
        n = ~ sum(!is.na(.)),
        mean = ~ mean(., na.rm = TRUE),
        sd = ~ sd(., na.rm = TRUE),
        min = ~ min(., na.rm = TRUE),
        p25 = ~ quantile(., 0.25, na.rm = TRUE),
        median = ~ median(., na.rm = TRUE),
        p75 = ~ quantile(., 0.75, na.rm = TRUE),
        max = ~ max(., na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = "statistic",
    values_to = "value"
  )

write_csv(
  descriptive_csv,
  here::here("results2", "table_01_descriptive_statistics.csv")
)


############################################################
# 12. Distribution tables for ownership and PBC
############################################################

ownership_distribution <- analysis_sales %>%
  mutate(
    ownership_group = case_when(
      ownership < 25 ~ "0-24%",
      ownership >= 25 & ownership < 50 ~ "25-49%",
      ownership >= 50 & ownership < 75 ~ "50-74%",
      ownership >= 75 ~ "75-100%",
      TRUE ~ NA_character_
    )
  ) %>%
  count(ownership_group) %>%
  mutate(
    share = n / sum(n)
  )

write_csv(
  ownership_distribution,
  here::here("results2", "table_02_ownership_distribution.csv")
)

pbc_distribution <- analysis_sales %>%
  summarise(
    n = n(),
    share_low_data_reliability = mean(low_data_reliability, na.rm = TRUE),
    share_informal_payment = mean(informal_payment_dummy, na.rm = TRUE),
    mean_informal_payment_share = mean(informal_payment_share, na.rm = TRUE),
    mean_pbc_core = mean(pbc_core, na.rm = TRUE),
    mean_pbc_environment = mean(pbc_environment, na.rm = TRUE),
    share_high_pbc_environment = mean(high_pbc_environment, na.rm = TRUE),
    mean_pbc_alt = mean(pbc_alt, na.rm = TRUE),
    mean_pbc_environment_alt = mean(pbc_environment_alt, na.rm = TRUE)
  )

write_csv(
  pbc_distribution,
  here::here("results2", "table_03_pbc_distribution.csv")
)


############################################################
# 13. Graphs
############################################################

# 13.1 Ownership distribution

fig_ownership_distribution <- ggplot(
  analysis_sales,
  aes(x = ownership)
) +
  geom_histogram(bins = 40, alpha = 0.8) +
  labs(
    title = "Distribution of ownership concentration",
    x = "Ownership concentration (%)",
    y = "Number of firms"
  ) +
  theme_minimal()

ggsave(
  here::here("results2", "fig_01_ownership_distribution.png"),
  fig_ownership_distribution,
  width = 7,
  height = 4,
  dpi = 300
)


# 13.2 Firm-level PBC distribution

fig_pbc_distribution <- ggplot(
  analysis_sales,
  aes(x = pbc_core)
) +
  geom_histogram(bins = 10, alpha = 0.8) +
  labs(
    title = "Distribution of firm-level PBC proxy",
    x = "PBC core index",
    y = "Number of firms"
  ) +
  theme_minimal()

ggsave(
  here::here("results2", "fig_02_pbc_core_distribution.png"),
  fig_pbc_distribution,
  width = 7,
  height = 4,
  dpi = 300
)


# 13.3 PBC environment distribution

fig_pbc_environment_distribution <- ggplot(
  analysis_sales,
  aes(x = pbc_environment)
) +
  geom_histogram(bins = 30, alpha = 0.8) +
  labs(
    title = "Distribution of leave-one-out PBC environment",
    x = "PBC environment",
    y = "Number of firms"
  ) +
  theme_minimal()

ggsave(
  here::here("results2", "fig_03_pbc_environment_distribution.png"),
  fig_pbc_environment_distribution,
  width = 7,
  height = 4,
  dpi = 300
)


# 13.4 Performance and ownership concentration

fig_sales_ownership <- ggplot(
  analysis_sales,
  aes(x = ownership, y = ln_sales)
) +
  geom_point(alpha = 0.12) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Firm performance and ownership concentration",
    x = "Ownership concentration (%)",
    y = "Log annual sales"
  ) +
  theme_minimal()

ggsave(
  here::here("results2", "fig_04_sales_by_ownership.png"),
  fig_sales_ownership,
  width = 7,
  height = 4,
  dpi = 300
)


# 13.5 Performance and ownership by PBC environment

fig_sales_ownership_pbc <- ggplot(
  analysis_sales,
  aes(
    x = ownership,
    y = ln_sales,
    color = as.factor(high_pbc_environment)
  )
) +
  geom_point(alpha = 0.10) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "Ownership-performance relationship by PBC environment",
    x = "Ownership concentration (%)",
    y = "Log annual sales",
    color = "High PBC environment"
  ) +
  theme_minimal()

ggsave(
  here::here("results2", "fig_05_sales_ownership_by_pbc.png"),
  fig_sales_ownership_pbc,
  width = 7,
  height = 4,
  dpi = 300
)


# 13.6 Mechanisms by ownership quartile

fig_mechanisms <- analysis_sales %>%
  mutate(
    ownership_quartile = ntile(ownership, 4),
    ownership_quartile = paste0("Q", ownership_quartile)
  ) %>%
  group_by(ownership_quartile) %>%
  summarise(
    monitoring = mean(monitoring, na.rm = TRUE),
    incentives = mean(incentives, na.rm = TRUE),
    pbc_core = mean(pbc_core, na.rm = TRUE),
    pbc_environment = mean(pbc_environment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(monitoring, incentives, pbc_core, pbc_environment),
    names_to = "mechanism",
    values_to = "mean_value"
  ) %>%
  ggplot(
    aes(x = ownership_quartile, y = mean_value, group = mechanism)
  ) +
  geom_line() +
  geom_point() +
  facet_wrap(~ mechanism, scales = "free_y") +
  labs(
    title = "Governance mechanisms by ownership concentration quartile",
    x = "Ownership concentration quartile",
    y = "Mean value"
  ) +
  theme_minimal()

ggsave(
  here::here("results2", "fig_06_mechanisms_by_ownership.png"),
  fig_mechanisms,
  width = 8,
  height = 5,
  dpi = 300
)


############################################################
# 14. Main regressions
############################################################

# Important modelling choices:
#
# 1. Ownership is scaled in 10 percentage points.
# 2. We do not include size categories. We use ln_employees instead.
# 3. Fixed effects:
#      - country-year fixed effects
#      - industry fixed effects
# 4. Standard errors are clustered at the country level.
# 5. Results are interpreted as conditional associations, not causal effects.

model_1_baseline <- feols(
  ln_sales ~ ownership_10 |
    country_year + industry_fe,
  data = analysis_sales,
  cluster = ~ country_id
)

model_2_controls <- feols(
  ln_sales ~ ownership_10 +
    ln_employees + firm_age + manager_experience |
    country_year + industry_fe,
  data = analysis_sales,
  cluster = ~ country_id
)

model_3_mechanisms <- feols(
  ln_sales ~ ownership_10 +
    monitoring + incentives + pbc_core +
    ln_employees + firm_age + manager_experience |
    country_year + industry_fe,
  data = analysis_sales,
  cluster = ~ country_id
)

model_4_pbc_environment <- feols(
  ln_sales ~ ownership_10 +
    monitoring + incentives + pbc_environment +
    ln_employees + firm_age + manager_experience |
    country_year + industry_fe,
  data = analysis_sales,
  cluster = ~ country_id
)

model_5_interaction <- feols(
  ln_sales ~ ownership_10 * pbc_environment +
    monitoring + incentives +
    ln_employees + firm_age + manager_experience |
    country_year + industry_fe,
  data = analysis_sales,
  cluster = ~ country_id
)

model_6_high_pbc_interaction <- feols(
  ln_sales ~ ownership_10 * high_pbc_environment +
    monitoring + incentives +
    ln_employees + firm_age + manager_experience |
    country_year + industry_fe,
  data = analysis_sales,
  cluster = ~ country_id
)

model_7_nonlinear <- feols(
  ln_sales ~ ownership_10 + ownership_sq +
    monitoring + incentives + pbc_environment +
    ln_employees + firm_age + manager_experience |
    country_year + industry_fe,
  data = analysis_sales,
  cluster = ~ country_id
)

main_models <- list(
  "Baseline" = model_1_baseline,
  "Controls" = model_2_controls,
  "Firm PBC" = model_3_mechanisms,
  "PBC environment" = model_4_pbc_environment,
  "Ownership x PBC environment" = model_5_interaction,
  "Ownership x high PBC" = model_6_high_pbc_interaction,
  "Nonlinear ownership" = model_7_nonlinear
)

modelsummary(
  main_models,
  output = here::here("results2", "table_04_main_regressions.docx"),
  stars = TRUE,
  gof_omit = "IC|Log|RMSE",
  notes = c(
    "Dependent variable: log annual sales.",
    "Ownership is measured in 10 percentage points.",
    "The firm-level PBC proxy combines low data reliability from a17 and informal payments from j7a.",
    "The preferred PBC measure is the leave-one-out country-year-industry PBC environment.",
    "All models include country-year and industry fixed effects.",
    "Standard errors are clustered at the country level.",
    "Results should be interpreted as conditional associations, not causal effects."
  )
)

modelsummary(
  main_models,
  output = here::here("results2", "table_04_main_regressions.html"),
  stars = TRUE,
  gof_omit = "IC|Log|RMSE"
)


############################################################
# 15. Mechanism regressions
############################################################

# These regressions test whether ownership concentration is associated
# with monitoring, incentives, and PBC.
#
# Monitoring and incentives are binary variables.
# We estimate linear probability models with fixed effects.

model_monitoring <- feols(
  monitoring ~ ownership_10 +
    ln_employees + firm_age + manager_experience |
    country_year + industry_fe,
  data = analysis_sales,
  cluster = ~ country_id
)

model_incentives <- feols(
  incentives ~ ownership_10 +
    ln_employees + firm_age + manager_experience |
    country_year + industry_fe,
  data = analysis_sales,
  cluster = ~ country_id
)

model_pbc_core <- feols(
  pbc_core ~ ownership_10 +
    ln_employees + firm_age + manager_experience |
    country_year + industry_fe,
  data = analysis_sales,
  cluster = ~ country_id
)

model_pbc_environment <- feols(
  pbc_environment ~ ownership_10 +
    ln_employees + firm_age + manager_experience |
    country_year + industry_fe,
  data = analysis_sales,
  cluster = ~ country_id
)

mechanism_models <- list(
  "Monitoring" = model_monitoring,
  "Incentives" = model_incentives,
  "Firm PBC" = model_pbc_core,
  "PBC environment" = model_pbc_environment
)

modelsummary(
  mechanism_models,
  output = here::here("results2", "table_05_mechanism_regressions.docx"),
  stars = TRUE,
  gof_omit = "IC|Log|RMSE",
  notes = c(
    "These models test whether ownership concentration is associated with proposed governance mechanisms.",
    "Monitoring and incentives are binary variables estimated using linear probability models.",
    "All models include country-year and industry fixed effects.",
    "Standard errors are clustered at the country level."
  )
)


############################################################
# 16. Alternative outcomes
############################################################

# These models check whether the relationship appears in other outcomes:
#   - sales per worker
#   - exporter status
#   - product innovation
#   - process innovation
#   - capacity utilization

model_productivity <- feols(
  ln_sales_per_worker ~ ownership_10 * pbc_environment +
    monitoring + incentives +
    firm_age + manager_experience |
    country_year + industry_fe,
  data = analysis_productivity,
  cluster = ~ country_id
)

model_exporter <- feols(
  exporter ~ ownership_10 * pbc_environment +
    monitoring + incentives +
    ln_employees + firm_age + manager_experience |
    country_year + industry_fe,
  data = analysis_export,
  cluster = ~ country_id
)

model_product_innovation <- feols(
  product_innovation ~ ownership_10 * pbc_environment +
    monitoring + incentives +
    ln_employees + firm_age + manager_experience |
    country_year + industry_fe,
  data = analysis_innovation,
  cluster = ~ country_id
)

model_process_innovation <- feols(
  process_innovation ~ ownership_10 * pbc_environment +
    monitoring + incentives +
    ln_employees + firm_age + manager_experience |
    country_year + industry_fe,
  data = analysis_innovation,
  cluster = ~ country_id
)

model_capacity <- feols(
  capacity_utilization ~ ownership_10 * pbc_environment +
    monitoring + incentives +
    ln_employees + firm_age + manager_experience |
    country_year + industry_fe,
  data = analysis_capacity,
  cluster = ~ country_id
)

alternative_models <- list(
  "Sales per worker" = model_productivity,
  "Exporter" = model_exporter,
  "Product innovation" = model_product_innovation,
  "Process innovation" = model_process_innovation,
  "Capacity utilization" = model_capacity
)

modelsummary(
  alternative_models,
  output = here::here("results2", "table_06_alternative_outcomes.docx"),
  stars = TRUE,
  gof_omit = "IC|Log|RMSE",
  notes = c(
    "Alternative outcomes are used as robustness and mechanism-related evidence.",
    "Binary outcomes are estimated using linear probability models.",
    "All models include country-year and industry fixed effects.",
    "Standard errors are clustered at the country level."
  )
)


############################################################
# 17. Robustness check:
#     Alternative PBC including corruption obstacle
############################################################

analysis_alt_pbc <- analysis_sales %>%
  filter(
    !is.na(pbc_alt),
    !is.na(pbc_environment_alt)
  )

model_alt_pbc_1 <- feols(
  ln_sales ~ ownership_10 * pbc_alt +
    monitoring + incentives +
    ln_employees + firm_age + manager_experience |
    country_year + industry_fe,
  data = analysis_alt_pbc,
  cluster = ~ country_id
)

model_alt_pbc_2 <- feols(
  ln_sales ~ ownership_10 * pbc_environment_alt +
    monitoring + incentives +
    ln_employees + firm_age + manager_experience |
    country_year + industry_fe,
  data = analysis_alt_pbc,
  cluster = ~ country_id
)

alt_pbc_models <- list(
  "Ownership x alternative firm PBC" = model_alt_pbc_1,
  "Ownership x alternative PBC environment" = model_alt_pbc_2
)

modelsummary(
  alt_pbc_models,
  output = here::here("results2", "table_07_robustness_alternative_pbc.docx"),
  stars = TRUE,
  gof_omit = "IC|Log|RMSE",
  notes = c(
    "Alternative PBC includes low data reliability, informal payments, and corruption obstacle.",
    "This is a robustness check. The preferred PBC measure uses only a17 and j7a.",
    "All models include country-year and industry fixed effects.",
    "Standard errors are clustered at the country level."
  )
)


############################################################
# 18. Robustness check:
#     Weighted regressions
############################################################

model_unweighted_same_sample <- feols(
  ln_sales ~ ownership_10 * pbc_environment +
    monitoring + incentives +
    ln_employees + firm_age + manager_experience |
    country_year + industry_fe,
  data = analysis_weighted,
  cluster = ~ country_id
)

model_weighted <- feols(
  ln_sales ~ ownership_10 * pbc_environment +
    monitoring + incentives +
    ln_employees + firm_age + manager_experience |
    country_year + industry_fe,
  data = analysis_weighted,
  weights = ~ weight,
  cluster = ~ country_id
)

weighted_models <- list(
  "Unweighted same sample" = model_unweighted_same_sample,
  "Weighted" = model_weighted
)

modelsummary(
  weighted_models,
  output = here::here("results2", "table_08_weighted_robustness.docx"),
  stars = TRUE,
  gof_omit = "IC|Log|RMSE",
  notes = c(
    "The weighted model uses WBES sampling weights.",
    "Both models use the same sample with non-missing positive weights.",
    "All models include country-year and industry fixed effects.",
    "Standard errors are clustered at the country level."
  )
)


############################################################
# 19. Robustness check:
#     Alternative fixed-effect structures
############################################################

model_fe_1 <- feols(
  ln_sales ~ ownership_10 * pbc_environment +
    monitoring + incentives +
    ln_employees + firm_age + manager_experience |
    country_year + industry_fe,
  data = analysis_sales,
  cluster = ~ country_id
)

model_fe_2 <- feols(
  ln_sales ~ ownership_10 * pbc_environment +
    monitoring + incentives +
    ln_employees + firm_age + manager_experience |
    country_year + sector_strata,
  data = analysis_sales,
  cluster = ~ country_id
)

model_fe_3 <- feols(
  ln_sales ~ ownership_10 * pbc_environment +
    monitoring + incentives +
    ln_employees + firm_age + manager_experience |
    country_id + survey_year + industry_fe,
  data = analysis_sales,
  cluster = ~ country_id
)

fe_models <- list(
  "Country-year + ISIC FE" = model_fe_1,
  "Country-year + sector FE" = model_fe_2,
  "Country + year + ISIC FE" = model_fe_3
)

modelsummary(
  fe_models,
  output = here::here("results2", "table_09_fixed_effects_robustness.docx"),
  stars = TRUE,
  gof_omit = "IC|Log|RMSE",
  notes = c(
    "This table checks whether results are robust to alternative fixed-effect structures.",
    "Standard errors are clustered at the country level."
  )
)


############################################################
# 20. Diagnostics
############################################################

# 20.1 Number of clusters and fixed-effect groups

cluster_summary <- analysis_sales %>%
  summarise(
    n_firms = n(),
    n_countries = n_distinct(country_id),
    n_country_years = n_distinct(country_year),
    n_industries = n_distinct(industry_fe),
    n_sectors = n_distinct(sector_strata)
  )

write_csv(
  cluster_summary,
  here::here("results2", "diagnostic_cluster_fe_summary.csv")
)


# 20.2 Correlation matrix

correlation_data <- analysis_sales %>%
  select(
    ln_sales,
    ownership_10,
    monitoring,
    incentives,
    pbc_core,
    pbc_environment,
    ln_employees,
    firm_age,
    manager_experience
  ) %>%
  drop_na()

correlation_matrix <- cor(correlation_data)

write_csv(
  as.data.frame(correlation_matrix) %>%
    rownames_to_column("variable"),
  here::here("results2", "diagnostic_correlation_matrix.csv")
)


# 20.3 VIF diagnostic

# VIF cannot be directly computed for high-dimensional fixed-effect models.
# Therefore, we use a simple auxiliary OLS model with the same main regressors.
# This is only a diagnostic for multicollinearity among regressors.

diagnostic_lm <- lm(
  ln_sales ~ ownership_10 + pbc_environment +
    ownership_x_pbc_environment +
    monitoring + incentives +
    ln_employees + firm_age + manager_experience,
  data = analysis_sales
)

vif_results <- car::vif(diagnostic_lm)

capture.output(
  vif_results,
  file = here::here("results2", "diagnostic_vif_results.txt")
)


############################################################
# 20.4 Residual plot for main model
############################################################

# The model may drop some observations internally because of missing values
# or fixed-effect issues. Therefore, we should not attach fitted values
# directly to analysis_sales. Instead, we create a separate dataset using
# only the observations actually used in the model.

residual_data <- data.frame(
  fitted_main_model = fitted(model_5_interaction),
  residual_main_model = resid(model_5_interaction)
)

fig_residuals <- ggplot(
  residual_data,
  aes(x = fitted_main_model, y = residual_main_model)
) +
  geom_point(alpha = 0.12) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residual plot: main ownership-performance model",
    x = "Fitted values",
    y = "Residuals"
  ) +
  theme_minimal()

ggsave(
  here::here("results2", "fig_07_residual_plot.png"),
  fig_residuals,
  width = 7,
  height = 4,
  dpi = 300
)
############################################################
# 21. Save cleaned analysis dataset
############################################################

write_csv(
  analysis_sales,
  here::here("results2", "analysis_dataset_main_sales.csv")
)


############################################################
# 22. Write methodological and interpretation notes
############################################################

method_notes <- paste0(
  "THE IMPACT OF OWNERSHIP CONCENTRATION ON FIRM PERFORMANCE\n\n",
  
  "1. Research question\n",
  "This project examines whether ownership concentration is associated with firm performance ",
  "and whether this relationship operates through monitoring, incentives, and private benefits of control (PBC).\n\n",
  
  "2. Main outcome\n",
  "The main dependent variable is log annual sales, constructed from d2.\n\n",
  
  "3. Ownership concentration\n",
  "Ownership concentration is constructed from b3. It is scaled in 10 percentage points, ",
  "so the coefficient on ownership_10 should be interpreted as the association of a 10 percentage-point increase ",
  "in ownership concentration with log sales.\n\n",
  
  "4. Monitoring and incentives\n",
  "Monitoring is constructed from r2. Incentives are constructed from r8. Both are coded as binary variables.\n\n",
  
  "5. PBC measurement using only WBES variables\n",
  "The WBES does not contain market-based information such as voting premia, block-transfer prices, or control premia. ",
  "Therefore, the script constructs a proxy for PBC risk using only information available in the dataset. ",
  "The main firm-level PBC proxy combines two components: low reliability of reported financial figures from a17 ",
  "and positive informal payments from j7a. The logic is that opaque reporting and informal payments are observable ",
  "manifestations of weak governance environments where private benefits of control are more likely.\n\n",
  
  "6. Preferred PBC environment measure\n",
  "To reduce simultaneity and firm-level reporting bias, the preferred PBC measure in regressions is a leave-one-out ",
  "country-year-industry average of firm-level PBC. This excludes the firm's own PBC value and captures the local ",
  "governance environment in which the firm operates.\n\n",
  
  "7. Main specification\n",
  "The main model regresses log sales on ownership concentration, PBC environment, the interaction between ownership ",
  "and PBC environment, monitoring, incentives, firm size, firm age, manager experience, country-year fixed effects, ",
  "and industry fixed effects. Standard errors are clustered at the country level.\n\n",
  
  "8. Interpretation of the interaction term\n",
  "The coefficient on ownership_10:pbc_environment shows whether the association between ownership concentration ",
  "and firm performance differs in environments where private benefits of control are more likely. ",
  "A negative coefficient would be consistent with the idea that ownership concentration is less beneficial when PBC risk is high.\n\n",
  
  "9. Endogeneity warning\n",
  "The results should not be interpreted as causal estimates. Ownership concentration is not randomly assigned. ",
  "Better firms may attract concentrated owners, concentrated owners may select into firms with higher expected performance, ",
  "and omitted managerial quality may affect both governance and performance. Monitoring, incentives, and PBC may also be endogenous. ",
  "Therefore, the results are best interpreted as conditional associations consistent with the proposed theoretical mechanisms.\n\n",
  
  "10. Robustness checks\n",
  "The script includes robustness checks using alternative outcomes, alternative PBC including corruption obstacle, ",
  "sampling weights, and alternative fixed-effect structures.\n\n"
)

writeLines(
  method_notes,
  here::here("results2", "methodological_and_interpretation_notes.txt")
)


############################################################
# 23. End of script
############################################################

message("Script completed successfully. Check the results2 folder.")
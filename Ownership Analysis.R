# 1. PACKAGES
install.packages(c(
  "haven",
  "tidyverse",
  "fixest",
  "modelsummary",
  "janitor",
  "skimr"
))

library(haven)
library(tidyverse)
library(fixest)
library(modelsummary)
library(janitor)
library(skimr)
library(tibble)

# 2. LOAD DATA
df <- read_dta("edata.dta")

# inspect first rows
head(df)

# 3. CLEAN VARIABLE NAMES
df <- clean_names(df)

# inspect names
names(df)

# 4. CHECK VARIABLE LABELS
var_labels <- tibble(
  variable = names(df),
  label = sapply(df, function(x) attr(x, "label"))
)

view(var_labels)

# 5. CLEAN NEGATIVE PLACEHOLDERS
# World Bank datasets often use negative codes for:
# don't know / refusal / not applicable

df <- df %>%
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(. < 0, NA, .)
    )
  )

# 6. CONSTRUCT VARIABLES
df_clean <- df %>%
  mutate(
    # DEPENDENT VARIABLE
    
    sales = ifelse(d2 > 0, d2, NA),
    log_sales = log(sales),

    # OWNERSHIP
    
    ownership = b3,
    
    # MECHANISMS
    
    monitoring = case_when(
      is.na(r2) ~ NA_real_,
      r2 == 1 ~ 1,
      TRUE ~ 0
    ),
    
    incentives = case_when(
      is.na(r8) ~ NA_real_,
      r8 == 1 ~ 1,
      TRUE ~ 0
    ),

    # PBC INDEX (La Porta adaptation)
    
    informal_payments = as.numeric(scale(j7a)),
    corruption = as.numeric(scale(j30f)),
    
    pbc = informal_payments + corruption,

    # INTERACTION

    ownership_pbc = ownership * pbc,

    # CONTROLS

    employees = l1,
    log_employees = log(l1 + 1),
    
    firm_age = ifelse((a20y - b5) >= 0, a20y - b5, NA),
    
    foreign_ownership = b2b,
    
    exporter = case_when(
      is.na(d3c) ~ NA_real_,
      d3c > 0 ~ 1,
      TRUE ~ 0
    ),
    
    survey_year = a20y
  )

# 7. SANITY CHECKS
colSums(is.na(df_clean[, c(
  "log_sales",
  "ownership",
  "monitoring",
  "incentives",
  "pbc",
  "log_employees",
  "firm_age",
  "foreign_ownership",
  "exporter"
)]))

summary(df_clean[, c(
  "log_sales",
  "ownership",
  "monitoring",
  "incentives",
  "pbc",
  "employees",
  "firm_age",
  "foreign_ownership"
)])

summary(df_clean$ownership)

# 8. CREATE FINAL REGRESSION SAMPLE
df_reg <- df_clean %>%
  filter(
    sample == 1,
    !is.na(log_sales),
    !is.na(ownership),
    !is.na(monitoring),
    !is.na(incentives),
    !is.na(pbc),
    !is.na(log_employees),
    !is.na(firm_age),
    !is.na(foreign_ownership),
    !is.na(exporter),
    ownership >= 0,
    ownership <= 100
  )

# 9. SAMPLE SIZE CHECK
nrow(df)
nrow(df_clean)
nrow(df_reg)

# 10. DESCRIPTIVE STATISTICS
datasummary_skim(
  df_reg %>%
    select(
      log_sales,
      ownership,
      monitoring,
      incentives,
      pbc,
      log_employees,
      firm_age,
      foreign_ownership
    )
)

# 11. CORRELATION MATRIX
cor(
  df_reg %>%
    select(
      ownership,
      monitoring,
      incentives,
      pbc,
      log_sales
    ),
  use = "complete.obs"
)

# 12. BASELINE REGRESSION
# Equation (1):
# ln(Sales) = Ownership + Controls + FE + error
model1 <- feols(
  log_sales ~ ownership +
    log_employees +
    firm_age +
    foreign_ownership +
    exporter
  | country + survey_year + isic_v4,
  cluster = ~country,
  data = df_reg
)

summary(model1)


# 13. MECHANISM REGRESSION
# Equation (2):
# ln(Sales) = Ownership + Monitoring + Incentives +
#             PBC + Ownership×PBC + Controls + FE + error

model2 <- feols(
  log_sales ~ ownership +
    monitoring +
    incentives +
    pbc +
    ownership_pbc +
    log_employees +
    firm_age +
    foreign_ownership +
    exporter
  | country + survey_year + isic_v4,
  cluster = ~country,
  data = df_reg
)

summary(model2)

# 15. ROBUSTNESS CHECK
# using composite PBC proxy
# (informal payments + corruption)

robust1 <- feols(
  log_sales ~ ownership +
    monitoring +
    incentives +
    informal_payments +
    corruption +
    ownership:informal_payments +
    ownership:corruption +
    log_employees +
    firm_age +
    foreign_ownership +
    exporter
  | country + survey_year + isic_v4,
  cluster = ~country,
  data = df_reg
)

summary(robust1)

# 16. EXPORT RESULTS

modelsummary(
  list(
    "Baseline" = model1,
    "Mechanism" = model2,
    "Robustness" = robust1
  ),
  stars = TRUE,
  output = "results.docx"
)
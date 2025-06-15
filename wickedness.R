
library(tidyverse)
library(janitor)
library(knitr)
library(psych)

# Assume df_fail is already in the environment with GPT_Fail and Forecast_Fail columns

# Create human-readable status columns
df_fail <- df_fail %>%
  mutate(
    GPT_Status   = if_else(GPT_Fail,       "GPT Fail", "GPT OK"),
    Human_Status = if_else(Forecast_Fail, "Human Fail", "Human OK")
  )

# Build and display an intuitive 2×2 table with counts and percentages
intuitive_table <- df_fail %>%
  tabyl(GPT_Status, Human_Status) %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") 

# Print the table
kable(
  intuitive_table,
  caption = "Contingency of GPT-4 vs. Human Forecast Failures (N = total studies)"
)

# 3.5 Compute chi-square test of independence on the 2×2 failure table --------
failure_table <- table(df_fail$GPT_Fail, df_fail$Forecast_Fail)
chi_test <- chisq.test(failure_table, correct = FALSE)  # continuity correction off for 2x2

# Extract test statistic and p-value
chi_stat  <- chi_test$statistic
chi_p     <- chi_test$p.value
chi_df    <- chi_test$parameter


# 4. Compute the raw correlation of the two failure indicators (phi coefficient)
phi_corr <- cor(
  as.integer(df_fail$GPT_Fail),
  as.integer(df_fail$Forecast_Fail)
)

# 5. Compute Cohen’s kappa for agreement beyond chance
kappa_val <- cohen.kappa(
  table(df_fail$GPT_Fail, df_fail$Forecast_Fail)
)$kappa

# 6. Assemble all stats into a single data frame
failure_pattern <- tibble(
  chi_squared_statistic = as.numeric(chi_stat),
  chi_df                = as.integer(chi_df),
  chi_p_value           = chi_p,
  phi_correlation       = phi_corr,
  cohens_kappa          = kappa_val
)

# Print result
print(failure_pattern, digits = 3)


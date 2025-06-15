## ── Topic‑level heterogeneity in prediction accuracy ─────────────────────────
##
## This script:
##   1. Reads the z‑score summary and computes GPT‑4 and crowd errors
##   2. Reads Study_type.csv (semicolon‑delimited) and keeps Study‑to‑Type pairs
##   3. Merges topic labels with the error table
##   4. Runs one‑way ANOVAs to test whether squared error differs by topic
##   5. Prints a tidy summary table (F, df, p) for GPT‑4 and the crowd
##
## Required packages: readr, dplyr, broom
## install.packages(c("readr", "dplyr", "broom"))  # run once if needed
###############################################################################

library(readr)
library(dplyr)
library(broom)

# 1.  Load z‑score summary and compute errors ---------------------------------
errors <- read_csv("final_summary_z.csv", show_col_types = FALSE) %>%
  transmute(
    study,
    GPT_Error   = GPT_Z_Estimate      - RCT_Z_Estimate,
    Crowd_Error = Forecast_Z_Estimate - RCT_Z_Estimate
  )

# 2.  Read Study_type.csv (semicolon‑separated) -------------------------------
topics <- read_delim("Study_type.csv",
                     delim = ";",
                     show_col_types = FALSE) %>%
  select(study, type) %>%      # keep only the two relevant columns
  rename(
    study = study,             # match the ID in 'errors'
    Topic = type               # clearer label
  )

# 3.  Merge topic labels with errors ------------------------------------------
df_topic <- errors %>%
  left_join(topics, by = "study") %>%
  filter(!is.na(Topic))        # retain only studies with a topic label

# 4.  One‑way ANOVA: Does error variance differ by topic? ---------------------
gpt_aov   <- oneway.test(I(GPT_Error^2)   ~ Topic, data = df_topic, var.equal = FALSE)
crowd_aov <- oneway.test(I(Crowd_Error^2) ~ Topic, data = df_topic, var.equal = FALSE)


# 5.  Summarise results in a tidy table ---------------------------------------
anova_summary <- bind_rows(
  tidy(gpt_aov)[1, ]   %>% mutate(Predictor = "GPT‑4"),
  tidy(crowd_aov)[1, ] %>% mutate(Predictor = "Crowd")
) %>%
  select(
    Predictor,
    num.df,
    den.df,
    F_value = statistic,
    p_value = p.value
  )


print(anova_summary, digits = 3)

## Optional: save the summary
# write_csv(anova_summary, "topic_heterogeneity_anova.csv")



#####TWO WAY ANOVA"######
# Required packages
library(readr)
library(dplyr)
library(tidyr)
library(broom)

# 1. Load z-score summary and compute squared errors
errors <- read_csv("final_summary_z.csv", show_col_types = FALSE) %>%
  transmute(
    study,
    GPT_SE   = (GPT_Z_Estimate      - RCT_Z_Estimate)^2,
    Crowd_SE = (Forecast_Z_Estimate - RCT_Z_Estimate)^2
  )

# 2. Load study topic labels
topics <- read_delim("Study_type.csv", delim = ";", show_col_types = FALSE) %>%
  select(study, type) %>%
  rename(Topic = type)

# 3. Merge and reshape to long format
df_long <- errors %>%
  left_join(topics, by = "study") %>%
  filter(!is.na(Topic)) %>%
  pivot_longer(
    cols = c(GPT_SE, Crowd_SE),
    names_to = "Predictor",
    values_to = "Squared_Error"
  ) %>%
  mutate(
    Predictor = case_when(
      Predictor == "GPT_SE"   ~ "GPT-4",
      Predictor == "Crowd_SE" ~ "Crowd",
      TRUE ~ as.character(Predictor)
    ),
    Predictor = factor(Predictor),
    Topic = factor(Topic)
  )

# 4. Run two-way ANOVA with interaction
interaction_model <- aov(Squared_Error ~ Predictor * Topic, data = df_long)

# 5. Tidy and print results
interaction_summary <- tidy(interaction_model)
print(interaction_summary, digits = 3)

# Optional: write results to CSV
# write_csv(interaction_summary, "predictor_topic_interaction_anova.csv")

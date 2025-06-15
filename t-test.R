## ── One‑sample t‑tests for systematic bias ───────────────────────────────────

# 1. Load required packages
# install.packages("readr")   # run once if not already installed
# install.packages("dplyr")

install.packages("broom")

library(readr)
library(dplyr)
library(broom)

# 2. Read the z‑score file and compute prediction errors
df <- read_csv("final_summary_z.csv") %>%
  mutate(
    GPT_Error   = GPT_Z_Estimate      - RCT_Z_Estimate,
    Crowd_Error = Forecast_Z_Estimate - RCT_Z_Estimate
  )

# 3. Run one‑sample t‑tests (null mean = 0)
gpt_t   <- t.test(df$GPT_Error,   mu = 0)
crowd_t <- t.test(df$Crowd_Error, mu = 0)

# 4. Tidy the output
results <- bind_rows(
  tidy(gpt_t)   %>% mutate(Predictor = "GPT‑4"),
  tidy(crowd_t) %>% mutate(Predictor = "Crowd")
) %>%
  select(Predictor,
         mean_error = estimate,
         t_value    = statistic,
         p_value    = p.value,
         ci_lower   = conf.low,
         ci_upper   = conf.high)

# 5. Display or save the summary table
print(results)              # default printing
# or, for a nicely rounded table:
library(knitr)
kable(results, digits = 3)

# write_csv(results, "bias_t_tests_summary.csv")

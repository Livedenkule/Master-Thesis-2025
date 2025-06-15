## ── Does error variance rise when studies are smaller? ──────────────────────
library(readr)
library(dplyr)
library(ggplot2)
library(broom)

# 1.  Per‑study errors (already cleaned) --------------------------------------
#     Contains one row per study with GPT and Crowd errors
errors <- read_csv("final_summary_z.csv") %>%
  mutate(
    GPT_Error   = GPT_Z_Estimate      - RCT_Z_Estimate,
    Crowd_Error = Forecast_Z_Estimate - RCT_Z_Estimate
  ) %>%
  select(study, GPT_Error, Crowd_Error)

# 2.  Per‑row RCT data with participant counts --------------------------------
#     Each row is one condition; has N_total.rct in every row.
rct_rows <- read_csv("rct_unnested.csv")

design_info <- rct_rows %>%                     # rct_rows already read
  group_by(study) %>%
  summarise(
    n_rows         = n(),                       # total rows in this study
    n_outcomes     = n_distinct(outcome.name),  # how many outcome variables
    N_participants = n_rows / n_outcomes        # rows per person = outcomes
  ) %>%
  ungroup()


# 3.  Merge design features with errors --------------------------------------
df <- errors %>%
  left_join(design_info, by = "study") %>%
  mutate(
    GPT_SqError   = GPT_Error^2,
    Crowd_SqError = Crowd_Error^2
  )

# 4.  Quick test: does squared error depend on sample size? -------------------
gpt_lm   <- lm(GPT_SqError   ~ I(1 / N_participants), data = df)
crowd_lm <- lm(Crowd_SqError ~ I(1 / N_participants), data = df)

summary(gpt_lm)
summary(crowd_lm)

# 5.  Scatterplot for visual confirmation ------------------------------------
ggplot(df, aes(x = N_participants, y = GPT_SqError)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, colour = "red") +
  scale_x_continuous(trans = "log10") +
  labs(
    x = "Total sample size of RCT (log scale)",
    y = "Squared GPT‑4 error (SD²)",
    title = "GPT‑4 accuracy vs. study sample size"
  ) +
  theme_minimal()

# Repeat with Crowd_SqError if desired

# ── Crowd version ──────────────────────────────────────────────
ggplot(df, aes(x = N_participants, y = Crowd_SqError)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, colour = "blue") +
  scale_x_continuous(trans = "log10") +
  labs(
    x = "Total sample size of RCT (log scale)",
    y = "Squared crowd error (SD²)",
    title = "Crowd accuracy vs. study sample size"
  ) +
  theme_minimal()



## ── Summarise the “error‑vs‑sample‑size” regressions in one table ───────────

library(readr)
library(dplyr)
library(broom)

# 1.  Load the error file and compute squared errors
df <- read_csv("final_summary_z.csv") %>%
  mutate(
    GPT_Error   = GPT_Z_Estimate      - RCT_Z_Estimate,
    Crowd_Error = Forecast_Z_Estimate - RCT_Z_Estimate
  )

# 2.  Load RCT rows, count respondents per study,
#     and merge with the error data (as in the earlier code)
rct_rows <- read_csv("rct_unnested.csv")

design_info <- rct_rows %>%
  group_by(study) %>%
  summarise(
    n_rows         = n(),
    n_outcomes     = n_distinct(outcome.name),
    N_participants = n_rows / n_outcomes
  ) %>%
  ungroup()

df <- df %>%
  left_join(design_info, by = "study") %>%
  mutate(
    GPT_SqError   = GPT_Error^2,
    Crowd_SqError = Crowd_Error^2
  )

# 3.  Fit both models
gpt_lm   <- lm(GPT_SqError   ~ I(1 / N_participants), data = df)
crowd_lm <- lm(Crowd_SqError ~ I(1 / N_participants), data = df)

# 4.  Extract tidy coefficients and R² using broom
results <- bind_rows(
  tidy(gpt_lm)   %>% filter(term == "I(1/N_participants)") %>%   # slope only
    mutate(R2 = glance(gpt_lm)$r.squared,
           Predictor = "GPT‑4"),
  tidy(crowd_lm) %>% filter(term == "I(1/N_participants)") %>%
    mutate(R2 = glance(crowd_lm)$r.squared,
           Predictor = "Crowd")
) %>%
  select(
    Predictor,
    beta = estimate,
    se   = std.error,
    t    = statistic,
    p    = p.value,
    R2
  )

# 5.  Display or save
print(results, digits = 3)
# write_csv(results, "error_vs_sample_size_summary.csv")


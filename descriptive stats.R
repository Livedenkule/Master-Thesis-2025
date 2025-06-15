## ── Descriptive accuracy: RMSE, MAE, mean, SD, min, max ─────────────────────

library(readr)
library(dplyr)

# 1.  Load per‑study z‑score summary and compute errors -----------------------
errors <- read_csv("final_summary_z.csv", show_col_types = FALSE) %>%
  transmute(
    study,
    GPT_Error   = GPT_Z_Estimate      - RCT_Z_Estimate,
    Crowd_Error = Forecast_Z_Estimate - RCT_Z_Estimate
  )

# 2.  Helper function to compute summary metrics -----------------------------
summarise_error <- function(x) {
  tibble(
    RMSE = sqrt(mean(x^2, na.rm = TRUE)),
    MAE  = mean(abs(x),  na.rm = TRUE),
    Mean = mean(x,       na.rm = TRUE),
    SD   = sd(x,         na.rm = TRUE),
    Min  = min(x,        na.rm = TRUE),
    Max  = max(x,        na.rm = TRUE)
  )
}

# 3.  Create a tidy summary table --------------------------------------------
accuracy_summary <- bind_rows(
  summarise_error(errors$GPT_Error)   %>% mutate(Predictor = "GPT‑4"),
  summarise_error(errors$Crowd_Error) %>% mutate(Predictor = "Crowd")
) %>%
  relocate(Predictor)

print(accuracy_summary, digits = 3)

# Optional: save to CSV
# write_csv(accuracy_summary, "accuracy_summary_rmse_mae.csv")




library(readr)
library(dplyr)

# 1. Read the z‑score file and compute GPT‑error
df <- read_csv("final_summary_z.csv", show_col_types = FALSE) %>%
  mutate(
    GPT_Error = GPT_Z_Estimate - RCT_Z_Estimate
  )

# 2. Choose a threshold (here ±2.5 SD) and list offending rows
threshold <- 2.5

large_errors <- df %>%
  filter(abs(GPT_Error) > threshold) %>%
  select(
    study,
    RCT_Z_Estimate,
    GPT_Z_Estimate,
    GPT_Error
  )

print(large_errors, digits = 3)


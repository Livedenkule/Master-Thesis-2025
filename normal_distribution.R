## ── Normality check for GPT‑4 and crowd errors ────────────────────────
library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)     # to place plots side by side

# 1. Load the z‑score summary and compute errors
df <- read_csv("final_summary_z.csv") %>%
  mutate(
    GPT_Error      = GPT_Z_Estimate      - RCT_Z_Estimate,
    Crowd_Error    = Forecast_Z_Estimate - RCT_Z_Estimate
  )

# 2. Shapiro–Wilk tests (null = normal distribution)
gpt_sw   <- shapiro.test(df$GPT_Error)
crowd_sw <- shapiro.test(df$Crowd_Error)

cat("Shapiro–Wilk GPT‑4: W =", round(gpt_sw$statistic, 3),
    " p =", round(gpt_sw$p.value, 4), "\n")
cat("Shapiro–Wilk Crowd: W =", round(crowd_sw$statistic, 3),
    " p =", round(crowd_sw$p.value, 4), "\n\n")

# 3. Q–Q plots
qq_gpt <- ggplot(df, aes(sample = GPT_Error)) +
  stat_qq() + stat_qq_line() +
  labs(title = "GPT‑4 errors", x = "Theoretical quantiles", y = "Sample quantiles") +
  theme_minimal(base_size = 11)

qq_crowd <- ggplot(df, aes(sample = Crowd_Error)) +
  stat_qq() + stat_qq_line() +
  labs(title = "Crowd errors", x = "Theoretical quantiles", y = "Sample quantiles") +
  theme_minimal(base_size = 11)

# Display plots side by side
(qq_gpt | qq_crowd)

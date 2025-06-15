library(readr)
library(dplyr)
library(ggplot2)   # optional

df <- read_csv("final_summary_z.csv")

# Signed prediction errors  (use the *mean* column)
df <- df %>%
  mutate(
    err_gpt   = GPT_Z_Estimate      - RCT_Z_Estimate,
    err_crowd = Forecast_Z_Estimate - RCT_Z_Estimate
  )

# Quick sanity check
summary(df$err_gpt)       # mean should now be near 0
summary(df$err_crowd)     # mean near 0
full_r <- cor(df$err_gpt, df$err_crowd, use = "complete.obs")
full_r                       # expect ≈ 0.39

# Leave-one-out loop
loo_r <- sapply(seq_len(nrow(df)), function(i) {
  cor(df$err_gpt[-i], df$err_crowd[-i], use = "complete.obs")
})

cat("Full-sample r :", round(full_r, 3), "\n")
cat("LOO range     :", round(range(loo_r), 3), "\n")
cat("Largest Δr    :", round(max(abs(loo_r - full_r)), 3), "\n")

# Optional plot
ggplot(
  tibble(study = df$study, r_loo = loo_r),
  aes(x = reorder(study, r_loo), y = r_loo)
) +
  geom_hline(yintercept = full_r, linetype = "dashed", colour = "red") +
  geom_point() +
  coord_flip() +
  labs(
    x = "Study omitted",
    y = "Leave-one-out correlation",
    title = "Sensitivity of GPT-4 × Crowd error correlation"
  ) +
  theme_minimal()

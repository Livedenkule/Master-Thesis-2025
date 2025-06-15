###########################################################################
##  Full, self-contained R script to test whether GPT-4 and human errors  ##
##  are more tightly aligned on “wicked” items (co-failures) than on     ##
##  the remaining studies, using a Fisher r-to-z difference test.        ##
###########################################################################

# ── 0.  Load libraries ───────────────────────────────────────────────────
library(tidyverse)   # dplyr, readr, etc.
library(psych)       # for optional Cohen’s kappa if you want it
library(boot)        # optional bootstrap CI for the r-difference

# ── 1.  Read the data ----------------------------------------------------
# (Assumes final_summary_z.csv is in the working directory)
df <- read_csv("final_summary_z.csv",
               show_col_types = FALSE)  # suppress column messages

# ── 2.  Compute signed errors for GPT-4 and human crowd ------------------
df <- df %>%
  mutate(
    gpt_error   = GPT_Z_Estimate      - RCT_Z_Estimate,
    crowd_error = Forecast_Z_Estimate - RCT_Z_Estimate
  )

# ── 3.  Label “fail” cases (|error| > 1.0 z-unit) ------------------------
df <- df %>%
  mutate(
    gpt_fail      = abs(gpt_error)   > 1,
    forecast_fail = abs(crowd_error) > 1,
    wicked        = gpt_fail & forecast_fail        # both miss badly
  )

# Quick sanity-check: how many wicked items?
print(table(df$wicked))
#> FALSE  TRUE
#>    32    10

# ── 4.  Correlations inside vs. outside wicked subset -------------------
r_wicked <- cor(df$gpt_error[df$wicked],
                df$crowd_error[df$wicked])          # n = 10
r_clean  <- cor(df$gpt_error[!df$wicked],
                df$crowd_error[!df$wicked])         # n = 32

n_wicked <- sum(df$wicked)
n_clean  <- sum(!df$wicked)

# ── 5.  Fisher r-to-z difference test -----------------------------------
z1 <- 0.5 * log((1 + r_wicked) / (1 - r_wicked))
z2 <- 0.5 * log((1 + r_clean ) / (1 - r_clean ))
se <- sqrt(1 / (n_wicked - 3) + 1 / (n_clean - 3))
z_diff <- (z1 - z2) / se
p_val  <- 2 * pnorm(-abs(z_diff))

cat(sprintf(
  "Correlation (wicked):      r = %.2f  (n = %d)\n", r_wicked,  n_wicked))
cat(sprintf(
  "Correlation (non-wicked):  r = %.2f  (n = %d)\n", r_clean,   n_clean))
cat(sprintf(
  "Fisher z-difference:       z = %.2f,  p = %.3f\n", z_diff, p_val))



# ── 7.  (Optional) Cohen’s kappa & chi-square on the 2×2 fail table -----
fail_table <- table(df$gpt_fail, df$forecast_fail)
chi_out    <- chisq.test(fail_table, correct = FALSE)
kappa_val  <- cohen.kappa(fail_table)$kappa

cat(sprintf(
  "\nChi-square on fail table: χ² = %.2f,  df = %d,  p = %.3f",
  chi_out$statistic, chi_out$parameter, chi_out$p.value))
cat(sprintf("\nCohen’s κ for co-fail agreement: κ = %.2f\n", kappa_val))



ggplot(df, aes(crowd_error, gpt_error, colour = wicked)) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  geom_point(size = 3) +
  labs(x = "Human signed error", y = "GPT-4 signed error")

# ── prerequisites ───────────────────────────────────────────────────────────
# df must already have: gpt_error, crowd_error, wicked  (logical)
x <- df$gpt_error  [df$wicked]           # GPT errors on the 10 wicked items
y <- df$crowd_error[df$wicked]           # Human errors on the 10 wicked items
n <- length(x)                           # should be 10

# ── 1.  Bootstrap 95 % CI for Pearson r  (10 000 resamples) ────────────────
boot_cor <- function(data, idx) cor(data[idx, 1], data[idx, 2])

set.seed(123)
boot_out <- boot::boot(cbind(x, y), boot_cor, R = 10000)
boot_ci  <- boot::boot.ci(boot_out, type = "perc")  # percentile CI

# ── 2.  Manual permutation test for exact p-value  (10 000 shuffles) ──────
set.seed(456)
perm_stats <- replicate(10000, {
  cor(x, sample(y))                    # shuffle y labels
})
p_perm <- mean(abs(perm_stats) >= abs(boot_out$t0))

# ── 3.  Report results ─────────────────────────────────────────────────────
cat(sprintf(
  "Observed r = %.2f\nBootstrap 95%% CI = [%.2f, %.2f]\nPermutation p = %.3f\n",
  boot_out$t0,
  boot_ci$percent[4],   # lower bound
  boot_ci$percent[5],   # upper bound
  p_perm
))

# run_final_z_pipeline.R

# ── 0. Load libraries ─────────────────────────────────────────────────────
library(tidyverse)
library(purrr)

# ── 1. Read raw data ──────────────────────────────────────────────────────
df_rct      <- read_csv("rct_unnested.csv")             # columns: study, outcome.name, condition.name, y, ...
df_llm      <- read_csv("df_llm_subset_clean_2.csv")   # columns: study, outcome.name, condition.name, expectation, ...
df_forecast <- read_csv("forecasting_responses_clean.csv")  # columns: study, outcome.name, condition.name, value, ...

# ── 2. Random outcome + reference assignment ──────────────────────────────
random_assignments <- function(df_rct, df_llm, df_frc) {
  map_dfr(unique(df_rct$study), function(study_id) {
    # 2a) common outcomes
    outcomes <- reduce(list(
      df_rct      %>% filter(study == study_id) %>% pull(outcome.name),
      df_llm      %>% filter(study == study_id) %>% pull(outcome.name),
      df_frc      %>% filter(study == study_id) %>% pull(outcome.name)
    ), intersect)
    if (length(outcomes) < 1) return(NULL)
    outcome <- sample(outcomes, 1)
    
    # 2b) common conditions for that outcome
    conds <- reduce(list(
      df_rct      %>% filter(study == study_id, outcome.name == outcome) %>% pull(condition.name),
      df_llm      %>% filter(study == study_id, outcome.name == outcome) %>% pull(condition.name),
      df_frc      %>% filter(study == study_id, outcome.name == outcome) %>% pull(condition.name)
    ), intersect)
    if (length(conds) < 2) return(NULL)
    
    tibble(
      study     = study_id,
      outcome   = outcome,
      reference = sample(conds, 1)
    )
  })
}

# ── 3. Compute one run of z‑score contrasts ────────────────────────────────
compute_run_z <- function(run_id, assigns, df_rct, df_llm, df_frc, seed = 123) {
  set.seed(seed + run_id)
  map_dfr(seq_len(nrow(assigns)), function(i) {
    s   <- assigns$study[i]
    o   <- assigns$outcome[i]
    ref <- assigns$reference[i]
    
    # 3a) get condition means
    means <- list(
      df_rct %>% filter(study == s, outcome.name == o) %>%
        group_by(condition.name) %>% summarise(rct  = mean(y, na.rm=TRUE), .groups="drop"),
      df_llm %>% filter(study == s, outcome.name == o) %>%
        group_by(condition.name) %>% summarise(gpt  = mean(expectation, na.rm=TRUE), .groups="drop"),
      df_frc %>% filter(study == s, outcome.name == o) %>%
        group_by(condition.name) %>% summarise(fore = mean(value, na.rm=TRUE), .groups="drop")
    ) %>% reduce(inner_join, by="condition.name")
    
    # 3b) sanity checks
    if (!(ref %in% means$condition.name))                      return(NULL)
    if (anyNA(means$rct) || anyNA(means$gpt) || anyNA(means$fore)) return(NULL)
    if (sd(means$rct)==0 || sd(means$gpt)==0 || sd(means$fore)==0)   return(NULL)
    
    # 3c) standardize to z‑scores
    means <- means %>% mutate(
      rct_z  = (rct  - mean(rct , na.rm=TRUE)) / sd(rct , na.rm=TRUE),
      gpt_z  = (gpt  - mean(gpt , na.rm=TRUE)) / sd(gpt , na.rm=TRUE),
      fore_z = (fore - mean(fore, na.rm=TRUE)) / sd(fore, na.rm=TRUE)
    )
    
    # 3d) contrasts vs. reference
    rv    <- means %>% filter(condition.name == ref)
    conts <- means %>% filter(condition.name != ref) %>%
      transmute(
        rct_z_contrast    = rct_z  - rv$rct_z,
        gpt_z_contrast    = gpt_z  - rv$gpt_z,
        fore_z_contrast   = fore_z - rv$fore_z
      )
    if (nrow(conts)==0) return(NULL)
    
    # 3e) return one row
    tibble(
      study               = s,
      run                 = run_id,
      RCT_Z_Estimate      = mean(conts$rct_z_contrast,  na.rm=TRUE),
      GPT_Z_Estimate      = mean(conts$gpt_z_contrast,  na.rm=TRUE),
      GPT_Z_SD            = sd(  conts$gpt_z_contrast,  na.rm=TRUE),
      Forecast_Z_Estimate = mean(conts$fore_z_contrast, na.rm=TRUE),
      Forecast_Z_SD       = sd(  conts$fore_z_contrast, na.rm=TRUE)
    )
  })
}

# ── 4. Run 16 times ───────────────────────────────────────────────────────
assigns <- random_assignments(df_rct, df_llm, df_forecast)
all_runs <- map_dfr(1:16, ~ compute_run_z(.x, assigns, df_rct, df_llm, df_forecast))

# ── 5. Drop any rows with NA (studies with missing SDs) ────────────────
all_runs <- all_runs %>% drop_na()

# ── 6. Collapse to one row per study (average across runs) ─────────────
final_summary_z <- all_runs %>%
  group_by(study) %>%
  summarise(
    RCT_Z_Estimate      = mean(RCT_Z_Estimate,     na.rm=TRUE),
    GPT_Z_Estimate      = mean(GPT_Z_Estimate,     na.rm=TRUE),
    GPT_Z_SD            = mean(GPT_Z_SD,           na.rm=TRUE),
    Forecast_Z_Estimate = mean(Forecast_Z_Estimate,na.rm=TRUE),
    Forecast_Z_SD       = mean(Forecast_Z_SD,      na.rm=TRUE),
    .groups = "drop"
  )

# ── 7. Write out the final file ─────────────────────────────────────────
write_csv(final_summary_z, "final_summary_z.csv")
cat("✔ Wrote", nrow(final_summary_z), "studies to final_summary_z.csv\n\n")

# ── 8. Compute & print error‐correlation and error‐variance ─────────────
metrics <- final_summary_z %>%
  mutate(
    GPT_Error      = GPT_Z_Estimate      - RCT_Z_Estimate,
    Forecast_Error = Forecast_Z_Estimate - RCT_Z_Estimate
  ) %>%
  summarise(
    Error_Correlation    = cor(GPT_Error, Forecast_Error),
    GPT_Error_Var        = var(GPT_Error),
    Forecast_Error_Var   = var(Forecast_Error),
    Variance_Difference  = GPT_Error_Var - Forecast_Error_Var
  )

print(metrics)



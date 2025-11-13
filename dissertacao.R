# Load necessary libraries
library(fixest)
library(dplyr)
library(ggplot2)
library(modelsummary)
library(readxl)

rm(list=ls())
# Assuming 'df' is your dataframe with columns:
# student_id, ano_observacao, remuneracao_media, mestre (1=graduate, 0=control),
# and mestrado (1=post-graduation period for graduates, 0 otherwise).
df = read_xlsx("/home/freitas/Downloads/0research/nilson/outubro/5. dados_longos_grupos.xlsx")
df <- df %>%
  filter(!situação %in% c("matriculado"))
#"desistente","desligado"
### 1 Estimate the Average Causal Effect

# First, create the 'event_time' variable as before
df <- df %>%
  group_by(cpf) %>%
  mutate(
    event_time = if_else(!is.na(ano_conclusao), ano_observacao - ano_conclusao, NA_real_))
  #) %>%
  # # Bin endpoints for a cleaner plot
  # mutate(
  #   event_time = case_when(
  #     event_time < -5 ~ -5,
  #     event_time > 5  ~ 5,
  #     TRUE ~ event_time
  #   )
  # )

# Bin endpoints for a cleaner plot
df <- df %>%
  group_by(cpf) %>%
  mutate(treated_group = max(mestre, na.rm = TRUE)) %>%
  ungroup()

# create time_to_treatement variable which is equal to event_time for treated units and -1000 for never treated
df <- df %>%
  mutate(time_to_treatment = if_else(treated_group == 1, event_time, -1000))


# TWFE DiD (note that the time to treatment for the never treated is -1000)
# (by using ref = c(-1, -1000) we exclude the period just before the treatment and 
# the never treated)
res_twfe = feols(log(remuneracao_media) ~ i(time_to_treatment, ref = c(-1, -1000)) | cpf + ano_observacao, data=df)
summary(res_twfe)

# Compare the results
# Plot the two TWFE results
iplot(res_twfe)

#https://docs.google.com/spreadsheets/d/1y79jJyIc0TFAob4qipMketKjixuXub6T/edit?usp=drive_link&ouid=105674475394683857648&rtpof=true&sd=true

# Load necessary libraries
library(fixest)
library(dplyr)
library(ggplot2)
library(modelsummary)
library(readxl)

rm(list=ls())

# Load and filter data
df = read_xlsx("/home/freitas/Downloads/0research/nilson/outubro/5. dados_longos_grupos.xlsx")

df <- df %>%
  filter(!situação %in% c("matriculado")) %>%
  filter(ano_observacao >= 2012 & ano_observacao <= 2022)

#==================================================================
# 1. VARIABLE PREPARATION
#==================================================================

# --- A. Variables for 'Classic TWFE' (i() function) ---
# This method requires a special time variable (-1000) for the control group.
df_1_twfe <- df %>%
  group_by(cpf) %>%
  mutate(
    event_time = ano_observacao - ano_conclusao,
    treated_group = max(mestre, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    # Create the relative time variable, binned at -5 and +5
    time_to_treatment = case_when(
      treated_group == 0 ~ -1000, # Control group
      event_time < -5  ~ -5,
      event_time > 5   ~ 5,
      TRUE ~ event_time
    )
  )

# --- B. Variables for 'Sun & Abraham' (sunab() function) ---
# This method requires the 'cohort' variable to be NA for the control group.
# The 'period' variable is just the calendar year.
df_1_sunab <- df %>%
  mutate(
    # Create cohort variable: year of treatment, or NA if never-treated
    sunab_cohort = if_else(mestre == 1, ano_conclusao, NA_integer_)
  )

# --- C. Create Log Outcome Variables (for both datasets) ---
# (Using log(x+1) for hours/tenure to handle potential zeros)
df_1_twfe <- df_1_twfe %>%
  mutate(
    log_rem_media = log(remuneracao_media),
    log_rem_real = log(remuneracao_media_real),
    log_horas = log(as.numeric(horas_contratuais) + 1),
    log_tempo = log(as.numeric(tempo_emprego) + 1)
  )

df_1_sunab <- df_1_sunab %>%
  mutate(
    log_rem_media = log(remuneracao_media),
    log_rem_real = log(remuneracao_media_real),
    log_horas = log(as.numeric(horas_contratuais) + 1),
    log_tempo = log(as.numeric(tempo_emprego) + 1)
  )


#===================as.numeric()#==================================================================
# 2. RUN MAIN REGRESSIONS (Nominal Remuneration)
#==================================================================

# --- Classic TWFE Model ---
res_twfe_media = feols(log_rem_media ~ i(time_to_treatment, ref = c(-1, -1000)) | cpf + ano_observacao, data=df_1_twfe)

# --- Sun & Abraham Model (Corrected) ---
# We use sunab_cohort (with NAs for controls) and the calendar year (ano_observacao)
# ref.c = -1 sets the reference to the year just before treatment
res_sunab_media = feols(log_rem_media ~ sunab(sunab_cohort, ano_observacao, ref.c = -1) | cpf + ano_observacao, data=df_1_sunab)

# Load and filter
df = read_xlsx("/home/freitas/Downloads/0research/nilson/outubro/5. dados_longos_grupos.xlsx")

df <- df %>%
  filter(!situação %in% c("matriculado")) %>%
  filter(ano_observacao >= 2012 & ano_observacao <= 2022)

# --- START: THE FIX ---
# We must convert 'ano' to an integer so it matches the type of 'NA_integer_'
df_1_sunab <- df %>%
  mutate(
    sunab_cohort = if_else(mestre == 1, as.integer(ano), NA_integer_)
  )
# --- END: THE FIX ---

# Create log outcome (using log(x+1) to be safe from zeros)
df_1_sunab <- df_1_sunab %>%
  mutate(
    log_rem_media = log(remuneracao_media + 1)
  )

# --- Check the cohort variable again ---
print("Checking the new 'sunab_cohort' variable:")
print(table(df_1_sunab$sunab_cohort, useNA = "ifany"))
# This should now show years (e.g., 2015, 2016) and NAs.

# Re-run the model
res_sunab_media = feols(
  log_rem_media ~ sunab(sunab_cohort, ano_observacao, ref.c = -1) | cpf + ano_observacao,
  data = df_1_sunab
)

summary(res_sunab_media)
iplot(res_sunab_media)

summary(res_sunab_media)
iplot(res_sunab_media)


# Plot and compare
iplot(list(res_twfe_media, res_sunab_media),
      main = "Event Study: TWFE vs. Sun & Abraham",
      xlab = "Years Relative to Master's Degree Completion",
      ylab = "Effect on Log(Nominal Income)")
legend("bottomright", legend = c("TWFE", "Sun & Abraham"), col = 1:2, pch = 20)


#==================================================================
# 3. RUN ADDITIONAL REGRESSIONS (Other Outcomes)
#==================================================================

# --- Outcome: Real Remuneration (Sunab) ---
res_sunab_real = feols(log_rem_real ~ sunab(sunab_cohort, ano_observacao, ref.c = -1) | cpf + ano_observacao, data=df_1_sunab)

# --- Outcome: Contractual Hours (Sunab) ---
res_sunab_horas = feols(log_horas ~ sunab(sunab_cohort, ano_observacao, ref.c = -1) | cpf + ano_observacao, data=df_1_sunab)

# --- Outcome: Job Tenure (Sunab) ---
res_sunab_tempo = feols(log_tempo ~ sunab(sunab_cohort, ano_observacao, ref.c = -1) | cpf + ano_observacao, data=df_1_sunab)


#==================================================================
# 4. PUBLICATION-READY TABLES & FIGURES
#==================================================================

# --- Figure: Plot all outcomes from the robust Sunab model ---
iplot(list(res_sunab_media, res_sunab_real, res_sunab_horas, res_sunab_tempo),
      main = "Effect of Master's Degree on Various Outcomes (Sun & Abraham)",
      xlab = "Years Relative to Master's Degree Completion")
legend("bottomright", legend = c("Log Nominal Income", "Log Real Income", "Log Hours", "Log Tenure"), col = 1:4, pch = 20)


# --- Table: Create a summary table for all models ---
model_list <- list(
  "(1) Log Nominal Income" = res_sunab_media,
  "(2) Log Real Income" = res_sunab_real,
  "(3) Log Contract. Hours" = res_sunab_horas,
  "(4) Log Job Tenure" = res_sunab_tempo
)

# This table will only show the main controls (FE)
# To see the dynamic coefficients, use summary(res_sunab_media), etc.
modelsummary(
  model_list,
  stars = TRUE,
  gof_map = c("nobs", "r.squared.within"),
  title = "Sun & Abraham (2020) Estimates of the Effect of a Master's Degree"
)


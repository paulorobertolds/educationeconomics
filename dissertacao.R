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

https://docs.google.com/spreadsheets/d/1y79jJyIc0TFAob4qipMketKjixuXub6T/edit?usp=drive_link&ouid=105674475394683857648&rtpof=true&sd=true


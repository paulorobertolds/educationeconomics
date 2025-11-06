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
  # ) %>%
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
res_twfe_real = feols(log(remuneracao_media_real) ~ i(time_to_treatment, ref = c(-1, -1000)) | cpf + ano_observacao, data=df)
summary(res_twfe_real)
summary(res_twfe)

# Compare the results
# Plot the two TWFE results
iplot(res_twfe)
iplot(res_twfe_real)


data("base_stagg")
head(base_stagg)
res_sa20 = feols(y ~ x1 + sunab(year_treated, year) | id + year, base_stagg)

res_sa20 = feols(log(remuneracao_media) ~ sunab(ano_conclusao, ano_observacao) | cpf + ano_observacao, data=df)
summary(res_sa20)

iplot(res_sa20)

# data("base_did")
# head(base_did)
# str(base_did$period)
# str(base_did$treat)
# 
# est_did = feols(y ~ x1 + i(period, treat, 5) | id + period, base_did)
# est_did
# 
# 
# str(df$dummie_situacao)
# df$ano_observacao = as.integer(df$ano_observacao)
# est_did = feols(log(remuneracao_media) ~ i(ano_observacao, dummie_situacao, 5) | cpf + ano_observacao, data=df)
# est_did

######## 2008 a 2022 ##########


# describe the df$ano_observacao variable
summary(df$ano_observacao)
#remove observations with ano_observacao < 2008 and > 2022 
df = read_xlsx("/home/freitas/Downloads/0research/nilson/outubro/5. dados_longos_grupos.xlsx")
df <- df %>%
  filter(!situação %in% c("matriculado"))
#"desistente","desligado"

df_1 = df %>%
  filter(ano_observacao >= 2008 & ano_observacao <= 2022)

# First, create the 'event_time' variable as before
df_1 <- df_1 %>%
  group_by(cpf) %>%
  mutate(
    event_time = if_else(!is.na(ano_conclusao), ano_observacao - ano_conclusao, NA_real_))
  # ) %>%
  # # Bin endpoints for a cleaner plot
  # mutate(
  #   event_time = case_when(
  #     event_time < -5 ~ -5,
  #     event_time > 5  ~ 5,
  #     TRUE ~ event_time
  #   )
  # )

# Bin endpoints for a cleaner plot
df_1 <- df_1 %>%
  group_by(cpf) %>%
  mutate(treated_group = max(mestre, na.rm = TRUE)) %>%
  ungroup()

# create time_to_treatement variable which is equal to event_time for treated units and -1000 for never treated
df_1 <- df_1 %>%
  mutate(time_to_treatment = if_else(treated_group == 1, event_time, -1000))


# TWFE DiD (note that the time to treatment for the never treated is -1000)
# (by using ref = c(-1, -1000) we exclude the period just before the treatment and 
# the never treated)
res_twfe = feols(log(remuneracao_media) ~ i(time_to_treatment, ref = c(-1, -1000)) | cpf + ano_observacao, data=df_1)
res_twfe_real = feols(log(remuneracao_media_real) ~ i(time_to_treatment, ref = c(-1, -1000)) | cpf + ano_observacao, data=df_1)
summary(res_twfe_real)
summary(res_twfe)

# Compare the results
# Plot the two TWFE results
iplot(res_twfe)
iplot(res_twfe_real)


data("base_stagg")
head(base_stagg)
res_sa20 = feols(y ~ x1 + sunab(year_treated, year) | id + year, base_stagg)

res_sa20 = feols(log(remuneracao_media) ~ sunab(ano_conclusao, ano_observacao) | cpf + ano_observacao, data=df_1)
summary(res_sa20)

iplot(res_sa20)

summary(df_1$ano_observacao)

######## 2009 a 2021 ##########


# describe the df$ano_observacao variable
summary(df$ano_observacao)
#remove observations with ano_observacao < 2009 and > 2021 
df = read_xlsx("/home/freitas/Downloads/0research/nilson/outubro/5. dados_longos_grupos.xlsx")
df <- df %>%
  filter(!situação %in% c("matriculado"))
#"desistente","desligado"

df_1 = df %>%
  filter(ano_observacao >= 2009 & ano_observacao <= 2021)

# First, create the 'event_time' variable as before
df_1 <- df_1 %>%
  group_by(cpf) %>%
  mutate(
    event_time = if_else(!is.na(ano_conclusao), ano_observacao - ano_conclusao, NA_real_)
  ) %>%
  # Bin endpoints for a cleaner plot
  mutate(
    event_time = case_when(
      event_time < -5 ~ -5,
      event_time > 5  ~ 5,
      TRUE ~ event_time
    )
  )

# Bin endpoints for a cleaner plot
df_1 <- df_1 %>%
  group_by(cpf) %>%
  mutate(treated_group = max(mestre, na.rm = TRUE)) %>%
  ungroup()

# create time_to_treatement variable which is equal to event_time for treated units and -1000 for never treated
df_1 <- df_1 %>%
  mutate(time_to_treatment = if_else(treated_group == 1, event_time, -1000))


# TWFE DiD (note that the time to treatment for the never treated is -1000)
# (by using ref = c(-1, -1000) we exclude the period just before the treatment and 
# the never treated)
res_twfe = feols(log(remuneracao_media) ~ i(time_to_treatment, ref = c(-1, -1000)) | cpf + ano_observacao, data=df_1)
res_twfe_real = feols(log(remuneracao_media_real) ~ i(time_to_treatment, ref = c(-1, -1000)) | cpf + ano_observacao, data=df_1)
summary(res_twfe_real)
summary(res_twfe)

# Compare the results
# Plot the two TWFE results
iplot(res_twfe)
iplot(res_twfe_real)


data("base_stagg")
head(base_stagg)
res_sa20 = feols(y ~ x1 + sunab(year_treated, year) | id + year, base_stagg)

res_sa20 = feols(log(remuneracao_media) ~ sunab(ano_conclusao, ano_observacao) | cpf + ano_observacao, data=df_1)
summary(res_sa20)

iplot(res_sa20)

summary(df_1$ano_observacao)

######## 2010 a 2020 ##########
rm(list=ls())

# describe the df$ano_observacao variable
summary(df$ano_observacao)
#remove observations with ano_observacao < 2010 and > 2020 
df = read_xlsx("/home/freitas/Downloads/0research/nilson/outubro/5. dados_longos_grupos.xlsx")
df <- df %>%
  filter(!situação %in% c("matriculado"))
#"desistente","desligado"

df_1 = df %>%
  filter(ano_observacao >= 2010 & ano_observacao <= 2020)

# First, create the 'event_time' variable as before
df_1 <- df_1 %>%
  group_by(cpf) %>%
  mutate(
    event_time = if_else(!is.na(ano_conclusao), ano_observacao - ano_conclusao, NA_real_)
  ) %>%
  # Bin endpoints for a cleaner plot
  mutate(
    event_time = case_when(
      event_time < -5 ~ -5,
      event_time > 5  ~ 5,
      TRUE ~ event_time
    )
  )

# Bin endpoints for a cleaner plot
df_1 <- df_1 %>%
  group_by(cpf) %>%
  mutate(treated_group = max(mestre, na.rm = TRUE)) %>%
  ungroup()

# create time_to_treatement variable which is equal to event_time for treated units and -1000 for never treated
df_1 <- df_1 %>%
  mutate(time_to_treatment = if_else(treated_group == 1, event_time, -1000))


# TWFE DiD (note that the time to treatment for the never treated is -1000)
# (by using ref = c(-1, -1000) we exclude the period just before the treatment and 
# the never treated)
res_twfe = feols(log(remuneracao_media) ~ i(time_to_treatment, ref = c(-1, -1000)) | cpf + ano_observacao, data=df_1)
res_twfe_real = feols(log(remuneracao_media_real) ~ i(time_to_treatment, ref = c(-1, -1000)) | cpf + ano_observacao, data=df_1)
summary(res_twfe_real)
summary(res_twfe)

# Compare the results
# Plot the two TWFE results
iplot(res_twfe)
iplot(res_twfe_real)


data("base_stagg")
head(base_stagg)
res_sa20 = feols(y ~ x1 + sunab(year_treated, year) | id + year, base_stagg)

res_sa20 = feols(log(remuneracao_media) ~ sunab(ano_conclusao, ano_observacao) | cpf + ano_observacao, data=df_1)
summary(res_sa20)

iplot(res_sa20)

summary(df_1$ano_observacao)


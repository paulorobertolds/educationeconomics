# Load necessary libraries
library(fixest)
library(dplyr)
library(ggplot2)
library(modelsummary)
library(readxl)

rm(list=ls())

#remove observations with ano_observacao < 2010 and > 2020 
df = read_xlsx("/home/freitas/Downloads/0research/nilson/outubro/5. dados_longos_grupos.xlsx")
df <- df %>%
  filter(!situação %in% c("matriculado"))
#"desistente","desligado"

df_1 = df %>%
  filter(ano_observacao >= 2012 & ano_observacao <= 2022)
summary(df_1$ano_observacao)
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



res_sa20 = feols(log(remuneracao_media) ~ sunab(ano_conclusao, ano_observacao) | cpf + ano_observacao, data=df_1)
summary(res_sa20)

iplot(res_sa20)



#remove title form plot and change the xlab and ylab
iplot(res_sa20, main = "", xlab = "Ano", ylab = "Estimativa do Log Remuneração Média e Int. Conf. 95%")

#save plot as jpg
jpeg("event_study_plot.jpg", width = 800, height = 600)  # Set your desired filename and size
iplot(res_sa20, main = "", xlab = "Ano", ylab = "Estimativa do Log Remuneração Média e Int. Conf. 95%")
dev.off()  # Always close the device!

library(fixest)
library(modelsummary)

# Assuming your variables:
# - 'event_time': relative time to treatment
# - 'gender': 1 = male, 0 = female
# - 'cpf': individual id
# - 'ano_observacao': period/year variable
# - 'remuneracao_media': outcome

# Estimate event-study for males
model_male <- feols(log(remuneracao_media_real) ~ sunab(ano_conclusao, event_time) | cpf + ano_observacao, data = subset(df_1, genero == 1))
iplot(model_male)
# Estimate event-study for females
model_female <- feols(log(remuneracao_media_real) ~ sunab(ano_conclusao, event_time) | cpf + ano_observacao, data = subset(df_1, genero == 2))
iplot(model_female)
# Compare results
modelsummary(list("Male" = model_male, "Female" = model_female), stars = TRUE)

# Robustness checks ###########################################################

# 1. Placebo test: Assign a fake treatment to never-treated and run DiD
set.seed(123)
df_placebo <- df_1
control_cpfs <- df_placebo %>% filter(treated_group == 0) %>% pull(cpf) %>% unique()
fake_treatment <- sample(2008:2022, length(control_cpfs), replace = TRUE)
placebo_map <- data.frame(cpf = control_cpfs, fake_conclusion = fake_treatment)
df_placebo <- left_join(df_placebo, placebo_map, by = "cpf")
df_placebo <- df_placebo %>%
  mutate(event_time_placebo = if_else(treated_group == 0, ano_observacao - fake_conclusion, event_time),
         time_to_treatment_placebo = if_else(treated_group == 0, event_time_placebo, time_to_treatment))

placebo_res = feols(log(remuneracao_media) ~ i(time_to_treatment_placebo, ref = c(-1, -1000)) | cpf + ano_observacao, data = df_placebo)
summary(placebo_res)

# 2. Alternative outcome: Use levels instead of logs
res_level = feols(remuneracao_media ~ i(time_to_treatment, ref = c(-1, -1000)) | cpf + ano_observacao, data = df_1)
summary(res_level)

# 3. Add covariates (assuming the variables exist, e.g., gender, age, etc)
if(all(c("sexo", "idade") %in% names(df_1))) {
  res_covars = feols(log(remuneracao_media) ~ i(time_to_treatment, ref = c(-1, -1000)) + sexo + idade | cpf + ano_observacao, data = df_1)
  summary(res_covars)
}

# 4. Subsample: Only men (assuming "sexo" == "M" for men)
if("sexo" %in% names(df_1)) {
  res_men = feols(log(remuneracao_media) ~ i(time_to_treatment, ref = c(-1, -1000)) | cpf + ano_observacao, data = filter(df_1, sexo == "M"))
  summary(res_men)
}

# 5. Wider event time bins (e.g., pre=-5 or less, post=5 or more)
df_1 <- df_1 %>%
  mutate(event_time_bin = case_when(
    event_time < -5 ~ -5,
    event_time > 5 ~ 5,
    TRUE ~ event_time
  ),
  time_to_treatment_bin = if_else(treated_group == 1, event_time_bin, -1000)
  )
res_binned = feols(log(remuneracao_media) ~ i(time_to_treatment_bin, ref = c(-1, -1000)) | cpf + ano_observacao, data=df_1)
summary(res_binned)

# 6. Clustered at different levels (e.g., by cohort/year)
res_cluster = feols(log(remuneracao_media) ~ i(time_to_treatment, ref = c(-1, -1000)) | cpf + ano_observacao, cluster = ~ano_observacao, data = df_1)
summary(res_cluster)


##############################
# Placebo check for event-study using sunab()
set.seed(123)  # for replicability

# Assume df_1 as your main sample, adjust if needed for the window
controls <- df_1 %>% filter(treated_group == 0) %>% pull(cpf) %>% unique()
fake_treat_years <- sample(df_1$ano_observacao, length(controls), replace = TRUE)
placebo_table <- data.frame(cpf = controls, ano_conclusao_placebo = fake_treat_years)

# Merge placebo (fake treatment years) into main dataframe
df_placebo <- left_join(df_1, placebo_table, by = "cpf") %>%
  mutate(ano_conclusao_placebo = ifelse(treated_group == 0, ano_conclusao_placebo, ano_conclusao))

# Run placebo event-study using sunab()
res_sa20_placebo = feols(log(remuneracao_media) ~ sunab(ano_conclusao_placebo, ano_observacao) | cpf + ano_observacao, data = df_placebo)
summary(res_sa20_placebo)
iplot(res_sa20_placebo)  # The dynamic estimates should be flat

# For remuneration_media_real
res_sa20_real_placebo = feols(log(remuneracao_media_real) ~ sunab(ano_conclusao_placebo, ano_observacao) | cpf + ano_observacao, data = df_placebo)
summary(res_sa20_real_placebo)
iplot(res_sa20_real_placebo)

# End of robustness checks ####################################################

# Install the package (if needed):
# install.packages("did")
# install.packages("data.table")
rm(list=ls())
library(did)
library(data.table)
library(readr)

# 1. Load your data
df = read_xlsx("/home/freitas/Downloads/0research/nilson/outubro/5. dados_longos_grupos.xlsx")
df <- df %>%
  filter(!situação %in% c("matriculado"))
#"desistente","desligado"

df_1 = df %>%
  filter(ano_observacao >= 2012 & ano_observacao <= 2022)
summary(df_1$ano_observacao)
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

# --- Install packages if not already installed ---
# install.packages("dplyr")
# install.packages("readr")
# install.packages("did")

library(dplyr)
library(readr)
library(did)


# 2. CREATE 'first_mestre' (first treated year per cpf)
df_1 <- df_1%>%
  group_by(cpf) %>%
  mutate(
    first_mestre = ifelse(
      any(mestre == 1),
      min(ano_observacao[mestre == 1], na.rm = TRUE),
      0
    )
  ) %>%
  ungroup()

# 3. CREATE THE 'G' VARIABLE (group: year first treated, 0 if never treated)
df_1 <- df_1 %>%
  mutate(
    G = ifelse(first_mestre > 0, first_mestre, 0)
  )

df_1 <- df_1 %>%
  mutate(cpf = as.numeric(as.character(cpf)))

# 4. ESTIMATION WITH CALLAWAY & SANT'ANNA (did package)
cs_did <- att_gt(
  yname = "remuneracao_media_real",
  tname = "ano_observacao",
  idname = "cpf",
  gname = "G",
  data = df_1,
  panel = TRUE,
  control_group = "nevertreated"   # <--- CORRECT!
)

# 5. AGGREGATE ATT (overall ATE and/or dynamic/event-study)
agg_group <- aggte(cs_did, type = "group")
agg_dyn   <- aggte(cs_did, type = "dynamic")

# 6. RESULTS
summary(agg_group)
summary(agg_dyn)
# For dynamic (event study) plot
#plot(agg_dyn, type = "dynamic")

#plot(agg_dyn) # event-time plot

library(ggplot2)
event_time <- agg_dyn$egt
att <- agg_dyn$att.egt
se <- agg_dyn$se.egt
ggplot(data = data.frame(event_time, att, se), aes(x = event_time, y = att)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = att - 1.96*se, ymax = att + 1.96*se), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Event Time", y = "ATT", title = "Dynamic Treatment Effects (Event Study)")

# --- END SCRIPT ---

######## Log of remuneration_media ##########

rm(list=ls())
library(did)
library(data.table)
library(readr)

# 1. Load your data
df = read_xlsx("/home/freitas/Downloads/0research/nilson/outubro/5. dados_longos_grupos.xlsx")
df <- df %>%
  filter(!situação %in% c("matriculado"))
#"desistente","desligado"

df_1 = df %>%
  filter(ano_observacao >= 2012 & ano_observacao <= 2022)
summary(df_1$ano_observacao)
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

# --- Install packages if not already installed ---
# install.packages("dplyr")
# install.packages("readr")
# install.packages("did")

library(dplyr)
library(readr)
library(did)


# 2. CREATE 'first_mestre' (first treated year per cpf)
df_1 <- df_1%>%
  group_by(cpf) %>%
  mutate(
    first_mestre = ifelse(
      any(mestre == 1),
      min(ano_observacao[mestre == 1], na.rm = TRUE),
      0
    )
  ) %>%
  ungroup()

# 3. CREATE THE 'G' VARIABLE (group: year first treated, 0 if never treated)
df_1 <- df_1 %>%
  mutate(
    G = ifelse(first_mestre > 0, first_mestre, 0)
  )

df_1 <- df_1 %>%
  mutate(cpf = as.numeric(as.character(cpf)))

#create new variable log of remuneracao_media
df_1["log_remuneracao_media"] <- log(df_1$remuneracao_media)


df_1 <- df_1 %>%
  mutate(
    log_remuneracao_media = log(remuneracao_media)
  ) %>%
  filter(!is.na(log_remuneracao_media), is.finite(log_remuneracao_media))

table(df_1$G, useNA = "ifany")

# 4. ESTIMATION WITH CALLAWAY & SANT'ANNA (did package)
cs_did <- att_gt(
  yname = "log_remuneracao_media",
  tname = "ano_observacao",
  idname = "cpf",
  gname = "G",
  data = df_1,
  panel = TRUE,
  control_group = "nevertreated"   # <--- CORRECT!
)

# 5. AGGREGATE ATT (overall ATE and/or dynamic/event-study)
agg_group <- aggte(cs_did, type = "group")
agg_dyn   <- aggte(cs_did, type = "dynamic")

# 6. RESULTS
summary(agg_group)
summary(agg_dyn)
# For dynamic (event study) plot
#plot(agg_dyn, type = "dynamic")

#plot(agg_dyn) # event-time plot

library(ggplot2)
event_time <- agg_dyn$egt
att <- agg_dyn$att.egt
se <- agg_dyn$se.egt
ggplot(data = data.frame(event_time, att, se), aes(x = event_time, y = att)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = att - 1.96*se, ymax = att + 1.96*se), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Event Time", y = "ATT", title = "Dynamic Treatment Effects (Event Study)")

################### --- END SCRIPT ---





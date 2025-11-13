
##### Final
# Load necessary libraries
library(fixest)
library(dplyr)
library(ggplot2)
library(modelsummary)
library(readxl)

rm(list=ls())

# Load and prepare data
df = read_xlsx("/home/freitas/Downloads/0research/nilson/outubro/5. dados_longos_grupos.xlsx")
df <- df %>%
  filter(!situação %in% c("matriculado", "matriculada"))

df_1 = df %>%
  filter(ano_observacao >= 2012 & ano_observacao <= 2022)

# Create event study variables
df_1 <- df_1 %>%
  group_by(cpf) %>%
  mutate(
    event_time = if_else(!is.na(ano_conclusao), ano_observacao - ano_conclusao, NA_real_),
    treated_group = max(mestre, na.rm = TRUE),
    time_to_treatment = if_else(treated_group == 1, event_time, -1000)
  ) %>%
  ungroup()

# Convert relevant variables to appropriate types
df_1 <- df_1 %>%
  mutate(
    # Create male dummy variable (1 = male, 0 = female)
    male = if_else(genero == 1, 1, 0),
    natureza_juridica = as.factor(natureza_juridica),
    cbo_2002_grupo = as.factor(cbo_2002_grupo),
    natureza_juridica_grupo = as.factor(natureza_juridica_grupo),
    # Convert tempo_emprego to numeric (removing commas)
    tempo_emprego = as.numeric(gsub(",", ".", gsub("\\.", "", tempo_emprego))),
    # Convert horas_contratuais to numeric
    horas_contratuais = as.numeric(gsub(",", ".", gsub("\\.", "", horas_contratuais)))
  )

# 1. MAIN SUN & ABRAHAM SPECIFICATION WITH CONTROLS
res_sa20_basic = feols(log(remuneracao_media_real) ~ sunab(ano_conclusao, ano_observacao) | 
                         cpf + ano_observacao, data = df_1, cluster = ~cpf)

# 2. WITH DEMOGRAPHIC CONTROLS (only male dummy now)
res_sa20_demographic = feols(log(remuneracao_media_real) ~ sunab(ano_conclusao, ano_observacao) + 
                               male | cpf + ano_observacao, data = df_1, cluster = ~cpf)

# 3. WITH JOB CHARACTERISTICS CONTROLS
res_sa20_job = feols(log(remuneracao_media_real) ~ sunab(ano_conclusao, ano_observacao) + 
                       natureza_juridica_grupo + cbo_2002_grupo + log(horas_contratuais + 1) | 
                       cpf + ano_observacao, data = df_1, cluster = ~cpf)

# 4. FULL SPECIFICATION WITH ALL CONTROLS
res_sa20_full = feols(log(remuneracao_media_real) ~ sunab(ano_conclusao, ano_observacao) + 
                        male + natureza_juridica_grupo + cbo_2002_grupo + 
                        log(horas_contratuais + 1) + tempo_emprego | cpf + ano_observacao, data = df_1, cluster = ~cpf)

# You already created this variable, use it!
# time_to_treatment = if_else(treated_group == 1, event_time, -1000)


# 5. ALTERNATIVE SPECIFICATION: INTERACTION WITH INSTITUTIONAL CHARACTERISTICS
# Interaction with public/private sector
res_sa20_public = feols(log(remuneracao_media_real) ~ sunab(ano_conclusao, ano_observacao):natureza_juridica_grupo + 
                          male + cbo_2002_grupo + log(horas_contratuais + 1) | 
                          cpf + ano_observacao, data = df_1, cluster = ~cpf)

# 6. HETEROGENEOUS EFFECTS BY SUBGROUPS
# By gender (using the male dummy)
res_by_gender = feols(log(remuneracao_media_real) ~ sunab(ano_conclusao, ano_observacao) + 
                        natureza_juridica_grupo + cbo_2002_grupo + log(horas_contratuais + 1) | 
                        cpf + ano_observacao, data = df_1, split = ~male, cluster = ~cpf)

# By occupation group
res_by_occupation = feols(log(remuneracao_media_real) ~ sunab(ano_conclusao, ano_observacao) + 
                            male + natureza_juridica_grupo + log(horas_contratuais + 1) | 
                            cpf + ano_observacao, data = df_1, split = ~cbo_2002_grupo, cluster = ~cpf)

# 7. DYNAMIC EFFECTS WITH EVENT STUDY - Extended version with controls
df_1 <- df_1 %>%
  mutate(
    event_time_binned = case_when(
      event_time < -5 ~ -5,
      event_time > 5 ~ 5,
      TRUE ~ event_time
    )
  )

res_dynamic_controls = feols(log(remuneracao_media_real) ~ i(event_time_binned, ref = -1) + 
                               male + natureza_juridica_grupo + cbo_2002_grupo + 
                               log(horas_contratuais + 1) | cpf + ano_observacao, data = df_1, cluster = ~cpf)

# 8. ROBUSTNESS CHECKS WITH DIFFERENT CONTROL SETS
# Only individual fixed effects
res_robust_fe = feols(log(remuneracao_media_real) ~ sunab(ano_conclusao, ano_observacao) | 
                        cpf, data = df_1, cluster = ~cpf)

# With sector-specific trends
res_robust_sector = feols(log(remuneracao_media_real) ~ sunab(ano_conclusao, ano_observacao) + 
                            male + cbo_2002_grupo + log(horas_contratuais + 1) | 
                            cpf + ano_observacao + natureza_juridica_grupo, data = df_1, cluster = ~cpf)

# 9. MECHANISMS ANALYSIS - Additional outcome variables
# Effect on hours worked
if("horas_contratuais" %in% names(df_1)) {
  res_hours = feols(log(horas_contratuais + 1) ~ sunab(ano_conclusao, ano_observacao) + 
                      male + natureza_juridica_grupo + cbo_2002_grupo | 
                      cpf + ano_observacao, data = df_1, cluster = ~cpf)
}

# Effect on job stability (tenure)
if("tempo_emprego" %in% names(df_1)) {
  res_tenure = feols(tempo_emprego ~ sunab(ano_conclusao, ano_observacao) + 
                       male + natureza_juridica_grupo + cbo_2002_grupo | 
                       cpf + ano_observacao, data = df_1, cluster = ~cpf)
}

# 10. SUMMARY OF RESULTS
models_summary = list(
  "Basic SA" = res_sa20_basic,
  "+ Male" = res_sa20_demographic,
  "+ Job Chars" = res_sa20_job,
  "Full Controls" = res_sa20_full,
  "Dynamic w/Controls" = res_dynamic_controls
)


# Create comprehensive regression table
modelsummary(models_summary, 
             stars = TRUE,
             output = "markdown",
             coef_rename = function(x) gsub(".*::", "", x),
             gof_map = c("nobs", "r.squared", "FE: cpf", "FE: ano_observacao"))

# 11. PLOTTING RESULTS
# Main event study plot
iplot(res_sa20_full, 
      main = "Dynamic Effects of Master's Degree Completion on Real Wages\n(Full Controls Specification)",
      xlab = "Years Relative to Degree Completion",
      ylab = "Effect on Log Real Wage")

# Plot with confidence intervals for dynamic specification
plot_dynamic_data = broom::tidy(res_dynamic_controls, conf.int = TRUE) %>%
  filter(grepl("event_time_binned", term)) %>%
  mutate(time = as.numeric(gsub(".*::", "", term)))

ggplot(plot_dynamic_data, aes(x = time, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = -1, linetype = "dashed", color = "blue") +
  labs(title = "Event Study: Treatment Effects on Real Wages",
       subtitle = "With Demographic and Job Characteristic Controls",
       x = "Years Relative to Master's Degree Completion",
       y = "Treatment Effect (Log Points)") +
  theme_minimal()

# 12. HETEROGENEITY PLOTS
# Plot by gender subgroups
iplot(res_by_gender, 
      main = "Treatment Effects by Gender (Male = 1, Female = 0)",
      xlab = "Years Relative to Treatment")
#add labels pch for female ball and for mael triangle
legend("topleft", legend = c("Female", "Male"), col = c("blue", "red"), pch = c(16, 17))

# 13. DESCRIPTIVE STATISTICS
cat("=== DESCRIPTIVE STATISTICS ===\n")
cat("Sample period:", min(df_1$ano_observacao, na.rm = TRUE), "-", max(df_1$ano_observacao, na.rm = TRUE), "\n")
cat("Number of individuals:", n_distinct(df_1$cpf), "\n")
cat("Number of treated individuals:", sum(df_1$treated_group == 1, na.rm = TRUE), "\n")
cat("Number of control individuals:", sum(df_1$treated_group == 0, na.rm = TRUE), "\n")
cat("\nWage distribution (real):\n")
print(summary(df_1$remuneracao_media_real))

# Distribution of key variables
cat("\n=== VARIABLE DISTRIBUTIONS ===\n")
cat("Gender distribution (male = 1, female = 0):\n")
print(table(df_1$male))
cat("\nSector distribution:\n")
print(table(df_1$natureza_juridica_grupo))
cat("\nOccupation distribution:\n")
print(table(df_1$cbo_2002_grupo))

# 14. BALANCE CHECK (Pre-treatment characteristics)
# Compare treated vs control in pre-treatment period
pre_treatment <- df_1 %>% 
  filter(event_time < 0 | treated_group == 0) %>%
  group_by(treated_group) %>%
  summarise(
    n_obs = n(),
    n_individuals = n_distinct(cpf),
    mean_wage = mean(remuneracao_media_real, na.rm = TRUE),
    mean_hours = mean(horas_contratuais, na.rm = TRUE),
    mean_tenure = mean(tempo_emprego, na.rm = TRUE),
    prop_male = mean(male, na.rm = TRUE)
  )

cat("\n=== BALANCE CHECK (Pre-treatment) ===\n")
print(pre_treatment)

# 15. EXPORT RESULTS
write.csv(broom::tidy(res_sa20_full), "sun_abraham_full_results.csv", row.names = FALSE)
write.csv(pre_treatment, "balance_check.csv", row.names = FALSE)

# Save workspace for further analysis
save.image("event_study_analysis_complete.RData")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Results exported to:\n")
cat("- sun_abraham_full_results.csv\n")
cat("- balance_check.csv\n")
cat("- event_study_analysis_complete.RData\n")



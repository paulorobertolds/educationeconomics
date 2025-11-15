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
# df_1 <- df_1 %>%
#   group_by(cpf) %>%
#   mutate(
#     event_time = if_else(!is.na(ano_conclusao), ano_observacao - ano_conclusao, NA_real_),
#     treated_group = max(mestre, na.rm = TRUE),
#     time_to_treatment = if_else(treated_group == 1, event_time, -1000)
#   ) %>%
#   ungroup()

df_1 <- df_1 %>%
  group_by(cpf) %>%
  mutate(
    event_time = if_else(!is.na(ano_conclusao), ano_observacao - ano_conclusao, NA_real_),
    # Bin event_time to -5 to 5 for cleaner analysis
    event_time = case_when(
      event_time < -5 ~ -5,
      event_time > 5  ~ 5,
      TRUE ~ event_time
    ),
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


####
# 7. DYNAMIC EFFECTS WITH EVENT STUDY - Sun and Abraham method
# Use the original treatment timing (ano_conclusao) for Sun & Abraham
res_dynamic_sa = feols(log(remuneracao_media_real) ~ sunab(ano_conclusao, ano_observacao) + 
                         male + natureza_juridica_grupo + cbo_2002_grupo + 
                         log(horas_contratuais + 1) | cpf + ano_observacao, 
                       data = df_1, cluster = ~cpf)

# Plot the Sun and Abraham results using iplot (built-in)
iplot(res_dynamic_sa, 
      main = "Dynamic Effects: Sun & Abraham Method",
      xlab = "Years Relative to Master's Degree Completion",
      ylab = "Effect on Log Real Wage")

# For custom plotting with binned event times, use this approach:
# Extract coefficients and manually create binned plot
sa_coefs = broom::tidy(res_dynamic_sa, conf.int = TRUE)

# Filter for the interaction terms (relative time coefficients)
sa_plot_data = sa_coefs %>%
  filter(grepl("::", term)) %>%
  mutate(
    # Extract the relative time from the term
    time = as.numeric(gsub(".*::", "", term))
  ) %>%
  # Bin the time values for plotting (-5 to 5)
  mutate(
    time_binned = case_when(
      time < -5 ~ -5,
      time > 5 ~ 5,
      TRUE ~ time
    )
  ) %>%
  # Group by binned time and calculate average effects
  group_by(time_binned) %>%
  summarise(
    estimate = mean(estimate, na.rm = TRUE),
    conf.low = mean(conf.low, na.rm = TRUE),
    conf.high = mean(conf.high, na.rm = TRUE),
    n_coefs = n(),  # Number of coefficients in each bin
    .groups = 'drop'
  )

# Plot the binned Sun & Abraham results
ggplot(sa_plot_data, aes(x = time_binned, y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = -1, linetype = "dashed", color = "blue") +
  labs(title = "Event Study: Treatment Effects on Real Wages",
       subtitle = "Sun & Abraham Method (Binned -5 to 5)",
       x = "Years Relative to Master's Degree Completion",
       y = "Treatment Effect (Log Points)") +
  theme_minimal()

# Alternative: Use the newer fixest aggregation syntax
# First check if the newer version of aggregate works
tryCatch({
  # Newer syntax for aggregate
  res_dynamic_sa_agg = aggregate(res_dynamic_sa, ~time, "::(-?[0-9]+)$")
  
  # Convert to data frame for plotting
  agg_plot_data = as.data.frame(res_dynamic_sa_agg)
  agg_plot_data$time = as.numeric(rownames(agg_plot_data))
  
  # Bin the aggregated results
  agg_plot_data_binned = agg_plot_data %>%
    mutate(
      time_binned = case_when(
        time < -5 ~ -5,
        time > 5 ~ 5,
        TRUE ~ time
      )
    ) %>%
    group_by(time_binned) %>%
    summarise(
      estimate = mean(Estimate, na.rm = TRUE),
      std.error = mean(Std..Error, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error
    )
  
  # Plot the alternative aggregated results
  ggplot(agg_plot_data_binned, aes(x = time_binned, y = estimate)) +
    geom_point(size = 2, color = "blue") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "blue") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_vline(xintercept = -1, linetype = "dashed", color = "blue") +
    labs(title = "Event Study: Sun & Abraham (Alternative Aggregation)",
         x = "Years Relative to Master's Degree Completion",
         y = "Treatment Effect (Log Points)") +
    theme_minimal()
  
}, error = function(e) {
  message("Alternative aggregation method failed: ", e$message)
  message("Using manual coefficient extraction instead.")
})

# For comparison: Traditional TWFE with binned event times
df_1 <- df_1 %>%
  mutate(
    event_time_binned = case_when(
      event_time < -5 ~ -5,
      event_time > 5 ~ 5,
      TRUE ~ event_time
    )
  )

res_dynamic_twfe = feols(log(remuneracao_media_real) ~ i(event_time_binned, ref = -1) + 
                           male + natureza_juridica_grupo + cbo_2002_grupo + 
                           log(horas_contratuais + 1) | cpf + ano_observacao, 
                         data = df_1, cluster = ~cpf)

# Compare both methods using iplot
iplot(list("Sun & Abraham" = res_dynamic_sa, "TWFE" = res_dynamic_twfe), 
      main = "Comparison: Sun & Abraham vs TWFE",
      xlab = "Years Relative to Treatment")

# Also create a side-by-side comparison plot manually
twfe_plot_data = broom::tidy(res_dynamic_twfe, conf.int = TRUE) %>%
  filter(grepl("event_time_binned", term)) %>%
  mutate(
    time = as.numeric(gsub(".*::", "", term)),
    method = "TWFE"
  )

comparison_plot_data = bind_rows(
  sa_plot_data %>% mutate(method = "Sun & Abraham") %>% rename(time = time_binned),
  twfe_plot_data %>% select(time, estimate, conf.low, conf.high, method)
)

ggplot(comparison_plot_data, aes(x = time, y = estimate, color = method)) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, position = position_dodge(width = 0.3)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = -1, linetype = "dashed", color = "blue") +
  labs(title = "Comparison: Sun & Abraham vs TWFE Methods",
       subtitle = "Treatment Effects on Real Wages",
       x = "Years Relative to Master's Degree Completion",
       y = "Treatment Effect (Log Points)",
       color = "Method") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")


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

#change the cbo_2002_grupo to professor and outros
df_2 <- df_1 %>%
  mutate(
    cbo_2002_grupo = case_when(
      cbo_2002_grupo %in% c("Outros", "Gestores", "Administrativos") ~ "Outros",
      TRUE ~ "Professor"
    )
  )
print(table(df_2$cbo_2002_grupo))



# 7. DYNAMIC EFFECTS WITH EVENT STUDY - Sun and Abraham method
# Use the original treatment timing (ano_conclusao) for Sun & Abraham
res_dynamic_sa = feols(log(remuneracao_media_real) ~ sunab(ano_conclusao, ano_observacao) + 
                         male + natureza_juridica_grupo + cbo_2002_grupo + 
                         log(horas_contratuais + 1) | cpf + ano_observacao, 
                       data = df_1, cluster = ~cpf)

# Plot the Sun and Abraham results using iplot (built-in)
iplot(res_dynamic_sa, 
      main = "Dynamic Effects: Sun & Abraham Method",
      xlab = "Years Relative to Master's Degree Completion",
      ylab = "Effect on Log Real Wage")

# For custom plotting with binned event times, use this approach:
# Extract coefficients and manually create binned plot
sa_coefs = broom::tidy(res_dynamic_sa, conf.int = TRUE)

# Filter for the interaction terms (relative time coefficients)
sa_plot_data = sa_coefs %>%
  filter(grepl("::", term)) %>%
  mutate(
    # Extract the relative time from the term
    time = as.numeric(gsub(".*::", "", term))
  ) %>%
  # Bin the time values for plotting (-5 to 5)
  mutate(
    time_binned = case_when(
      time < -5 ~ -5,
      time > 5 ~ 5,
      TRUE ~ time
    )
  ) %>%
  # Group by binned time and calculate average effects
  group_by(time_binned) %>%
  summarise(
    estimate = mean(estimate, na.rm = TRUE),
    conf.low = mean(conf.low, na.rm = TRUE),
    conf.high = mean(conf.high, na.rm = TRUE),
    n_coefs = n(),  # Number of coefficients in each bin
    .groups = 'drop'
  )

# Plot the binned Sun & Abraham results
ggplot(sa_plot_data, aes(x = time_binned, y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = -1, linetype = "dashed", color = "blue") +  # Changed from -1 to 0
  labs(title = "Estudo de Evento: Efeitos do Tratamento nos Salários Reais",
       subtitle = "Método Sun & Abraham (Agrupado -5 a 5)",
       x = "Anos Relativos à Conclusão do Mestrado",
       y = "Efeito do Tratamento (Pontos Log)") +
  scale_x_continuous(
    breaks = -5:5,  # Keep the same break positions
    labels = -4:6   # Shift labels by +1: -5 becomes -4, -4 becomes -3, ..., 5 becomes 6
  ) +
  theme_minimal()

##
# 8. HETEROGENEITY 
iplot(res_dynamic_sa_by_gender, 
      main = "Treatment Effects by Gender\nSun & Abraham Method",
      xlab = "Years Relative to Master's Degree Completion",
      ylab = "Treatment Effect (Log Points)",
      col = c("#E41A1C", "#377EB8"),  # Custom colors
      pt.pch = c(16, 17),             # Different point shapes
      pt.cex = 1.2)                   # Larger points

# Add custom legend
legend("topleft", 
       legend = c("Female", "Male"), 
       col = c("#E41A1C", "#377EB8"), 
       pch = c(16, 17),
       pt.cex = 1.2,
       bty = "n")

# For custom plotting with binned event times - IMPROVED VISUALIZATION
# Extract coefficients from the split models using INTEGER indexing
print(paste("Sample size for group 1:", nobs(res_dynamic_sa_by_gender[[1]])))
print(paste("Sample size for group 2:", nobs(res_dynamic_sa_by_gender[[2]])))
table(df_1$male)

# Use integer indexing
sa_coefs_group1 = broom::tidy(res_dynamic_sa_by_gender[[1]], conf.int = TRUE)
sa_coefs_group2 = broom::tidy(res_dynamic_sa_by_gender[[2]], conf.int = TRUE)

# Determine which group is which based on your data
sa_coefs_female = sa_coefs_group1
sa_coefs_male = sa_coefs_group2

# Process coefficients for both genders
process_gender_coefs <- function(coefs, gender_name) {
  coefs %>%
    filter(grepl("::", term)) %>%
    mutate(
      time = as.numeric(gsub(".*::", "", term)),
      time_binned = case_when(
        time < -5 ~ -5,
        time > 5 ~ 5,
        TRUE ~ time
      )
    ) %>%
    group_by(time_binned) %>%
    summarise(
      estimate = mean(estimate, na.rm = TRUE),
      conf.low = mean(conf.low, na.rm = TRUE),
      conf.high = mean(conf.high, na.rm = TRUE),
      n_coefs = n(),
      .groups = 'drop'
    ) %>%
    mutate(gender = gender_name)
}

sa_plot_data_female = process_gender_coefs(sa_coefs_female, "Mulheres")
sa_plot_data_male = process_gender_coefs(sa_coefs_male, "Homens")

# Combine both gender datasets
sa_plot_data_gender = bind_rows(sa_plot_data_female, sa_plot_data_male)

# OPTION 2: OVERLAID PLOT WITH BETTER SEPARATION

ggplot(sa_plot_data_gender, aes(x = time_binned, y = estimate, color = gender, shape = gender)) +
  geom_point(size = 3, position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.3, position = position_dodge(width = 0.3)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = -1, linetype = "dashed", color = "blue") +
  labs(title = "Estudo de Evento: Efeitos do Tratamento nos Salários Reais por Gênero",
       subtitle = "Método Sun & Abraham (Agrupado -5 a 5)",
       x = "Anos Relativos à Conclusão do Mestrado",
       y = "Efeito do Tratamento (Pontos Log)",
       color = "Gênero", shape = "Gênero") +
  scale_color_manual(values = c("Mulheres" = "#E41A1C", "Homens" = "#377EB8")) +
  scale_shape_manual(values = c("Mulheres" = 16, "Homens" = 17)) +
  scale_x_continuous(
    breaks = -5:5,
    labels = -4:6
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.box.background = element_blank()
  )


# Print summary statistics for interpretation
cat("\n=== SUMMARY STATISTICS BY GENDER ===\n")
gender_summary = sa_plot_data_gender %>%
  group_by(gender) %>%
  summarise(
    mean_effect = mean(estimate, na.rm = TRUE),
    max_effect = max(estimate, na.rm = TRUE),
    min_effect = min(estimate, na.rm = TRUE),
    pos_effects = sum(estimate > 0, na.rm = TRUE),
    total_periods = n()
  )
print(gender_summary)



####
# Create occupation groups
df_2 <- df_1 %>%
  mutate(
    cbo_2002_grupo = case_when(
      cbo_2002_grupo %in% c("Outros", "Gestores", "Administrativos") ~ "Outros",
      TRUE ~ "Professor"
    )
  )

# Run Sun & Abraham model by occupation
res_by_ocupacao = feols(log(remuneracao_media_real) ~ sunab(ano_conclusao, ano_observacao) + 
                          male + natureza_juridica_grupo + log(horas_contratuais + 1) | 
                          cpf + ano_observacao, data = df_2, split = ~cbo_2002_grupo, cluster = ~cpf)

# OPTION 1: Built-in iplot with custom styling
iplot(res_by_ocupacao, 
      main = "Treatment Effects by Occupation\nSun & Abraham Method",
      xlab = "Years Relative to Master's Degree Completion",
      ylab = "Treatment Effect (Log Points)",
      col = c("#4DAF4A", "#984EA3"),  # Custom colors for occupation
      pt.pch = c(15, 18),             # Different point shapes (square and diamond)
      pt.cex = 1.2)                   # Larger points

# Add custom legend
legend("topleft", 
       legend = c("Outros", "Professor"), 
       col = c("#4DAF4A", "#984EA3"), 
       pch = c(15, 18),
       pt.cex = 1.2,
       bty = "n")



# OPTION 2: Custom plotting with binned event times
# Extract coefficients from the split models using INTEGER indexing
print(paste("Sample size for group 1:", nobs(res_by_ocupacao[[1]])))
print(paste("Sample size for group 2:", nobs(res_by_ocupacao[[2]])))
table(df_2$cbo_2002_grupo)

# Use integer indexing
ocupacao_coefs_group1 = broom::tidy(res_by_ocupacao[[1]], conf.int = TRUE)
ocupacao_coefs_group2 = broom::tidy(res_by_ocupacao[[2]], conf.int = TRUE)

# Determine which group is which based on your data
# Check which group corresponds to Professor and which to Outros
ocupacao_coefs_professor = ocupacao_coefs_group1
ocupacao_coefs_outros = ocupacao_coefs_group2

# Process coefficients for both occupation groups
process_ocupacao_coefs <- function(coefs, ocupacao_name) {
  coefs %>%
    filter(grepl("::", term)) %>%
    mutate(
      time = as.numeric(gsub(".*::", "", term)),
      time_binned = case_when(
        time < -5 ~ -5,
        time > 5 ~ 5,
        TRUE ~ time
      )
    ) %>%
    group_by(time_binned) %>%
    summarise(
      estimate = mean(estimate, na.rm = TRUE),
      conf.low = mean(conf.low, na.rm = TRUE),
      conf.high = mean(conf.high, na.rm = TRUE),
      n_coefs = n(),
      .groups = 'drop'
    ) %>%
    mutate(occupation = ocupacao_name)
}

ocupacao_plot_data_professor = process_ocupacao_coefs(ocupacao_coefs_professor, "Outros")
ocupacao_plot_data_outros = process_ocupacao_coefs(ocupacao_coefs_outros, "Professor")

# Combine both occupation datasets
ocupacao_plot_data = bind_rows(ocupacao_plot_data_professor, ocupacao_plot_data_outros)

# OPTION 2A with different legend styling:

ocupacao_plot_data_shifted <- ocupacao_plot_data %>%
  mutate(time_binned_shifted = time_binned + 1)

ggplot(ocupacao_plot_data_shifted, aes(x = time_binned_shifted, y = estimate, color = occupation, shape = occupation)) +
  geom_point(size = 3, position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.3, position = position_dodge(width = 0.3)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "Estudo de Evento: Efeitos do Tratamento nos Salários Reais por Ocupação",
       subtitle = "Método Sun & Abraham (Agrupado -4 a 6)",
       x = "Anos Relativos à Conclusão do Mestrado",
       y = "Efeito do Tratamento (Pontos Log)",
       color = "Ocupação", shape = "Ocupação") +
  scale_color_manual(values = c("Outros" = "#4DAF4A", "Professor" = "black")) +
  scale_shape_manual(values = c("Outros" = 15, "Professor" = 18)) +
  scale_x_continuous(breaks = -4:6) +
  theme_minimal() +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.box.background = element_blank()
  )

# Print summary statistics for interpretation
cat("\n=== SUMMARY STATISTICS BY OCCUPATION ===\n")
ocupacao_summary = ocupacao_plot_data %>%
  group_by(occupation) %>%
  summarise(
    mean_effect = mean(estimate, na.rm = TRUE),
    max_effect = max(estimate, na.rm = TRUE),
    min_effect = min(estimate, na.rm = TRUE),
    pos_effects = sum(estimate > 0, na.rm = TRUE),
    total_periods = n()
  )
print(ocupacao_summary)



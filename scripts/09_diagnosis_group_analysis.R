
# ============================================================
# Script 09: Diagnosis Group and 30-Day Readmission
# Project: Hospital Readmission RWE
# Purpose: Assess whether diagnosis group adds explanatory
#          value to the adjusted readmission model.
# Data: Synthetic hospital admissions dataset
# ============================================================

library(tidyverse)

# Load the cleaned working dataset
hospital_data <- read_csv(
  "data/clean/hospital_admissions_working.csv",
  show_col_types = FALSE
)

# ------------------------------------------------------------
# Step 1: Prepare the eligible analysis dataset
# ------------------------------------------------------------

diagnosis_model_data <- hospital_data |>
  filter(eligible_index == TRUE) |>
  select(
    patient_id,
    readmission_30d,
    admission_type,
    diagnosis_group,
    age,
    sex,
    comorbidity_count
  ) |>
  drop_na()

# Set reference categories consistently
diagnosis_model_data <- diagnosis_model_data |>
  mutate(
    admission_type = factor(
      admission_type,
      levels = c(
        "Emergency",
        "Elective",
        "Urgent_non_emergency"
      )
    ),
    diagnosis_group = factor(
      diagnosis_group,
      levels = c(
        "Digestive",
        "Cardiovascular",
        "Infection",
        "Neoplasms",
        "Neurological",
        "Other",
        "Respiratory",
        "Symptoms_abnormal_findings",
        "Unknown"
      )
    ),
    sex = factor(
      sex,
      levels = c("Female", "Male", "Unknown")
    )
  )

# Check the analysis sample
cat("Analysis sample size:", nrow(diagnosis_model_data), "\n")
cat(
  "Unique patients:",
  n_distinct(diagnosis_model_data$patient_id),
  "\n"
)

# Check diagnosis-group counts
print(
  diagnosis_model_data |>
    count(diagnosis_group, sort = TRUE)
)


# ------------------------------------------------------------
# Step 2: Fit and compare logistic regression models
# ------------------------------------------------------------

# Model A: Original adjusted model
model_A <- glm(
  readmission_30d ~ admission_type + age + sex + comorbidity_count,
  data = diagnosis_model_data,
  family = binomial(link = "logit")
)

# Model B: Adjusted model including diagnosis group
model_B <- glm(
  readmission_30d ~ admission_type + diagnosis_group +
    age + sex + comorbidity_count,
  data = diagnosis_model_data,
  family = binomial(link = "logit")
)

# Display model summaries
cat("\n--- Model A: Without diagnosis group ---\n")
print(summary(model_A))

cat("\n--- Model B: With diagnosis group ---\n")
print(summary(model_B))

# Compare model fit using a likelihood-ratio test
cat("\n--- Likelihood-ratio test ---\n")
print(anova(model_A, model_B, test = "Chisq"))


# ------------------------------------------------------------
# Step 3: Compare model fit statistics
# ------------------------------------------------------------

model_comparison <- tibble(
  model = c(
    "Model A: Without diagnosis group",
    "Model B: With diagnosis group"
  ),
  observations = c(
    nobs(model_A),
    nobs(model_B)
  ),
  residual_deviance = c(
    deviance(model_A),
    deviance(model_B)
  ),
  AIC = c(
    AIC(model_A),
    AIC(model_B)
  ),
  BIC = c(
    BIC(model_A),
    BIC(model_B)
  )
)

print(model_comparison)

# Save results
write_csv(
  model_comparison,
  "outputs/results/diagnosis_model_comparison.csv"
)

# ------------------------------------------------------------
# Step 4: Compare apparent AUC
# ------------------------------------------------------------

# Install pROC first if needed:
# install.packages("pROC")

library(pROC)

roc_A <- roc(
  response = diagnosis_model_data$readmission_30d,
  predictor = fitted(model_A),
  quiet = TRUE
)

roc_B <- roc(
  response = diagnosis_model_data$readmission_30d,
  predictor = fitted(model_B),
  quiet = TRUE
)

auc_comparison <- tibble(
  model = c(
    "Model A: Without diagnosis group",
    "Model B: With diagnosis group"
  ),
  sample_size = c(
    nobs(model_A),
    nobs(model_B)
  ),
  apparent_auc = c(
    as.numeric(auc(roc_A)),
    as.numeric(auc(roc_B))
  )
)

print(auc_comparison)

write_csv(
  auc_comparison,
  "outputs/results/diagnosis_model_auc_comparison.csv"
)

# ------------------------------------------------------------
# Step 5: Prepare for patient-level cross-validation
# ------------------------------------------------------------

# Check that each patient has a numeric cluster identifier
diagnosis_model_data <- diagnosis_model_data |>
  mutate(patient_cluster_id = as.integer(factor(patient_id)))

# Confirm the number of patients and admissions
cat(
  "Admissions:",
  nrow(diagnosis_model_data),
  "\n"
)

cat(
  "Unique patients:",
  n_distinct(diagnosis_model_data$patient_cluster_id),
  "\n"
)


# ------------------------------------------------------------
# Step 6: Assign patients to cross-validation folds
# ------------------------------------------------------------

set.seed(2026)

# Create one fold assignment per unique patient
patient_folds <- diagnosis_model_data |>
  distinct(patient_cluster_id) |>
  mutate(fold = sample(rep(1:5, length.out = n())))

# Attach the fold to every admission
diagnosis_model_data <- diagnosis_model_data |>
  select(-any_of("fold")) |>
  left_join(patient_folds, by = "patient_cluster_id")

# Check fold allocation
print(
  diagnosis_model_data |>
    group_by(fold) |>
    summarise(
      admissions = n(),
      unique_patients = n_distinct(patient_cluster_id),
      readmissions = sum(readmission_30d == 1),
      .groups = "drop"
    )
)

# ------------------------------------------------------------
# Step 7: Patient-level 5-fold cross-validated AUC
# ------------------------------------------------------------

library(pROC)

cv_results <- map_dfr(1:5, function(k) {
  
  train_data <- diagnosis_model_data |>
    filter(fold != k)
  
  test_data <- diagnosis_model_data |>
    filter(fold == k)
  
  # Model A: Without diagnosis group
  fit_A <- glm(
    readmission_30d ~ admission_type + age + sex +
      comorbidity_count,
    data = train_data,
    family = binomial(link = "logit")
  )
  
  # Model B: With diagnosis group
  fit_B <- glm(
    readmission_30d ~ admission_type + diagnosis_group +
      age + sex + comorbidity_count,
    data = train_data,
    family = binomial(link = "logit")
  )
  
  # Predict held-out admissions
  pred_A <- predict(fit_A, newdata = test_data, type = "response")
  pred_B <- predict(fit_B, newdata = test_data, type = "response")
  
  tibble(
    fold = k,
    sample_size = nrow(test_data),
    auc_A = as.numeric(
      roc(test_data$readmission_30d, pred_A, quiet = TRUE)$auc
    ),
    auc_B = as.numeric(
      roc(test_data$readmission_30d, pred_B, quiet = TRUE)$auc
    )
  )
})

print(cv_results)

# Summarise fold-level performance
cv_summary <- cv_results |>
  summarise(
    mean_auc_A = mean(auc_A),
    sd_auc_A = sd(auc_A),
    mean_auc_B = mean(auc_B),
    sd_auc_B = sd(auc_B),
    mean_auc_difference = mean(auc_B - auc_A)
  )

print(cv_summary)

# Save results
write_csv(
  cv_results,
  "outputs/results/diagnosis_cross_validation_folds.csv"
)

write_csv(
  cv_summary,
  "outputs/results/diagnosis_cross_validation_summary.csv"
)


# ------------------------------------------------------------
# Step 8: Adjusted odds ratios and 95% confidence intervals
# ------------------------------------------------------------

model_B_results <- broom::tidy(
  model_B,
  conf.int = TRUE,
  exponentiate = TRUE
) |>
  rename(
    odds_ratio = estimate,
    conf_low = conf.low,
    conf_high = conf.high
  ) |>
  select(
    term,
    odds_ratio,
    conf_low,
    conf_high,
    p.value
  )

print(model_B_results)

# Save results
write_csv(
  model_B_results,
  "outputs/results/diagnosis_model_B_adjusted_ORs.csv"
)

# ------------------------------------------------------------
# Step 9: Prepare outcome-aware patient-level folds
# ------------------------------------------------------------

set.seed(2026)

# Summarise readmission outcomes for each patient
patient_fold_data <- diagnosis_model_data |>
  group_by(patient_cluster_id) |>
  summarise(
    any_readmission = as.integer(any(readmission_30d == 1)),
    .groups = "drop"
  )

# Assign folds separately within patient outcome groups
patient_fold_data <- patient_fold_data |>
  group_by(any_readmission) |>
  mutate(
    fold = sample(rep(1:5, length.out = n()))
  ) |>
  ungroup()

# Attach patient-level folds to every admission
diagnosis_model_data <- diagnosis_model_data |>
  select(-any_of("fold")) |>
  left_join(
    patient_fold_data |> select(patient_cluster_id, fold),
    by = "patient_cluster_id"
  )

# Check patient and admission distribution
fold_summary <- diagnosis_model_data |>
  group_by(fold) |>
  summarise(
    admissions = n(),
    unique_patients = n_distinct(patient_cluster_id),
    readmissions = sum(readmission_30d == 1),
    .groups = "drop"
  )

print(fold_summary)

# ------------------------------------------------------------
# Step 10: Revised patient-level 5-fold cross-validation
# ------------------------------------------------------------

cv_results_balanced <- purrr::map_dfr(1:5, function(k) {
  
  train_data <- diagnosis_model_data |>
    filter(fold != k)
  
  test_data <- diagnosis_model_data |>
    filter(fold == k)
  
  fit_A <- glm(
    readmission_30d ~ admission_type + age + sex +
      comorbidity_count,
    data = train_data,
    family = binomial(link = "logit")
  )
  
  fit_B <- glm(
    readmission_30d ~ admission_type + diagnosis_group +
      age + sex + comorbidity_count,
    data = train_data,
    family = binomial(link = "logit")
  )
  
  pred_A <- predict(fit_A, newdata = test_data, type = "response")
  pred_B <- predict(fit_B, newdata = test_data, type = "response")
  
  tibble(
    fold = k,
    sample_size = nrow(test_data),
    readmissions = sum(test_data$readmission_30d == 1),
    auc_A = as.numeric(
      pROC::roc(test_data$readmission_30d, pred_A, quiet = TRUE)$auc
    ),
    auc_B = as.numeric(
      pROC::roc(test_data$readmission_30d, pred_B, quiet = TRUE)$auc
    )
  )
})

print(cv_results_balanced)

cv_summary_balanced <- cv_results_balanced |>
  summarise(
    mean_auc_A = mean(auc_A),
    sd_auc_A = sd(auc_A),
    mean_auc_B = mean(auc_B),
    sd_auc_B = sd(auc_B),
    mean_auc_difference = mean(auc_B - auc_A)
  )

print(cv_summary_balanced)

write_csv(
  cv_results_balanced,
  "outputs/results/diagnosis_cross_validation_folds_balanced.csv"
)

write_csv(
  cv_summary_balanced,
  "outputs/results/diagnosis_cross_validation_summary_balanced.csv"
)

# ------------------------------------------------------------
# Step 11: Consolidated model-selection evidence
# ------------------------------------------------------------

final_model_comparison <- model_comparison |>
  left_join(
    auc_comparison |>
      select(model, apparent_auc),
    by = "model"
  ) |>
  mutate(
    cross_validated_mean_auc = c(
      cv_summary_balanced$mean_auc_A,
      cv_summary_balanced$mean_auc_B
    ),
    cross_validated_auc_sd = c(
      cv_summary_balanced$sd_auc_A,
      cv_summary_balanced$sd_auc_B
    )
  )

print(final_model_comparison)

write_csv(
  final_model_comparison,
  "outputs/results/diagnosis_final_model_comparison.csv"
)

# ------------------------------------------------------------
# Step 12: Add likelihood-ratio test to final comparison
# ------------------------------------------------------------

lrt_result <- anova(model_A, model_B, test = "Chisq")

final_model_comparison <- final_model_comparison |>
  mutate(
    likelihood_ratio_chisq = as.numeric(lrt_result$Deviance[2]),
    likelihood_ratio_df = as.integer(lrt_result$Df[2]),
    likelihood_ratio_p_value = as.numeric(lrt_result$`Pr(>Chi)`[2])
  )

print(final_model_comparison)

write_csv(
  final_model_comparison,
  "outputs/results/diagnosis_final_model_comparison.csv"
)


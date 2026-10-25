
# ============================================================
# Project: Hospital Readmission RWE
# Script: 08_model_diagnostics.R
# Purpose: Diagnostic checks and sensitivity analyses
# Dataset: Synthetic hospital admissions
# ============================================================

library(tidyverse)

# ------------------------------------------------------------
# Step 1: Load the cleaned dataset
# ------------------------------------------------------------

hospital_data <- readr::read_csv(
  "data/clean/hospital_admissions_working.csv",
  show_col_types = FALSE
)

# ------------------------------------------------------------
# Step 2: Recreate the adjusted readmission analysis dataset
# ------------------------------------------------------------

model_2_data <- hospital_data |>
  filter(eligible_index == TRUE) |>
  select(
    patient_id,
    readmission_30d,
    admission_type,
    age,
    sex,
    comorbidity_count
  ) |>
  drop_na()

# ------------------------------------------------------------
# Step 3: Fit the adjusted logistic regression
# ------------------------------------------------------------

# Explicitly set reference categories for reproducibility
model_2_data <- model_2_data |>
  mutate(
    admission_type = factor(
      admission_type,
      levels = c("Elective", "Emergency", "Urgent_non_emergency")
    ),
    sex = factor(
      sex,
      levels = c("Female", "Male", "Unknown")
    )
  )

model_2 <- glm(
  readmission_30d ~ admission_type + age + sex + comorbidity_count,
  data = model_2_data,
  family = binomial(link = "logit")
)

# Confirm sample size and reference categories
cat("Admissions:", nobs(model_2), "\n")
cat("Reference admission type: Elective\n")
cat("Reference sex: Female\n")



# ------------------------------------------------------------
# Step 4: Check multicollinearity
# ------------------------------------------------------------

if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}

vif_results <- car::vif(model_2)

print(vif_results)

# Save the results
vif_output <- as.data.frame(vif_results)
vif_output$predictor <- rownames(vif_output)
rownames(vif_output) <- NULL

readr::write_csv(
  vif_output,
  "outputs/results/readmission_vif_diagnostics.csv"
)


# ------------------------------------------------------------
# Step 5: Identify influential observations
# ------------------------------------------------------------

cooks_d <- cooks.distance(model_2)

influence_threshold <- 4 / nobs(model_2)

influence_summary <- tibble(
  admissions = nobs(model_2),
  mean_cooks_distance = mean(cooks_d),
  maximum_cooks_distance = max(cooks_d),
  screening_threshold = influence_threshold,
  observations_above_threshold = sum(
    cooks_d > influence_threshold
  )
)

print(influence_summary)

# Save the summary
readr::write_csv(
  influence_summary,
  "outputs/results/readmission_influence_summary.csv"
)

# Save the 10 most influential observations
influence_check <- tibble(
  row_number = seq_along(cooks_d),
  cooks_distance = as.numeric(cooks_d),
  fitted_probability = fitted(model_2),
  observed_readmission = model.response(model.frame(model_2))
) |>
  arrange(desc(cooks_distance)) |>
  slice_head(n = 10)

print(influence_check)

readr::write_csv(
  influence_check,
  "outputs/results/readmission_top_influential_observations.csv"
)


# ------------------------------------------------------------
# Step 6: Sensitivity analysis excluding the most influential
# observation
# ------------------------------------------------------------

most_influential <- which.max(cooks_d)

model_2_sensitivity <- glm(
  readmission_30d ~ admission_type + age + sex + comorbidity_count,
  data = model_2_data[-most_influential, ],
  family = binomial(link = "logit")
)

# Compare admission-type odds ratios
terms_to_compare <- c(
  "admission_typeEmergency",
  "admission_typeUrgent_non_emergency"
)

sensitivity_comparison <- tibble(
  predictor = terms_to_compare,
  original_OR = unname(exp(coef(model_2)[terms_to_compare])),
  sensitivity_OR = unname(
    exp(coef(model_2_sensitivity)[terms_to_compare])
  )
) |>
  mutate(
    percentage_change = 100 * (
      sensitivity_OR / original_OR - 1
    )
  )

print(sensitivity_comparison)

readr::write_csv(
  sensitivity_comparison,
  "outputs/results/readmission_influence_sensitivity.csv"
)


# ------------------------------------------------------------
# Step 7: Discrimination and calibration
# ------------------------------------------------------------

if (!requireNamespace("pROC", quietly = TRUE)) {
  install.packages("pROC")
}

# AUC: apparent discrimination on the model-fitting data
roc_model_2 <- pROC::roc(
  response = model.response(model.frame(model_2)),
  predictor = fitted(model_2),
  quiet = TRUE
)

auc_result <- tibble(
  model = "Adjusted logistic regression",
  sample_size = nobs(model_2),
  apparent_auc = as.numeric(pROC::auc(roc_model_2))
)

print(auc_result)

readr::write_csv(
  auc_result,
  "outputs/results/readmission_auc.csv"
)

# Calibration by decile of predicted risk
calibration_data <- tibble(
  observed = model.response(model.frame(model_2)),
  predicted = fitted(model_2)
) |>
  mutate(risk_group = dplyr::ntile(predicted, 10))

calibration_table <- calibration_data |>
  group_by(risk_group) |>
  summarise(
    admissions = n(),
    mean_predicted = mean(predicted),
    observed_rate = mean(observed),
    difference = mean_predicted - observed_rate,
    .groups = "drop"
  )

print(calibration_table)

readr::write_csv(
  calibration_table,
  "outputs/results/readmission_calibration.csv"
)


# ============================================================
# Project: Hospital Readmission & Length-of-Stay RWE
# Script: 06_logistic_regression.R
# Purpose: Logistic regression analysis of 30-day readmission
# ============================================================

# Load the cleaned working dataset
admission_data <- read.csv(
  "data/clean/hospital_admissions_working.csv",
  stringsAsFactors = FALSE
)

# Prepare the eligible index-admission cohort
eligible_data <- admission_data[
  admission_data$eligible_index == TRUE,
]

# Check cohort size and outcome distribution
cat("Eligible admissions:", nrow(eligible_data), "\n")
cat(
  "Unique patients:",
  length(unique(eligible_data$patient_id)),
  "\n"
)

print(table(eligible_data$readmission_30d, useNA = "ifany"))


# ------------------------------------------------------------
# Step 2: Unadjusted logistic regression
# ------------------------------------------------------------

# Convert admission type into a categorical variable
eligible_data$admission_type <- factor(
  eligible_data$admission_type
)

# Set Emergency as the reference category
eligible_data$admission_type <- relevel(
  eligible_data$admission_type,
  ref = "Emergency"
)

# Fit the logistic regression model
model_1 <- glm(
  readmission_30d ~ admission_type,
  data = eligible_data,
  family = binomial(link = "logit")
)

# Display model results
summary(model_1)


# ------------------------------------------------------------
# Step 3: Calculate odds ratios and 95% confidence intervals
# ------------------------------------------------------------

model_1_results <- data.frame(
  Term = names(coef(model_1)),
  Odds_Ratio = exp(coef(model_1)),
  Lower_95_CI = exp(
    coef(model_1) - 1.96 * sqrt(diag(vcov(model_1)))
  ),
  Upper_95_CI = exp(
    coef(model_1) + 1.96 * sqrt(diag(vcov(model_1)))
  ),
  P_value = summary(model_1)$coefficients[, 4]
)

print(model_1_results, row.names = FALSE)
# ------------------------------------------------------------
# Step 4 — Save the results
# ------------------------------------------------------------
# Create results folder if needed
dir.create(
  "outputs/results",
  recursive = TRUE,
  showWarnings = FALSE
)

# Save unadjusted logistic regression results
write.csv(
  model_1_results,
  "outputs/results/logistic_model_1_results.csv",
  row.names = FALSE
)

# Confirm that the file exists
file.exists("outputs/results/logistic_model_1_results.csv")


# ------------------------------------------------------------
# Step 5: Check variables for the adjusted model
# ------------------------------------------------------------

str(eligible_data[, c(
  "readmission_30d",
  "admission_type",
  "age",
  "sex",
  "comorbidity_count",
  "previous_admissions"
)])

summary(eligible_data[, c(
  "age",
  "comorbidity_count",
  "previous_admissions"
)])

table(eligible_data$sex, useNA = "ifany")


# ------------------------------------------------------------
# Step 6: Prepare variables for adjusted logistic regression
# ------------------------------------------------------------

# Convert categorical predictors to factors
eligible_data$sex <- factor(eligible_data$sex)
eligible_data$admission_type <- factor(
  eligible_data$admission_type
)

# Check missingness in the proposed model variables
colSums(is.na(eligible_data[, c(
  "readmission_30d",
  "admission_type",
  "age",
  "sex",
  "comorbidity_count"
)]))

# Count complete observations for Model 2
model_2_data <- eligible_data[
  complete.cases(eligible_data[, c(
    "readmission_30d",
    "admission_type",
    "age",
    "sex",
    "comorbidity_count"
  )]),
]

cat("Eligible admissions before exclusions:", nrow(eligible_data), "\n")
cat("Complete observations for Model 2:", nrow(model_2_data), "\n")
cat(
  "Unique patients in Model 2:",
  length(unique(model_2_data$patient_id)),
  "\n"
)


# ------------------------------------------------------------
# Step 7: Adjusted logistic regression — Model 2
# ------------------------------------------------------------

model_2 <- glm(
  readmission_30d ~ admission_type + age + sex + comorbidity_count,
  data = model_2_data,
  family = binomial(link = "logit")
)

# Display model results
summary(model_2)


# ------------------------------------------------------------
# Step 8: Adjusted odds ratios and 95% confidence intervals
# ------------------------------------------------------------

model_2_results <- data.frame(
  Term = names(coef(model_2)),
  Adjusted_OR = exp(coef(model_2)),
  Lower_95_CI = exp(
    coef(model_2) - 1.96 * sqrt(diag(vcov(model_2)))
  ),
  Upper_95_CI = exp(
    coef(model_2) + 1.96 * sqrt(diag(vcov(model_2)))
  ),
  P_value = summary(model_2)$coefficients[, 4]
)

print(model_2_results, row.names = FALSE)


# ------------------------------------------------------------
# Step 9: Save adjusted logistic regression results
# ------------------------------------------------------------

write.csv(
  model_2_results,
  "outputs/results/logistic_model_2_adjusted_results.csv",
  row.names = FALSE
)

# Confirm that the results file exists
file.exists(
  "outputs/results/logistic_model_2_adjusted_results.csv"
)

# ------------------------------------------------------------
# Step 10: Check repeated admissions per patient
# ------------------------------------------------------------

admissions_per_patient <- table(model_2_data$patient_id)

cat(
  "Patients with one admission:",
  sum(admissions_per_patient == 1),
  "\n"
)

cat(
  "Patients with more than one admission:",
  sum(admissions_per_patient > 1),
  "\n"
)

cat(
  "Maximum admissions for one patient:",
  max(admissions_per_patient),
  "\n"
)

summary(as.numeric(admissions_per_patient))

# ------------------------------------------------------------
# Step 11: Install and verify geepack
# ------------------------------------------------------------
"geepack" %in% rownames(installed.packages())

# ------------------------------------------------------------
# Step 12: Patient-clustered logistic regression using GEE
# ------------------------------------------------------------

library(geepack)

# model_3 <- geeglm(
#   readmission_30d ~ admission_type + age + sex + comorbidity_count,
#   id = patient_id,
#   data = model_2_data,
#   family = binomial(link = "logit"),
#   corstr = "exchangeable"
# )
# 
# # Display model results
# summary(model_3)

# ------------------------------------------------------------
# Step 13: Diagnose patient ID format
# ------------------------------------------------------------

class(model_2_data$patient_id)

head(model_2_data$patient_id, 10)

cat(
  "Unique patient IDs:",
  length(unique(model_2_data$patient_id)),
  "\n"
)

cat(
  "Missing patient IDs:",
  sum(is.na(model_2_data$patient_id)),
  "\n"
)
# ------------------------------------------------------------
# Step 14: Prepare patient IDs for GEE analysis
# ------------------------------------------------------------

model_3_data <- model_2_data

model_3_data$patient_cluster_id <- as.integer(
  factor(model_3_data$patient_id)
)

# Sort records so each patient's admissions are together
model_3_data <- model_3_data[
  order(model_3_data$patient_cluster_id),
]

# Check the cluster structure
cat(
  "Unique patient clusters:",
  length(unique(model_3_data$patient_cluster_id)),
  "\n"
)

cat("Total admissions:", nrow(model_3_data), "\n")

cat(
  "Maximum admissions per patient:",
  max(table(model_3_data$patient_cluster_id)),
  "\n"
)

# ------------------------------------------------------------
# Step 15: Fit GEE model accounting for repeated admissions
# ------------------------------------------------------------


library(geepack)

model_3 <- geeglm(
  readmission_30d ~ admission_type + age + sex + comorbidity_count,
  id = patient_cluster_id,
  data = model_3_data,
  family = binomial(link = "logit"),
  corstr = "exchangeable"
)

summary(model_3)

# ------------------------------------------------------------
# Step 16: Extract and save GEE model results
# ------------------------------------------------------------

gee_coef <- summary(model_3)$coefficients

gee_results <- data.frame(
  term = rownames(gee_coef),
  estimate = gee_coef[, "Estimate"],
  standard_error = gee_coef[, "Std.err"],
  odds_ratio = exp(gee_coef[, "Estimate"]),
  conf_low = exp(
    gee_coef[, "Estimate"] - 1.96 * gee_coef[, "Std.err"]
  ),
  conf_high = exp(
    gee_coef[, "Estimate"] + 1.96 * gee_coef[, "Std.err"]
  ),
  p_value = gee_coef[, "Pr(>|W|)"],
  row.names = NULL
)

write.csv(
  gee_results,
  "outputs/results/logistic_model_3_gee_results.csv",
  row.names = FALSE
)

cat("GEE results saved successfully.\n")

# ------------------------------------------------------------
# Step 17: Verify the saved GEE results
# ------------------------------------------------------------


gee_file <- "outputs/results/logistic_model_3_gee_results.csv"

cat("File exists:", file.exists(gee_file), "\n")

gee_results_check <- read.csv(gee_file)

print(gee_results_check, digits = 4)


# ------------------------------------------------------------
# Step 18: Compare results across the three models
# ------------------------------------------------------------

model_1_results <- read.csv(
  "outputs/results/logistic_model_1_results.csv"
)

model_2_results <- read.csv(
  "outputs/results/logistic_model_2_adjusted_results.csv"
)

model_3_results <- read.csv(
  "outputs/results/logistic_model_3_gee_results.csv"
)

cat("\n--- Model 1: Unadjusted ---\n")
print(model_1_results)

cat("\n--- Model 2: Adjusted logistic regression ---\n")
print(model_2_results)

cat("\n--- Model 3: GEE accounting for patient clusters ---\n")
print(model_3_results)

# ------------------------------------------------------------
# Step 20: Build a model comparison table
# ------------------------------------------------------------


comparison_terms <- c(
  "admission_typeElective",
  "admission_typeUrgent_non_emergency",
  "age",
  "sexMale",
  "sexUnknown",
  "comorbidity_count"
)

model_1_comparison <- data.frame(
  term = model_1_results$Term,
  OR_Model_1 = model_1_results$Odds_Ratio,
  CI_low_Model_1 = model_1_results$Lower_95_CI,
  CI_high_Model_1 = model_1_results$Upper_95_CI,
  p_Model_1 = model_1_results$P_value
)
model_2_comparison <- data.frame(
  term = model_2_results$Term,
  OR_Model_2 = model_2_results$Adjusted_OR,
  CI_low_Model_2 = model_2_results$Lower_95_CI,
  CI_high_Model_2 = model_2_results$Upper_95_CI,
  p_Model_2 = model_2_results$P_value
)
model_3_comparison <- data.frame(
  term = model_3_results$term,
  OR_Model_3 = model_3_results$odds_ratio,
  CI_low_Model_3 = model_3_results$conf_low,
  CI_high_Model_3 = model_3_results$conf_high,
  p_Model_3 = model_3_results$p_value
)

model_comparison <- merge(
  model_1_comparison,
  model_2_comparison,
  by = "term",
  all = TRUE
)

model_comparison <- merge(
  model_comparison,
  model_3_comparison,
  by = "term",
  all = TRUE
)

model_comparison <- model_comparison[
  model_comparison$term %in% comparison_terms,
]
# ------------------------------------------------------------
# Step 21: Create and save the forest plot comparing models
# ------------------------------------------------------------
plot_data <- rbind(
  data.frame(
    term = model_comparison$term,
    model = "Model 1: Unadjusted",
    OR = model_comparison$OR_Model_1,
    CI_low = model_comparison$CI_low_Model_1,
    CI_high = model_comparison$CI_high_Model_1
  ),
  data.frame(
    term = model_comparison$term,
    model = "Model 2: Adjusted",
    OR = model_comparison$OR_Model_2,
    CI_low = model_comparison$CI_low_Model_2,
    CI_high = model_comparison$CI_high_Model_2
  ),
  data.frame(
    term = model_comparison$term,
    model = "Model 3: GEE",
    OR = model_comparison$OR_Model_3,
    CI_low = model_comparison$CI_low_Model_3,
    CI_high = model_comparison$CI_high_Model_3
  )
)
plot_data <- plot_data[!is.na(plot_data$OR), ]

plot_data$predictor <- factor(
  plot_data$term,
  levels = rev(c(
    "admission_typeElective",
    "admission_typeUrgent_non_emergency",
    "age",
    "sexMale",
    "sexUnknown",
    "comorbidity_count"
  )),
  labels = rev(c(
    "Elective vs Emergency",
    "Urgent non-emergency vs Emergency",
    "Age (per year)",
    "Male vs Female",
    "Unknown vs Female",
    "Comorbidity count (per condition)"
  ))
)

forest_plot <- ggplot(
  plot_data,
  aes(x = OR, y = predictor, colour = model, group = model)
) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed"
  ) +
  geom_errorbar(
    aes(xmin = CI_low, xmax = CI_high),
    width = 0.2,
    position = position_dodge(width = 0.6)
  ) +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 2.5
  ) +
  scale_x_log10() +
  labs(
    title = "30-Day Readmission: Model Comparison",
    subtitle = "Odds ratios and 95% confidence intervals",
    x = "Odds ratio (log scale)",
    y = NULL,
    colour = "Model"
  ) +
  theme_minimal()

print(forest_plot)

ggsave(
  "outputs/figures/logistic_models_forest_plot.png",
  plot = forest_plot,
  width = 10,
  height = 6,
  dpi = 300
)

cat(
  "\nForest plot saved:",
  file.exists("outputs/figures/logistic_models_forest_plot.png"),
  "\n"
)

# ------------------------------------------------------------
# Step 22: Verify the forest plot file
# ------------------------------------------------------------


plot_file <- "outputs/figures/logistic_models_forest_plot.png"

cat("Forest plot exists:", file.exists(plot_file), "\n")

if (file.exists(plot_file)) {
  print(file.info(plot_file)[, c("size", "mtime")])
}


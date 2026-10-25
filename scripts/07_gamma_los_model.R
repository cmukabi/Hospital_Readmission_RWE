
# ------------------------------------------------------------
# Project: Hospital Readmission RWE
# Script: 07_gamma_los_model.R
# Purpose: Model hospital length of stay (LOS)
# ------------------------------------------------------------

library(tidyverse)

# Load the cleaned working dataset
hospital_data <- read_csv(
  "data/clean/hospital_admissions_working.csv",
  show_col_types = FALSE
)

# Check the dataset dimensions and variables
dim(hospital_data)
names(hospital_data)


# ------------------------------------------------------------
# Step 2: Examine length of stay
# ------------------------------------------------------------

# Check the distribution and missing values
summary(hospital_data$length_of_stay)

sum(is.na(hospital_data$length_of_stay))

# Check for zero or negative values
sum(hospital_data$length_of_stay <= 0, na.rm = TRUE)

# Examine skewness visually
ggplot(hospital_data, aes(x = length_of_stay)) +
  geom_histogram(binwidth = 1, boundary = 0) +
  labs(
    title = "Distribution of Hospital Length of Stay",
    x = "Length of stay (days)",
    y = "Number of admissions"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# Step 4: Prepare the LOS analysis dataset
# ------------------------------------------------------------

los_data <- hospital_data %>%
  filter(eligible_index == TRUE) %>%
  select(
    patient_id,
    length_of_stay,
    admission_type,
    age,
    sex,
    comorbidity_count
  ) %>%
  drop_na()

# Check the analysis sample
cat("Admissions included:", nrow(los_data), "\n")
cat("Unique patients:", n_distinct(los_data$patient_id), "\n")
cat("Minimum LOS:", min(los_data$length_of_stay), "\n")
cat("Maximum LOS:", max(los_data$length_of_stay), "\n")

# Confirm that LOS is positive
stopifnot(all(los_data$length_of_stay > 0))


# ------------------------------------------------------------
# Step 5: Gamma regression for length of stay
# ------------------------------------------------------------

los_gamma_model <- glm(
  length_of_stay ~ admission_type + age + sex + comorbidity_count,
  data = los_data,
  family = Gamma(link = "log")
)

# Display model summary
summary(los_gamma_model)



# ------------------------------------------------------------
# Step 6: Adjusted length-of-stay ratios and 95% CIs
# ------------------------------------------------------------

los_ratio_results <- data.frame(
  term = names(coef(los_gamma_model)),
  estimate = exp(coef(los_gamma_model)),
  conf_low = exp(confint.default(los_gamma_model)[, 1]),
  conf_high = exp(confint.default(los_gamma_model)[, 2]),
  p_value = summary(los_gamma_model)$coefficients[, 4]
)

# Exclude the intercept for easier clinical interpretation
los_ratio_results <- los_ratio_results %>%
  filter(term != "(Intercept)") %>%
  mutate(
    percentage_difference = (estimate - 1) * 100
  )

print(los_ratio_results, digits = 3)


# ------------------------------------------------------------
# Step 7: Patient-clustered Gamma GEE
# ------------------------------------------------------------

# Install geepack if it is not already installed
if (!requireNamespace("geepack", quietly = TRUE)) {
  install.packages("geepack")
}

# Create numeric patient cluster IDs and order observations
los_gee_data <- los_data %>%
  mutate(patient_cluster_id = as.integer(factor(patient_id))) %>%
  arrange(patient_cluster_id)

# Fit Gamma GEE with a log link
los_gamma_gee <- geepack::geeglm(
  length_of_stay ~ admission_type + age + sex + comorbidity_count,
  id = patient_cluster_id,
  data = los_gee_data,
  family = Gamma(link = "log"),
  corstr = "exchangeable"
)

summary(los_gamma_gee)


# ------------------------------------------------------------
# Step 9: Save patient-clustered Gamma GEE results
# ------------------------------------------------------------

gee_coef <- summary(los_gamma_gee)$coefficients

los_gee_results <- data.frame(
  term = rownames(gee_coef),
  estimate = gee_coef[, "Estimate"],
  standard_error = gee_coef[, "Std.err"],
  los_ratio = exp(gee_coef[, "Estimate"]),
  conf_low = exp(
    gee_coef[, "Estimate"] - 1.96 * gee_coef[, "Std.err"]
  ),
  conf_high = exp(
    gee_coef[, "Estimate"] + 1.96 * gee_coef[, "Std.err"]
  ),
  p_value = gee_coef[, "Pr(>|W|)"],
  row.names = NULL
)

# Remove the intercept for the main results table
los_gee_results <- los_gee_results %>%
  filter(term != "(Intercept)") %>%
  mutate(
    percentage_difference = (los_ratio - 1) * 100
  )

# Save results
dir.create("outputs/results", recursive = TRUE, showWarnings = FALSE)

write_csv(
  los_gee_results,
  "outputs/results/gamma_los_gee_results.csv"
)

print(los_gee_results, digits = 3)


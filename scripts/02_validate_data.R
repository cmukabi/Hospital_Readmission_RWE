
# ============================================================
# Project: Hospital Readmission & Length-of-Stay RWE
# Script: 02_validate_data.R
# Purpose: Validate the generated synthetic dataset
# ============================================================

# Check the number of admissions
nrow(admission_data)

# Check the number of columns
ncol(admission_data)

# Check the structure of the dataset
str(admission_data)

# Check for duplicate admission IDs
sum(duplicated(admission_data$admission_id))

# Check for missing values
colSums(is.na(admission_data))

# Check length-of-stay values
summary(admission_data$length_of_stay)

# Check whether discharge dates are after admission dates
sum(
  admission_data$discharge_date <
    admission_data$admission_date
)

# Check the admission type categories
table(admission_data$admission_type)

# Check the diagnosis categories
table(
  admission_data$diagnosis_group,
  useNA = "ifany"
)

# Check the simulated readmission outcome
table(
  admission_data$readmission_30d_label,
  useNA = "ifany"
)

# ============================================================
# Final checks: index admission eligibility
# ============================================================

expected_eligible <- (
  admission_data$discharge_date >= as.Date("2025-01-01") &
    admission_data$discharge_date <= as.Date("2025-12-31") &
    (
      is.na(admission_data$days_since_previous_discharge) |
        admission_data$days_since_previous_discharge > 30
    )
)

# Confirm eligibility matches the intended rule
stopifnot(
  sum(expected_eligible != admission_data$eligible_index) == 0
)

# Confirm there are no missing eligibility flags
stopifnot(
  sum(is.na(admission_data$eligible_index)) == 0
)

message("Validation passed: eligibility checks are correct.")


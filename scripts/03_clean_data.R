
# ============================================================
# Project: Hospital Readmission & Length-of-Stay RWE
# Script: 03_clean_data.R
# Purpose: Load and prepare synthetic hospital data
# ============================================================

# Load the saved synthetic dataset
admission_data <- read.csv(
  "data/raw/synthetic_hospital_admissions.csv",
  stringsAsFactors = FALSE
)

# Check the dataset dimensions
dim(admission_data)

# Inspect the variable types
str(admission_data)

# Check missing values
colSums(is.na(admission_data))

# Convert date columns back to Date format
admission_data$admission_date <- as.Date(
  admission_data$admission_date
)

admission_data$discharge_date <- as.Date(
  admission_data$discharge_date
)
# Convert remaining date columns back to Date format
admission_data$next_admission_date <- as.Date(
  admission_data$next_admission_date
)
# Verify date and duration variable types
class(admission_data$admission_date)
class(admission_data$discharge_date)
class(admission_data$next_admission_date)
class(admission_data$days_to_next_admission)

# Create a separate clean-data folder
dir.create(
  "data/clean",
  recursive = TRUE,
  showWarnings = FALSE
)

# Save a working copy for further preparation
write.csv(
  admission_data,
  "data/clean/hospital_admissions_working.csv",
  row.names = FALSE
)

# Confirm the saved file exists
file.exists(
  "data/clean/hospital_admissions_working.csv"
)
# ============================================================
# Final checks: prepared working dataset
# ============================================================

# Confirm the working file exists
stopifnot(
  file.exists("data/clean/hospital_admissions_working.csv")
)

# Confirm the expected number of rows and columns
stopifnot(
  nrow(admission_data) == 1861,
  ncol(admission_data) == 21
)

# Confirm date columns are correctly formatted in memory
stopifnot(
  inherits(admission_data$admission_date, "Date"),
  inherits(admission_data$discharge_date, "Date"),
  inherits(admission_data$next_admission_date, "Date")
)

message("Cleaning checks passed.")

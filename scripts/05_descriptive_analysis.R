
# ============================================================
# Project: Hospital Readmission & Length-of-Stay RWE
# Script: 05_descriptive_analysis.R
# Purpose: Describe the synthetic admission cohort
# ============================================================

# Load the cleaned working dataset
admission_data <- read.csv(
  "data/clean/hospital_admissions_working.csv",
  stringsAsFactors = FALSE
)

# Restore date variables
admission_data$admission_date <- as.Date(
  admission_data$admission_date
)
admission_data$discharge_date <- as.Date(
  admission_data$discharge_date
)


# Confirm required variables are present
required_variables <- c(
  "patient_id",
  "admission_type",
  "length_of_stay",
  "readmission_30d",
  "eligible_index"
)

stopifnot(
  all(required_variables %in% names(admission_data))
)

# Confirm that the dataset contains records
stopifnot(nrow(admission_data) > 0)

message("Initial dataset checks passed.")

# 1. Dataset size
cat("Number of admissions:", nrow(admission_data), "\n")
cat(
  "Number of unique patients:",
  length(unique(admission_data$patient_id)),
  "\n"
)

# 2. Age distribution
summary(admission_data$age)

# 3. Sex distribution
table(admission_data$sex, useNA = "ifany")

# 4. Admission type
table(admission_data$admission_type)

# 5. Diagnosis group
table(admission_data$diagnosis_group, useNA = "ifany")

# 6. Length of stay
summary(admission_data$length_of_stay)

# 7. Overall simulated readmission percentage
mean(admission_data$readmission_30d, na.rm = TRUE) * 100


# ------------------------------------------------------------
# Visualise the length-of-stay distribution
# ------------------------------------------------------------

# ------------------------------------------------------------
# 8. Eligible index-admission cohort
# ------------------------------------------------------------

# Identify eligible index admissions
eligible_data <- admission_data[
  admission_data$eligible_index == TRUE,
]

# Number of eligible index admissions
cat(
  "Eligible index admissions:",
  nrow(eligible_data),
  "\n"
)

# Number of unique patients in the eligible cohort
cat(
  "Unique patients in eligible cohort:",
  length(unique(eligible_data$patient_id)),
  "\n"
)

# 30-day readmission rate in the eligible cohort
cat(
  "30-day readmission rate (%):",
  round(mean(eligible_data$readmission_30d) * 100, 2),
  "\n"
)

# Readmission counts
table(eligible_data$readmission_30d)

# ------------------------------------------------------------
# 9. Readmission by admission type in the eligible cohort
# ------------------------------------------------------------

# Count outcomes by admission type
readmission_by_type <- table(
  eligible_data$admission_type,
  eligible_data$readmission_30d
)

print(readmission_by_type)

# Calculate the readmission percentage for each type
readmission_rate_by_type <- prop.table(
  readmission_by_type,
  margin = 1
) * 100

print(round(readmission_rate_by_type, 2))

# ------------------------------------------------------------
# 10. Length of stay by readmission outcome
# ------------------------------------------------------------

# Summarise length of stay for eligible index admissions
aggregate(
  length_of_stay ~ readmission_30d,
  data = eligible_data,
  FUN = function(x) {
    c(
      n = length(x),
      mean = mean(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      IQR = IQR(x, na.rm = TRUE)
    )
  }
)

# Cleaner length-of-stay summary for the eligible cohort
los_summary <- data.frame(
  Outcome = c("No readmission", "Readmission"),
  N = c(
    sum(eligible_data$readmission_30d == 0),
    sum(eligible_data$readmission_30d == 1)
  ),
  Mean_LOS = c(
    mean(eligible_data$length_of_stay[
      eligible_data$readmission_30d == 0
    ]),
    mean(eligible_data$length_of_stay[
      eligible_data$readmission_30d == 1
    ])
  ),
  Median_LOS = c(
    median(eligible_data$length_of_stay[
      eligible_data$readmission_30d == 0
    ]),
    median(eligible_data$length_of_stay[
      eligible_data$readmission_30d == 1
    ])
  ),
  IQR_LOS = c(
    IQR(eligible_data$length_of_stay[
      eligible_data$readmission_30d == 0
    ]),
    IQR(eligible_data$length_of_stay[
      eligible_data$readmission_30d == 1
    ])
  )
)

print(los_summary)

# Create results folder if needed
dir.create(
  "outputs/results",
  recursive = TRUE,
  showWarnings = FALSE
)

# Save length-of-stay summary
write.csv(
  los_summary,
  "outputs/results/length_of_stay_by_readmission.csv",
  row.names = FALSE
)

# Save readmission counts by admission type
write.csv(
  as.data.frame(readmission_by_type),
  "outputs/results/readmission_by_admission_type.csv",
  row.names = FALSE
)

# Confirm both files exist
file.exists(
  c(
    "outputs/results/length_of_stay_by_readmission.csv",
    "outputs/results/readmission_by_admission_type.csv"
  )
)

# Create the figures folder if needed
dir.create(
  "outputs/figures",
  recursive = TRUE,
  showWarnings = FALSE
)

# Save the histogram to a PNG file
png(
  "outputs/figures/length_of_stay_histogram.png",
  width = 1000,
  height = 700
)

hist(
  admission_data$length_of_stay,
  breaks = seq(
    0.5,
    max(admission_data$length_of_stay, na.rm = TRUE) + 1,
    by = 1
  ),
  main = "Distribution of Hospital Length of Stay",
  xlab = "Length of stay (days)",
  ylab = "Number of admissions"
)

dev.off()


# ------------------------------------------------------------
# 11. Final output checks
# ------------------------------------------------------------

# Confirm that the results files and figure exist
expected_outputs <- c(
  "outputs/results/length_of_stay_by_readmission.csv",
  "outputs/results/readmission_by_admission_type.csv",
  "outputs/figures/length_of_stay_histogram.png"
)

output_status <- file.exists(expected_outputs)

print(data.frame(
  File = expected_outputs,
  Exists = output_status
))

# Stop if any expected output is missing
stopifnot(all(output_status))

message("Descriptive analysis completed successfully.")













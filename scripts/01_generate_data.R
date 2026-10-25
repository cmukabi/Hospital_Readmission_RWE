# ============================================================
# Project: Hospital Readmission & Length-of-Stay RWE
# Purpose: Generate synthetic hospital admission data
# Data: Entirely simulated; not real patient records
# Script: 01_generate_data.R
# ============================================================

# Set a fixed random seed for reproducibility
set.seed(2026)

# Confirm that the random seed is set
print("Random seed set successfully")

# ------------------------------------------------------------
# 1. Generate synthetic patient identifiers
# ------------------------------------------------------------

n_patients <- 1200
n_admissions <- 2000

patient_ids <- sprintf("P%04d", 1:n_patients)

# Assign each admission to a patient
admission_patient_id <- sample(
  patient_ids,
  size = n_admissions,
  replace = TRUE
)

# Check the number of unique patients represented
length(unique(admission_patient_id))

# ------------------------------------------------------------
# 2. Generate patient-level demographic characteristics
# ------------------------------------------------------------

patient_data <- data.frame(
  patient_id = patient_ids,
  
  # Age in years, restricted to adults aged 18 and over
  age = sample(
    18:95,
    size = n_patients,
    replace = TRUE
  ),
  
  # Sex, including unknown/not recorded
  sex = sample(
    c("Female", "Male", "Unknown"),
    size = n_patients,
    replace = TRUE,
    prob = c(0.51, 0.47, 0.02)
  )
)

# Inspect the first six patients
head(patient_data)

# Check the age distribution
summary(patient_data$age)

# Count patients by sex category
table(patient_data$sex)

# ------------------------------------------------------------
# 3. Generate patient-level clinical characteristics
# ------------------------------------------------------------

# Number of comorbid conditions
patient_data$comorbidity_count <- sample(
  0:6,
  size = n_patients,
  replace = TRUE,
  prob = c(0.15, 0.22, 0.23, 0.18, 0.12, 0.07, 0.03)
)

# Previous admissions in the preceding 12 months
patient_data$previous_admissions <- sample(
  c("0", "1", "2", "3+", "Unknown"),
  size = n_patients,
  replace = TRUE,
  prob = c(0.50, 0.25, 0.13, 0.08, 0.04)
)

# Inspect the new variables
head(patient_data)

# Summarise comorbidity burden
summary(patient_data$comorbidity_count)

# Count previous-admission categories
table(patient_data$previous_admissions)


# ------------------------------------------------------------
# 4. Define admission-level clinical characteristics
# ------------------------------------------------------------

admission_type_categories <- c(
  "Elective",
  "Emergency",
  "Urgent_non_emergency"
)
admission_type_prob <- c(0.30, 0.50, 0.20)

specialty_categories <- c(
  "General_medicine",
  "Cardiology_respiratory",
  "General_surgery",
  "Orthopaedic_surgery"
)
specialty_prob <- c(0.40, 0.25, 0.20, 0.15)

diagnosis_group_categories <- c(
  "Cardiovascular",
  "Respiratory",
  "Digestive",
  "Infection",
  "Neoplasms",
  "Neurological",
  "Symptoms_abnormal_findings",
  "Other",
  "Unknown"
)
diagnosis_group_prob <- c(
  0.12, 0.12, 0.18, 0.12, 0.10,
  0.08, 0.16, 0.10, 0.02
)

discharge_destination_categories <- c(
  "Home_without_formal_support",
  "Home_with_formal_support",
  "Another_care_facility",
  "Unknown"
)
discharge_destination_prob <- c(
  0.65, 0.20, 0.10, 0.05
)

# ------------------------------------------------------------
# 5. Generate admission dates and length of stay
# ------------------------------------------------------------

# Study period and additional follow-up period
study_start <- as.Date("2025-01-01")
study_end <- as.Date("2025-12-31")
followup_end <- as.Date("2026-02-28")

# Generate admission episodes chronologically by patient
set.seed(20261020)

# Prepare patient-level risk factors for admission scheduling
patient_data$risk_score <-
  0.03 * (patient_data$age - 50) +
  0.30 * patient_data$comorbidity_count +
  0.35 * ifelse(
    patient_data$previous_admissions == "3+", 2,
    ifelse(patient_data$previous_admissions == "2", 1,
           ifelse(patient_data$previous_admissions == "1", 0.5, 0))
  )


patient_admission_counts <- tabulate(
  sample(
    seq_len(n_patients),
    size = n_admissions,
    replace = TRUE
  ),
  nbins = n_patients
)

admission_records <- vector("list", n_admissions)
record_index <- 1

for (i in seq_len(n_patients)) {
  
  n_patient_admissions <- patient_admission_counts[i]
  
  if (n_patient_admissions == 0) next
  
  # First admission for this patient
  current_admission <- sample(
    seq(study_start, study_end, by = "day"),
    size = 1
  )
  
  for (j in seq_len(n_patient_admissions)) {
    
    # Generate a positive, right-skewed length of stay
    current_los <- max(
      1,
      round(rgamma(1, shape = 2, scale = 2.5))
    )
    
    current_discharge <- current_admission + current_los
    
    # Store admissions only within the required follow-up period
    if (current_admission <= followup_end) {
      
      admission_records[[record_index]] <- data.frame(
        patient_id = patient_ids[i],
        admission_date = current_admission,
        discharge_date = current_discharge,
        length_of_stay = current_los
      )
      
      record_index <- record_index + 1
    }
    
    # Schedule the next admission using the patient's simulated risk score
    patient_risk <- patient_data$risk_score[i]
    
    # Higher risk produces a shorter average gap
    mean_gap <- 75 * exp(-0.25 * patient_risk)
    
    gap_days <- max(
      1,
      round(rgamma(1, shape = 2, scale = mean_gap / 2))
    )
    
    current_admission <- current_discharge + gap_days
    
    if (current_admission > followup_end) break
  }
}

# Combine generated admission records
admission_records <- admission_records[
  !vapply(admission_records, is.null, logical(1))
]

admission_data <- do.call(rbind, admission_records)
rownames(admission_data) <- NULL

# Assign unique admission identifiers
admission_data$admission_id <- sprintf(
  "A%04d",
  seq_len(nrow(admission_data))
)

# Check dates and length of stay
head(admission_data)

summary(admission_data$length_of_stay)
summary(
  as.numeric(
    admission_data$discharge_date -
      admission_data$admission_date
  )
)

# Check number of admissions and patients
nrow(admission_data)
length(unique(admission_data$patient_id))


# ------------------------------------------------------------
# 6. Combine patient and admission data
# ------------------------------------------------------------

# Section 5 has already created admission_data.
# Generate clinical characteristics for the actual records.

n_generated <- nrow(admission_data)

admission_data$admission_type <- sample(
  admission_type_categories,
  size = n_generated,
  replace = TRUE,
  prob = admission_type_prob
)

admission_data$specialty <- sample(
  specialty_categories,
  size = n_generated,
  replace = TRUE,
  prob = specialty_prob
)

admission_data$diagnosis_group <- sample(
  diagnosis_group_categories,
  size = n_generated,
  replace = TRUE,
  prob = diagnosis_group_prob
)

admission_data$discharge_destination <- sample(
  discharge_destination_categories,
  size = n_generated,
  replace = TRUE,
  prob = discharge_destination_prob
)

# Match each admission to its patient's characteristics
patient_match <- match(
  admission_data$patient_id,
  patient_data$patient_id
)

admission_data$age <- patient_data$age[patient_match]
admission_data$sex <- patient_data$sex[patient_match]

admission_data$comorbidity_count <-
  patient_data$comorbidity_count[patient_match]

admission_data$previous_admissions <-
  patient_data$previous_admissions[patient_match]

# Sort chronologically within each patient
admission_data <- admission_data[
  order(
    admission_data$patient_id,
    admission_data$admission_date
  ),
]

rownames(admission_data) <- NULL

# Inspect the completed dataset
dim(admission_data)
head(admission_data)
str(admission_data)

# Check category distributions
table(admission_data$admission_type)
table(admission_data$specialty)
table(admission_data$diagnosis_group)
table(admission_data$discharge_destination)

# Check patient matching and admission IDs
sum(is.na(patient_match))
anyDuplicated(admission_data$admission_id)


# ------------------------------------------------------------
# 7. Generate risk-linked 30-day unplanned readmission outcomes
# ------------------------------------------------------------

# Sort admissions chronologically within each patient
admission_data <- admission_data[
  order(admission_data$patient_id, admission_data$admission_date),
]
rownames(admission_data) <- NULL

# Identify the next admission after each discharge
admission_data$next_admission_date <- as.Date(NA)
admission_data$next_admission_type <- NA_character_

for (i in seq_len(nrow(admission_data))) {
  
  later_admissions <- which(
    admission_data$patient_id == admission_data$patient_id[i] &
      admission_data$admission_date > admission_data$discharge_date[i]
  )
  
  if (length(later_admissions) > 0) {
    next_row <- later_admissions[
      which.min(admission_data$admission_date[later_admissions])
    ]
    
    admission_data$next_admission_date[i] <-
      admission_data$admission_date[next_row]
    
    admission_data$next_admission_type[i] <-
      admission_data$admission_type[next_row]
  }
}

# Calculate days from discharge to the next admission
admission_data$days_to_next_admission <- as.numeric(
  admission_data$next_admission_date -
    admission_data$discharge_date
)

# Define whether the next admission is unplanned
admission_data$next_admission_unplanned <-
  admission_data$next_admission_type %in%
  c("Emergency", "Urgent_non_emergency")


# Identify whether ANY unplanned admission occurs within 30 days
# after discharge, rather than checking only the next admission.

admission_data$readmission_30d <- 0L

for (i in seq_len(nrow(admission_data))) {
  
  index_patient <- admission_data$patient_id[i]
  index_discharge <- admission_data$discharge_date[i]
  
  later_rows <- which(
    admission_data$patient_id == index_patient &
      admission_data$admission_date > index_discharge &
      admission_data$admission_date <= index_discharge + 30
  )
  
  if (length(later_rows) > 0) {
    
    unplanned_rows <- later_rows[
      admission_data$admission_type[later_rows] %in%
        c("Emergency", "Urgent_non_emergency")
    ]
    
    if (length(unplanned_rows) > 0) {
      admission_data$readmission_30d[i] <- 1L
    }
  }
}

admission_data$readmission_30d_label <- ifelse(
  admission_data$readmission_30d == 1,
  "Yes",
  "No"
)


# Calculate days since the previous discharge
admission_data$days_since_previous_discharge <- NA_real_

for (i in seq_len(nrow(admission_data))) {
  
  earlier_discharges <- which(
    admission_data$patient_id == admission_data$patient_id[i] &
      admission_data$discharge_date < admission_data$admission_date[i]
  )
  
  if (length(earlier_discharges) > 0) {
    most_recent_discharge <- max(
      admission_data$discharge_date[earlier_discharges]
    )
    
    admission_data$days_since_previous_discharge[i] <-
      as.numeric(
        admission_data$admission_date[i] -
          most_recent_discharge
      )
  }
}

# Define the eligible index cohort
admission_data$eligible_index <-
  admission_data$discharge_date >= as.Date("2025-01-01") &
  admission_data$discharge_date <= as.Date("2025-12-31") &
  (
    is.na(admission_data$days_since_previous_discharge) |
      admission_data$days_since_previous_discharge > 30
  )

# Summarise qualifying readmissions
print(
  table(
    admission_data$readmission_30d_label[
      admission_data$eligible_index
    ],
    useNA = "ifany"
  )
)

print(
  round(
    mean(
      admission_data$readmission_30d[
        admission_data$eligible_index
      ],
      na.rm = TRUE
    ) * 100,
    2
  )
)

# ------------------------------------------------------------
# 8. Introduce selected missing values
# ------------------------------------------------------------

# Keep a complete copy for comparison during validation
admission_data_complete <- admission_data

# Set a fixed seed for the missingness simulation
set.seed(2027)

# Introduce missing age values in approximately 2% of admissions
age_missing <- sample(
  seq_len(nrow(admission_data)),
  size = round(0.02 * nrow(admission_data))
)

admission_data$age[age_missing] <- NA

# Introduce missing specialty values in approximately 2%
specialty_missing <- sample(
  seq_len(nrow(admission_data)),
  size = round(0.02 * nrow(admission_data))
)

admission_data$specialty[specialty_missing] <- NA

# Introduce missing comorbidity values in approximately 3%
comorbidity_missing <- sample(
  seq_len(nrow(admission_data)),
  size = round(0.03 * nrow(admission_data))
)

admission_data$comorbidity_count[comorbidity_missing] <- NA

# Check missing values by variable
colSums(is.na(admission_data))

# Check the proportion missing for each variable
round(
  colMeans(is.na(admission_data)) * 100,
  2
)

# ------------------------------------------------------------
# 9. Save the raw synthetic dataset
# ------------------------------------------------------------

# Create the raw data folder if it does not exist
dir.create(
  "data/raw",
  recursive = TRUE,
  showWarnings = FALSE
)

# Save the synthetic dataset as a CSV file
write.csv(
  admission_data,
  "data/raw/synthetic_hospital_admissions.csv",
  row.names = FALSE
)

# Confirm the file exists
file.exists(
  "data/raw/synthetic_hospital_admissions.csv"
)





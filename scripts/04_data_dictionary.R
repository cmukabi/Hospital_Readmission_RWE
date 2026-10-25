
# ============================================================
# Project: Hospital Readmission & Length-of-Stay RWE
# Script: 04_data_dictionary.R
# Purpose: Document variables in the synthetic dataset
# ============================================================

data_dictionary <- data.frame(
  variable = c(
    "admission_id",
    "patient_id",
    "admission_date",
    "discharge_date",
    "length_of_stay",
    "age",
    "sex",
    "comorbidity_count",
    "previous_admissions",
    "admission_type",
    "specialty",
    "diagnosis_group",
    "discharge_destination",
    "readmission_30d",
    "readmission_30d_label"
  ),
  
  description = c(
    "Unique identifier for each hospital admission",
    "Synthetic patient identifier; may repeat across admissions",
    "Date of hospital admission",
    "Date of hospital discharge",
    "Length of stay in days",
    "Patient age in years",
    "Recorded sex category",
    "Number of comorbid conditions",
    "Simulated baseline category for previous admissions; not derived from observed admission records",
    "Type of hospital admission",
    "Clinical specialty",
    "Broad primary diagnosis group",
    "Destination following discharge",
    "Simulated binary 30-day readmission outcome",
    "Label for the simulated readmission outcome"
  ),
  
  data_type = c(
    "Character", "Character", "Date", "Date",
    "Numeric", "Numeric", "Categorical",
    "Numeric", "Categorical", "Categorical",
    "Categorical", "Categorical", "Categorical",
    "Binary", "Categorical"
  ),
  
  stringsAsFactors = FALSE
)

# Inspect the data dictionary
print(data_dictionary)

# Save it as a CSV file
write.csv(
  data_dictionary,
  "data_dictionary.csv",
  row.names = FALSE
)

# Confirm the file exists
file.exists("data_dictionary.csv")

# Hospital Readmission & Length-of-Stay RWE

## Project Overview

This project demonstrates a reproducible Real-World Evidence (RWE) workflow using a fully synthetic hospital admissions dataset. It examines 30-day unplanned hospital readmission and length of stay (LOS), progressing from data generation and validation to descriptive analysis, regression modelling, diagnostic assessment, and model comparison.

The project is designed to demonstrate applied skills in healthcare data analysis, epidemiological methods, statistical modelling, reproducible research, and interpretation of clinical outcomes.

## Research Questions

This project addresses four analytical questions using a synthetic hospital admissions dataset:

1. What is the observed 30-day readmission rate among eligible index admissions?
2. Which patient and admission characteristics are associated with 30-day readmission in the adjusted logistic regression model?
3. Does adding diagnosis group improve model fit or discrimination compared with the simpler readmission model?
4. Which patient and admission characteristics are associated with length of hospital stay in the patient-clustered Gamma GEE analysis?

These analyses are exploratory demonstrations of RWE methods. Because the dataset is synthetic, the findings do not represent actual hospital outcomes or establish clinical effects.

## View the Published Report

[Open the full interactive Hospital Readmission RWE Report](https://cmukabi.github.io/Hospital_Readmission_RWE/report/hospital_readmission_report.html)

**Data privacy and provenance:** All data are simulated. They do not represent real patients, actual NHS hospital records, or observed hospital performance.

## Project Objectives

- Generate a reproducible synthetic hospital admissions cohort.
- Validate data integrity, variable types, and index-admission eligibility.
- Prepare a working dataset and document variable definitions.
- Describe patient and admission characteristics and 30-day readmission patterns.
- Estimate associations between patient/admission characteristics and readmission.
- Analyse length of stay using generalized linear modelling and patient-clustered methods.
- Assess model discrimination, calibration, multicollinearity, and influential observations.
- Compare readmission models with and without diagnosis group.
- Produce reproducible CSV outputs, figures, and a Quarto HTML report.

## Project Structure

```text
Hospital_Readmission_RWE/
├── data/
│   ├── raw/
│   └── clean/
├── scripts/
│   ├── 01_generate_data.R
│   ├── 02_validate_data.R
│   ├── 03_clean_data.R
│   ├── 04_data_dictionary.R
│   ├── 05_descriptive_analysis.R
│   └── [Scripts 06–09: modelling and diagnostics]
├── outputs/
│   ├── figures/
│   └── results/
├── report/
│   ├── hospital_readmission_report.qmd
│   └── hospital_readmission_report.html
├── data_dictionary.csv
├── Hospital_Readmission_RWE.Rproj
└── README.md
```

## Analytical Workflow

### Data generation and preparation

| Script | Purpose |
|---|---|
| `01_generate_data.R` | Generate synthetic patient and admission records, clinical characteristics, length of stay, readmission outcomes, and selected missing values. |
| `02_validate_data.R` | Check dataset structure, duplicate admission identifiers, missingness, length of stay, dates, categories, and index-admission eligibility. |
| `03_clean_data.R` | Load the raw data, restore date formats, and save the working dataset. |
| `04_data_dictionary.R` | Document selected variables, their meanings, and data types. |
| `05_descriptive_analysis.R` | Summarise the cohort, readmission outcomes, admission types, and length of stay; export tables and a histogram. |
| `06_logistic_regression.R` | Fits logistic regression models for 30-day readmission, including adjusted and patient-clustered models, and produces a forest plot. |
| `07_gamma_los_model.R` | Models length of stay using Gamma regression and patient-clustered generalised estimating equations (GEE). |
| `08_model_diagnostics.R` | Assesses model assumptions and performance using diagnostics, influence analysis, sensitivity analysis, discrimination, and calibration. |
| `09_diagnosis_group_analysis.R` | Compares readmission models with and without diagnosis group, including patient-level cross-validation. |

### Statistical methods

The project includes:

- **Descriptive analysis:** Cohort characteristics, outcome frequencies, readmission percentages, and length-of-stay summaries.
- **Logistic regression:** Estimation of adjusted odds ratios (aORs) for 30-day readmission.
- **Patient-clustered modelling:** Generalized estimating equations (GEE) to account for correlation between repeated admissions from the same patient.
- **Gamma regression with a log link:** Analysis of the positively skewed length-of-stay outcome.
- **Model diagnostics:** Variance inflation factors (VIF), influence assessment using Cook's distance, sensitivity analysis, discrimination using area under the ROC curve (AUC), and calibration assessment.
- **Model comparison:** Likelihood ratio testing, information criteria, and patient-level five-fold cross-validation to assess whether adding diagnosis group improves model performance.

## Cohort and Descriptive Findings

The initial descriptive analysis identified an eligible index-admission cohort of **1,443 admissions from 952 unique synthetic patients**.

| Measure | Result |
|---|---:|
| Eligible index admissions | 1,443 |
| Unique patients in eligible cohort | 952 |
| 30-day readmissions | 150 |
| Overall 30-day readmission rate | 10.40% |
| Elective admission readmission rate | 10.42% |
| Emergency admission readmission rate | 9.12% |
| Urgent non-emergency admission readmission rate | 13.27% |

Mean length of stay was approximately 4.98 days among admissions without a recorded 30-day readmission and 4.77 days among admissions with a recorded readmission.

These descriptive results are generated from simulated data. They should not be interpreted as actual hospital performance estimates or evidence of causal relationships.

## Readmission Modelling and Diagnostics

The primary complete-case modelling cohort contained **1,368 admissions from 922 unique patients**.

The adjusted logistic regression analysis found the following associations:

- **Age:** Adjusted odds ratio approximately 1.012 per additional year (p = 0.0024).
- **Comorbidity count:** Adjusted odds ratio approximately 1.162 per additional comorbid condition (p = 0.0058).
- **Urgent non-emergency admission:** Adjusted odds ratio approximately 1.524 (p = 0.054); this did not meet the conventional 5% significance threshold.

These are associations within a synthetic dataset, not clinical findings. Emergency admission is the reference category for admission type in Model A. The estimated odds ratios for Elective and Urgent non-emergency admissions are therefore interpreted relative to Emergency admissions.

Diagnostic assessment produced an apparent AUC of approximately **0.619** for the baseline adjusted model. In the diagnosis-group comparison, the apparent AUC was approximately 0.619 for Model A and 0.643 for Model B.

The likelihood ratio test comparing the models gave χ² = 9.83, with 8 degrees of freedom and p = 0.28. The additional diagnosis-group terms did not produce a statistically significant improvement according to this test.

Using the revised outcome-aware, patient-level five-fold cross-validation analysis, the mean AUC was approximately:

| Model | Mean cross-validated AUC | Standard deviation |
|---|---:|---:|
| Model A: without diagnosis group | 0.605 | 0.0587 |
| Model B: with diagnosis group | 0.609 | 0.0523 |

The mean AUC difference was approximately 0.004 in favour of Model B. This small difference suggests little improvement in cross-validated discrimination from adding diagnosis group in this synthetic analysis.

Apparent AUC and cross-validated AUC measure different aspects of model performance and should not be treated as interchangeable. These results do not establish clinical predictive validity or external generalisability.

## Length-of-Stay Analysis

Length of stay was modelled using a Gamma generalized linear model with a log link. A patient-clustered Gamma GEE model was also fitted to account for repeated admissions.

In the GEE analysis, the estimated length-of-stay ratio for urgent non-emergency admissions was approximately 0.907 (95% CI 0.820–1.00; p = 0.0556). This corresponds to an estimated 9.3% shorter stay relative to the model's reference category, but the result did not meet the conventional 5% significance threshold.

All model estimates are illustrative results from simulated data.

## Key Outputs

The project generates CSV result files and figures, including:

- `outputs/results/length_of_stay_by_readmission.csv`
- `outputs/results/readmission_by_admission_type.csv`
- `outputs/figures/length_of_stay_histogram.png`
- `outputs/figures/logistic_models_forest_plot.png`
- `outputs/results/gamma_los_gee_results.csv`
- `outputs/results/readmission_vif_diagnostics.csv`
- `outputs/results/readmission_influence_summary.csv`
- `outputs/results/readmission_top_influential_observations.csv`
- `outputs/results/readmission_influence_sensitivity.csv`
- `outputs/results/readmission_auc.csv`
- `outputs/results/readmission_calibration.csv`
- `outputs/results/diagnosis_model_comparison.csv`
- `outputs/results/diagnosis_model_auc_comparison.csv`
- `outputs/results/diagnosis_cross_validation_summary_balanced.csv`
- `outputs/results/diagnosis_final_model_comparison.csv`
- `outputs/results/diagnosis_model_B_adjusted_ORs.csv`

The Quarto report is available in `report/hospital_readmission_report.html`, with its source document in `report/hospital_readmission_report.qmd`.

## Reproducibility and Data Limitations

The workflow is organised into numbered R scripts and uses saved CSV files to support reproducibility. Run scripts from the project root and in the intended sequence.

The data-generation script recreates the synthetic cohort. Because generation uses random seeds, the script is designed to produce reproducible simulated data when run under the same conditions and software environment.

## R Package Requirements

The analysis uses R and the following packages:

- `tidyverse` — data preparation and manipulation
- `geepack` — generalized estimating equations (GEE)
- `pROC` — ROC curves and AUC estimation
- `broom` — tidying model results
- `car` — variance inflation factor (VIF) diagnostics

Install the required packages once in R before running the analysis:

```r
install.packages(c(
  "tidyverse",
  "geepack",
  "pROC",
  "broom",
  "car"
))
```

Open `Hospital_Readmission_RWE.Rproj` in RStudio and run the numbered scripts from the project root in the documented workflow order. The dataset is synthetic; no real patient data are used.

Important limitations include:

- The dataset is simulated and cannot establish real-world clinical associations.
- Selected missing values are deliberately introduced; complete-case modelling may change the analytical sample.
- Multiple admissions can belong to the same patient, requiring consideration of within-patient correlation and appropriate validation strategies.
- Apparent model performance may be optimistic compared with performance on new data.
- Patient-level cross-validation provides internal validation only; external validation has not been established.
- The synthetic data-generating mechanisms may not reflect the complexity of actual hospital populations or care pathways.

## Tools and Technologies

- R and RStudio
- Logistic regression and generalized linear models
- Generalized estimating equations (GEE)
- Model diagnostics and cross-validation
- CSV-based data management
- Quarto for reproducible reporting
- Data visualisation using R plotting packages

## Intended Audience

This portfolio project is intended for healthcare research, Real-World Evidence (RWE), clinical outcomes research, health services research, monitoring and evaluation, and applied health data analytics roles.

It demonstrates an end-to-end analytical workflow, from synthetic data generation to model assessment and reproducible reporting, while emphasising transparent interpretation and the limitations of simulated evidence.
## AI Use and Transparency

Generative AI (ChatGPT by OpenAI) was used as a supporting tool during project development to explain R code, assist with debugging, refine documentation, and support the organisation of the analytical workflow.

The synthetic data generation, statistical modelling, diagnostic procedures, and interpretation of results were reviewed iteratively. AI-generated suggestions were checked against the code and outputs, and the project author remains responsible for the accuracy, reproducibility, and conclusions of the published work.

All data in this project are simulated and do not represent real patients or actual hospital records.

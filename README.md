# HTN-301 Clinical Trial Analysis (using R)

## About This Project

This is a personal project where I used R to clean and analyze a simulated clinical trial dataset.

The dataset includes 600 patients with hypertension who were assigned to either a Drug or Placebo group. I wanted to see whether the drug was more effective at lowering blood pressure and whether it affected other outcomes.

## Why I Did This Project

I wanted to get more practice with R and statistics by working on a dataset that is similar to what might be used in clinical research.
While working on this project, I learned more about data cleaning, choosing appropriate statistical methods and interpreting the results. I also learned that missing data can affect the analysis and why it's important to consider it before drawing conclusions.

## What I Did

- Cleaned the dataset by handling duplicates, missing values and invalid measurements.
- Explored patient characteristics and created tables and visualizations.
- Compared blood pressure outcomes between the Drug and Placebo groups.
- Used ANCOVA, logistic regression, negative binomial regression and survival analysis.

## Main Results

- The Drug group had lower adjusted systolic blood pressure at week 12 compared to Placebo.
- Patients in the Drug group were more likely to achieve a blood pressure reduction of at least 10 mmHg.
- The Drug group had lower ER visit rates and a lower hazard of cardiovascular events.

These results are based on simulated data and should not be interpreted as evidence that a real drug is effective.

## Tools

- R
- dplyr, ggplot2, gtsummary
- Statistical modeling in R

## Files

- [Full Report](HTN301_Report.pdf) - includes the cleaning process, charts, statistical analysis, and results.
- [Data Cleaning](cleaning.R) - code used to clean the raw dataset.
- [Exploratory Data Analysis](EDA.R) - code for summary tables and visualizations.
- [Statistical Analysis](Analysis.R) - code for regression models and survival analysis.
- [Raw Dataset](htn_trial_raw.csv)
- [Clean Dataset](htn_trial_clean.csv)



This project uses simulated clinical trial data for learning purposes. It is not based on real patients or an actual clinical trial.

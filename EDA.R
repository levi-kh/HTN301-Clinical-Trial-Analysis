#Hypertension Trial (HTN-301)
#Exploratory Data Analysis


#1. Load the libraries into R session
# ------------------------------------------------------------
library(readr)
library(dplyr)
library(gtsummary)
library(tidyr)
library(ggplot2)

#2. Read the clean Dataset from the cleaning part
# ------------------------------------------------------------
df <- read_csv("htn_trial_clean.csv")

#Set the factor levels again (read_csv reads them as text)
df <- df %>%
  mutate(
    arm = factor(arm, levels = c("Placebo", "Drug")),
    smoker = factor(smoker, levels = c("No", "Yes")),
    diabetes = factor(diabetes, levels = c("No", "Yes"))
  )

#3. Create the baseline characteristics table (before treatment)
# ------------------------------------------------------------

baseline_table <- df %>%
  select(
    arm, age, sex, weight_kg, height_cm,
    baseline_sbp, smoker, diabetes,
    crp_mg_l_num, region
  ) %>%
  tbl_summary(
    by = arm,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      crp_mg_l_num ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "ifany"
  )
  
baseline_table  
  
#4. Create histograms and boxplots 
#   to see the distributions and outliers
# ------------------------------------------------------------

#Select numerical variables
numeric_data <- df %>%
  select(
    age, weight_kg, height_cm, crp_mg_l_num,
    baseline_sbp, sbp_wk4, sbp_wk8,
    sbp_wk12, sbp_wk24,
    adherence_pct, er_visits, time_months
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  )

#Histograms for patients' characteristics
numeric_data %>%
  filter(variable %in% c(
    "age", "weight_kg", "height_cm",
    "crp_mg_l_num", "adherence_pct"
  )) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 25, fill = "steelblue",
                 color = "white") +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  labs(
    title = "Distribution of Patient Characteristics",
    x = "Value",
    y = "Frequency"
  ) +
  theme_minimal()
  
#Histograms for SBP measurements
numeric_data %>%
  filter(variable %in% c(
    "baseline_sbp", "sbp_wk4", "sbp_wk8",
    "sbp_wk12", "sbp_wk24"
  )) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 25, fill = "steelblue",
                 color = "white") +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  labs(
    title = "Distribution of SBP Measurements",
    x = "SBP (mmHg)",
    y = "Frequency"
  ) +
  theme_minimal()
  
#Histogram for Follow-up time
ggplot(df, aes(x = time_months)) +
  geom_histogram(
    binwidth = 1,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Distribution of Follow-up time",
    x = "Follow-up time (months)",
    y = "Number of patients"
  )+
  theme_minimal()
  
#Bar chart for ER visits
ggplot(df, aes(x = factor(er_visits))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribution of ER Visits",
    x = "Number of ER Visits",
    y = "Number of Patients"
  ) +
  theme_minimal()


#5. Explore the outcomes (before statistical testing)
# ------------------------------------------------------------

#5.1 SBP over time by arm

#Calculate mean SBP and 95% CI at each visit, by arm
sbp_time <- df %>%
  select(arm, baseline_sbp, sbp_wk4, sbp_wk8, sbp_wk12, sbp_wk24) %>%
  pivot_longer(
    cols = -arm,
    names_to = "visit",
    values_to = "sbp"
  ) %>%
  mutate(
    week = case_when(
      visit == "baseline_sbp" ~ 0,
      visit == "sbp_wk4" ~ 4,
      visit == "sbp_wk8" ~ 8,
      visit == "sbp_wk12" ~ 12,
      visit == "sbp_wk24" ~ 24
    )
  ) %>%
  group_by(arm, week) %>%
  summarise(
    n = sum(!is.na(sbp)),
    mean_sbp = mean(sbp, na.rm = TRUE),
    se = sd(sbp, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

sbp_time

#Line chart for mean SBP over time
ggplot(sbp_time, aes(x = week, y = mean_sbp, color = arm)) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(ymin = mean_sbp - 1.96 * se, ymax = mean_sbp + 1.96 * se),
    width = 0.5
  ) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 24)) +
  labs(
    title = "Mean SBP Over Time by Arm",
    x = "Week",
    y = "Mean SBP (mmHg)",
    color = "Arm"
  ) +
  theme_minimal()

#5.2 Change in SBP from baseline to week 12

df <- df %>%
  mutate(change_wk12 = sbp_wk12 - baseline_sbp)

#Mean and SD of the change by arm
df %>%
  filter(!is.na(change_wk12)) %>%
  group_by(arm) %>%
  summarise(
    n = n(),
    mean_change = mean(change_wk12),
    sd_change = sd(change_wk12)
  )

#Boxplot for change in SBP at week 12
df %>%
  filter(!is.na(change_wk12)) %>%
  ggplot(aes(x = arm, y = change_wk12, fill = arm)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Change in SBP from Baseline to Week 12",
    x = "Arm",
    y = "Change in SBP (mmHg)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#5.3 Responder rate
#Responder = SBP decreases by at least 10 mmHg from baseline to week 12

df <- df %>%
  mutate(
    responder = case_when(
      is.na(change_wk12) ~ NA_character_,
      change_wk12 <= -10 ~ "Responder",
      TRUE ~ "Non-responder"
    )
  )

#Responder rate by arm (only patients with week 12 SBP)
responder_rate <- df %>%
  filter(!is.na(responder)) %>%
  count(arm, responder) %>%
  group_by(arm) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

responder_rate

#Bar chart for responder rate
responder_rate %>%
  filter(responder == "Responder") %>%
  ggplot(aes(x = arm, y = pct, fill = arm)) +
  geom_col() +
  labs(
    title = "Responder Rate by Arm (SBP drop of at least 10 mmHg at Week 12)",
    x = "Arm",
    y = "Responders (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#5.4 ER visits

#Average ER visits and ER visits per 100 patient-months, by arm
df %>%
  filter(!is.na(er_visits)) %>%
  group_by(arm) %>%
  summarise(
    patients = n(),
    mean_er_visits = mean(er_visits),
    er_per_100_patient_months = sum(er_visits) / sum(time_months) * 100
  )

#Bar chart for ER visits by arm
df %>%
  filter(!is.na(er_visits)) %>%
  ggplot(aes(x = factor(er_visits), fill = arm)) +
  geom_bar(position = "dodge") +
  labs(
    title = "ER Visits by Arm",
    x = "Number of ER Visits",
    y = "Number of Patients",
    fill = "Arm"
  ) +
  theme_minimal()

#5.5 CV events

#Number and percentage of patients with a CV event, by arm
table(df$arm, df$event)
round(prop.table(table(df$arm, df$event), margin = 1) * 100, 1)

#Bar chart for CV events
df %>%
  group_by(arm) %>%
  summarise(pct_event = mean(event) * 100) %>%
  ggplot(aes(x = arm, y = pct_event, fill = arm)) +
  geom_col() +
  labs(
    title = "Patients with a CV Event by Arm",
    x = "Arm",
    y = "Patients with a CV event (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


#6. Missing data and dropout
# ------------------------------------------------------------

#6.1 Missing SBP by visit and arm

#Percentage of missing SBP at each visit
sbp_missing <- df %>%
  select(arm, sbp_wk4, sbp_wk8, sbp_wk12, sbp_wk24) %>%
  pivot_longer(
    cols = -arm,
    names_to = "visit",
    values_to = "sbp"
  ) %>%
  mutate(
    week = case_when(
      visit == "sbp_wk4" ~ 4,
      visit == "sbp_wk8" ~ 8,
      visit == "sbp_wk12" ~ 12,
      visit == "sbp_wk24" ~ 24
    )
  ) %>%
  group_by(arm, week) %>%
  summarise(pct_missing = mean(is.na(sbp)) * 100, .groups = "drop")

sbp_missing

#Line chart for missing SBP
ggplot(sbp_missing, aes(x = week, y = pct_missing, color = arm)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(4, 8, 12, 24)) +
  labs(
    title = "Missing SBP at Each Visit by Arm",
    x = "Week",
    y = "Patients with missing SBP (%)",
    color = "Arm"
  ) +
  theme_minimal()

#6.2 Reason for end of follow-up by arm

#Number and percentage of patients by end reason
table(df$arm, df$end_reason)
round(prop.table(table(df$arm, df$end_reason), margin = 1) * 100, 1)

#Bar chart for end reason by arm
ggplot(df, aes(x = arm, fill = end_reason)) +
  geom_bar(position = "fill") +
  labs(
    title = "Reason for End of Follow-up by Arm",
    x = "Arm",
    y = "Proportion of patients",
    fill = "End reason"
  ) +
  theme_minimal()

#6.3 Patients with and without week 12 SBP

#Compare baseline characteristics of the two groups of patients
missing_table <- df %>%
  mutate(
    wk12_status = ifelse(is.na(sbp_wk12), "Missing week 12 SBP", "Has week 12 SBP")
  ) %>%
  select(
    wk12_status, arm, age, sex, baseline_sbp,
    smoker, diabetes, crp_mg_l_num, region
  ) %>%
  tbl_summary(
    by = wk12_status,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      crp_mg_l_num ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "ifany"
  )

missing_table

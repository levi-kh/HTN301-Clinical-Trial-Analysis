#Hypertension Trial (HTN-301)
#Cleaning

#1. Load the libraries into R session
# ------------------------------------------------------------
library(readr)
library(dplyr)
library(stringr)

#2. Read the Dataset
# ------------------------------------------------------------
df <- read_csv("htn_trial_raw.csv")

#3. Initial data inspection
# ------------------------------------------------------------

#Check the first 6 rows
head(df)

#Check dataset dimensions
dim(df)


#4. Create the data dictionary
# ------------------------------------------------------------
data_dictionary <- data.frame(
  variable = names(df),
  r_type = sapply(df, function(x) class(x)[1]),
  analysis_type = NA_character_
)

data_dictionary <- data_dictionary %>%
  mutate(
    analysis_type = case_when(
      variable == "patient_id" ~ "nominal",
      variable == "site" ~ "nominal",
      variable == "region" ~ "nominal",
      variable == "arm" ~ "binary",
      variable == "sex" ~ "binary",
      variable == "age" ~ "continuous",
      variable == "weight_kg" ~ "continuous",
      variable == "height_cm" ~ "continuous",
      variable == "smoker" ~ "binary",
      variable == "diabetes" ~ "binary",
      variable == "crp_mg_l" ~ "continuous",
      variable == "baseline_sbp" ~ "continuous",
      variable == "sbp_wk4" ~ "continuous",
      variable == "sbp_wk8" ~ "continuous",
      variable == "sbp_wk12" ~ "continuous",
      variable == "sbp_wk24" ~ "continuous",
      variable == "adherence_pct" ~ "continuous",
      variable == "er_visits" ~ "count",
      variable == "time_months" ~ "time-to-event",
      variable == "event" ~ "binary",
      variable == "end_reason" ~ "nominal",
      TRUE ~ NA_character_
    )
  )

#5. Handle CRP Values
# ------------------------------------------------------------

#Check rows with "<0.2"
sum(df$crp_mg_l == "<0.2")  # 6 rows

#Create a numeric CRP column, convert "<0.2" to NA
df$crp_mg_l_num <- as.numeric(
  ifelse(df$crp_mg_l == "<0.2", NA, df$crp_mg_l)
)

#6. Check and remove duplicate patient IDs
# ------------------------------------------------------------

#Check for duplicate patient IDs
df %>%
  count(patient_id) %>%
  filter(n > 1)

#Remove duplicate patient IDs
df <- df %>%
  distinct(patient_id, .keep_all = TRUE)


#7. Inspect categorical variables
# ------------------------------------------------------------

#Print distinct values and counts, including missing values
vars <- c("arm", "sex", "smoker", "diabetes", "region")

for (v in vars) {
  cat("\n---", v, "---\n")
  print(table(df[[v]], useNA = "ifany"))
}


#8. Standardize categorical labels
# ------------------------------------------------------------

df <- df %>%
  mutate(
    #trim spaces and convert blank strings to NA
    across(
      all_of(c("arm", "sex", "smoker", "diabetes", "region")),
      ~ na_if(str_trim(as.character(.x)), "")
    ),
    
    #standardize arm labels
    arm = case_when(
      str_to_lower(arm) == "drug" ~ "Drug",
      str_to_lower(arm) == "placebo" ~ "Placebo",
      TRUE ~ arm
    ),
    
    #standardize sex labels
    sex = case_when(
      str_to_lower(sex) %in% c("f", "female") ~ "Female",
      str_to_lower(sex) %in% c("m", "male") ~ "Male",
      TRUE ~ sex
    ),
    
    #standardize smoker labels
    smoker = case_when(
      str_to_lower(smoker) %in% c("n", "no") ~ "No",
      str_to_lower(smoker) %in% c("y", "yes") ~ "Yes",
      TRUE ~ smoker
    ),
    
    #standardize diabetes labels
    diabetes = case_when(
      str_to_lower(diabetes) %in% c("n", "no") ~ "No",
      str_to_lower(diabetes) %in% c("y", "yes") ~ "Yes",
      TRUE ~ diabetes
    ),
    
    #standardize region labels
    region = case_when(
      str_to_lower(region) == "asia" ~ "Asia",
      str_to_lower(region) == "europe" ~ "Europe",
      region == "N. America" ~ "North America",
      TRUE ~ region
    )
  )


#9. Convert categorical variables to factors, with reference levels
# ------------------------------------------------------------

df <- df %>%
  mutate(
    arm = factor(arm, levels = c("Placebo", "Drug")),
    sex = factor(sex),
    smoker = factor(smoker, levels = c("No", "Yes")),
    diabetes = factor(diabetes, levels = c("No", "Yes")),
    region = factor(region)
  )

#10. Find impossible or suspicious values
# ------------------------------------------------------------

#Quick inspection
summary(df)

#Check unusual age values
df %>%
  filter(age < 18 | age >= 100 | is.na(age)) %>%
  select(patient_id, age)

#Check unusual height values
df %>%
  filter(height_cm < 100 | is.na(height_cm)) %>%
  select(patient_id, height_cm)

#Check unusual SBP values
df %>%
  filter(sbp_wk8 < 70 | sbp_wk8 > 250 |
           sbp_wk12 < 70 | sbp_wk12 > 250) %>%
  select(patient_id, sbp_wk8, sbp_wk12)


#11. Create a clean version of the dataset named df_clean
# ------------------------------------------------------------

df_clean <- df %>%
  mutate(
    
    #age
    age = ifelse(age < 18 | age >= 100, NA, age),
    
    #height
    height_cm = ifelse(
      height_cm < 3 | is.na(height_cm),
      height_cm * 100,
      height_cm
    ),
    
    #sbp at week 8
    sbp_wk8 = ifelse(sbp_wk8 < 70 | sbp_wk8 > 250,
                     NA, sbp_wk8),
    
    #sbp at week 12
    sbp_wk12 = ifelse(sbp_wk12 < 70 | sbp_wk12 > 250,
                      NA, sbp_wk12)
    
  )

#Save df_clean as a csv file
write.csv(df_clean, "htn_trial_clean.csv", row.names = FALSE)

#12. Check missing SBP by treatment arm
# ------------------------------------------------------------

df_clean %>%
  group_by(arm) %>%
  summarise(
    total = n(),
    missing_wk4 = sum(is.na(sbp_wk4)),
    missing_wk8 = sum(is.na(sbp_wk8)),
    missing_wk12 = sum(is.na(sbp_wk12)),
    missing_wk24 = sum(is.na(sbp_wk24))
  )


#13. Check how the missing values are related to end reason
# ------------------------------------------------------------
table(
  df_clean$end_reason,
  is.na(df_clean$sbp_wk12)
)


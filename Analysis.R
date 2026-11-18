#Hypertension Trial (HTN-301)
#Statistical Analysis


#1. Load the libraries into R session
# ------------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(survival)

#2. Read the clean Dataset from the cleaning part
# ------------------------------------------------------------
df <- read_csv("htn_trial_clean.csv")

#Set the factor levels again (Placebo is the reference group)
df <- df %>%
  mutate(
    arm = factor(arm, levels = c("Placebo", "Drug")),
    sex = factor(sex),
    smoker = factor(smoker, levels = c("No", "Yes")),
    diabetes = factor(diabetes, levels = c("No", "Yes")),
    region = factor(region)
  )

#3. Does the drug lower SBP more than placebo at week 12?
# ------------------------------------------------------------

#Mean SBP at week 12 by arm (before adjusting)
df %>%
  filter(!is.na(sbp_wk12)) %>%
  group_by(arm) %>%
  summarise(
    n = n(),
    mean_sbp = mean(sbp_wk12),
    sd_sbp = sd(sbp_wk12)
  )

#ANCOVA: week 12 SBP by arm, adjusting for baseline SBP
ancova_model <- lm(sbp_wk12 ~ arm + baseline_sbp, data = df)
summary(ancova_model)

#Adjusted treatment difference (Drug - Placebo) and 95% CI
coef(ancova_model)["armDrug"]
confint(ancova_model)["armDrug", ]

#Check the model: residuals should be spread evenly and follow a straight line
par(mfrow = c(1, 2))
plot(ancova_model, which = 1:2)
par(mfrow = c(1, 1))

#4. Do more patients respond on the drug?
# ------------------------------------------------------------

#Responder = SBP decreases by at least 10 mmHg from baseline to week 12
df <- df %>%
  mutate(
    change_wk12 = sbp_wk12 - baseline_sbp,
    responder = ifelse(change_wk12 <= -10, 1, 0)
  )

#Response rate by arm (patients with missing week 12 SBP are left out)
df %>%
  filter(!is.na(responder)) %>%
  group_by(arm) %>%
  summarise(
    n = n(),
    responders = sum(responder),
    response_rate = round(mean(responder) * 100, 1)
  )

#Logistic regression, Placebo is the reference
response_model <- glm(responder ~ arm, data = df, family = binomial)
summary(response_model)

#Odds ratio and 95% CI
exp(cbind(OR = coef(response_model), confint(response_model)))["armDrug", ]

#5. Does the drug reduce ER visits?
# ------------------------------------------------------------

#Average and variance of ER visits by arm
df %>%
  group_by(arm) %>%
  summarise(
    mean_er = mean(er_visits, na.rm = TRUE),
    var_er = var(er_visits, na.rm = TRUE)
  )

#Poisson regression, follow-up time is used as exposure
poisson_model <- glm(
  er_visits ~ arm + offset(log(time_months)),
  data = df,
  family = poisson
)
summary(poisson_model)

#Check overdispersion (a value much bigger than 1 means the variance is too large)
sum(residuals(poisson_model, type = "pearson")^2) / poisson_model$df.residual

#Overdispersion is present, so use negative binomial regression
nb_model <- MASS::glm.nb(
  er_visits ~ arm + offset(log(time_months)),
  data = df
)
summary(nb_model)

#Incidence rate ratio (IRR) and 95% CI
exp(cbind(IRR = coef(nb_model), confint(nb_model)))["armDrug", ]

#6. Does the drug delay CV events?
# ------------------------------------------------------------

#Kaplan-Meier curves (patients who left for other reasons are censored)
km_fit <- survfit(Surv(time_months, event) ~ arm, data = df)
km_fit

#Event-free probability at 12 and 24 months
summary(km_fit, times = c(12, 24))

#Put the curves in a data frame for ggplot
km_data <- data.frame(
  time = km_fit$time,
  survival = km_fit$surv,
  arm = rep(c("Placebo", "Drug"), km_fit$strata)
)

#Add the starting point (time 0, event-free = 100%)
km_data <- bind_rows(
  data.frame(time = 0, survival = 1, arm = c("Placebo", "Drug")),
  km_data
)

#Kaplan-Meier chart
ggplot(km_data, aes(x = time, y = survival, color = arm)) +
  geom_step() +
  labs(
    title = "Time to First CV Event by Arm",
    x = "Time (months)",
    y = "Probability of staying event-free",
    color = "Arm"
  ) +
  theme_minimal()

#Log-rank test
survdiff(Surv(time_months, event) ~ arm, data = df)

#Cox proportional hazards regression
cox_model <- coxph(Surv(time_months, event) ~ arm, data = df)
summary(cox_model)

#Check the proportional hazards assumption (p > 0.05 is good)
cox.zph(cox_model)

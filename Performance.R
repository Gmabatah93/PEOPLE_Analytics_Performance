library(readr)
library(dplyr)
library(ggplot2)
library(infer)
library(broom)

# PERFROMANCE: ----

## Data
performance <- read_csv("Data/performance_data.csv")
hr_data <- read_csv("Data/hr_data.csv")
# - join
hr_perfromance <- hr_data %>% 
  left_join(performance, by = "employee_id")

## Exploratory Data Analysis
# - summary
hr_perfromance %>% skimr::skim()
hr_perfromance %>% glimpse()
hr_perfromance %>% count(department)
hr_perfromance %>% count(job_level)
hr_perfromance %>% count(gender)

### Visual: Average Ratings by Gender
hr_perfromance %>% 
  group_by(gender) %>% 
  summarise(avg_ratings = mean(rating)) %>% 
  ggplot(aes(gender, avg_ratings)) +
  geom_col() + geom_hline(yintercept = 2.83, color = "red") +
  ggtitle("Avg Rating ~ Gender")

### Visual: Proportion of High Performanced by Gender
hr_perfromance %>% 
  mutate(high_performer = factor(ifelse(rating >= 4, 1, 0))) %>% 
  ggplot(aes(gender, fill = high_performer)) +
  geom_bar(position = "fill") +
  ggtitle("Are males ranked favorably as opposed to women ?")
# - stat test: Significant
hr_perfromance %>% 
  mutate(high_performer = factor(ifelse(rating >= 4, 1, 0))) %>% 
  chisq_test(high_performer ~ gender)

### Visual: Job Distribution By Gender
hr_perfromance %>% 
  ggplot(aes(gender, fill = job_level)) +
  geom_bar(position = "fill") +
  ggtitle("Job Distribution by Gender Significant ?")
# - stat test: Significant
hr_perfromance %>% chisq_test(job_level ~ gender)

### Visual: High Performance by Gender factored by Job Level
hr_perfromance %>% 
  mutate(high_performer = factor(ifelse(rating >= 4, 1, 0))) %>% 
  ggplot(aes(gender, fill = high_performer)) +
  geom_bar(position = "fill") +
  facet_wrap(~ job_level) +
  ggtitle("Are males ranked favorably as opposed to women ~ Job Level ?")
# - stat test
hr_perfromance %>% 
  mutate(high_performer = factor(ifelse(rating >= 4, 1, 0))) %>% 
  glm(high_performer ~ gender + job_level, family = "binomial", data = .) %>% 
  tidy(exponentiate = TRUE)
# -- Male: Significant
# -- Hourly: Significant
# -- Manager: Significant
# -- Salaried: Significant

# PATH Analysis: ----

## Data
pm <- read_csv("Data/PerfMgmtRewardSystemsExample.csv")

## Exploratory Data Analysis
# - summary
pm %>% skimr::skim()
pm %>% glimpse()
# - Feature Engineering
pm <- pm %>% 
  mutate(Perf_Overall = ( Perf_Qual + Perf_Prod + Perf_Effort + Perf_Qual ) / 4)

## Visual: Correlation Matrix
pm %>% select(SalesRevenue, everything(), -Sex, EmpID) %>% cor() %>% 
  ggcorrplot::ggcorrplot(method = "circle", type = "lower")

## Visual: Sales Revenue By Sex
pm %>% 
  ggplot(aes(Sex, SalesRevenue)) +
  geom_boxplot() +
  ggtitle("Sales Revenue ~ Sex")
# - stat test: Not Significant
pm %>% t_test(SalesRevenue ~ Sex)

## Visual: Sales Revenue By Performance factored by Education Level
pm %>% 
  ggplot(aes(Perf_Overall, SalesRevenue)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE,
              aes(color = factor(EducationLevel))) +
  ggtitle("Sale Revenue ~ Overall Peformance")
# - stat tests: Significant
lm(SalesRevenue ~ Perf_Overall, data = pm) %>% tidy()
lm(SalesRevenue ~ Perf_Overall + factor(EducationLevel), data = pm) %>% tidy()

# MEDIATION: Indirect Effect ----
library(lavaan)

## Data
pb <- read_csv("Data/PlannedBehavior.csv")
## Exploratory Data Analysis
# - summary
pb %>% skimr::skim()
pb %>% glimpse()

## Path Analysis
spec_mod <- "
# Path c' (Direct Effect)
behavior ~ c*attitude

# Path a
intention ~ a*attitude

# Path b
behavior ~ b*intention

# Indirect Effect (a * b)
ab := a*b
"
mod <- sem(model = spec_mod, data = pb)
mod %>% summary(fit.measures = TRUE, rsquare = TRUE)

## Path Analysis: Resampling (Percentile Bootstrap)
set.seed(2019)
mod_2 <- sem(spec_mod, data = pb,
             se = "bootstrap", bootstrap = 100)
parameterEstimates(mod_2, ci = TRUE, level = 0.95, boot.ci.type = "perc")


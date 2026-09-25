### Project:    QolReg
### PI:         Peter H
### Co-authors: Karl BC, Magnus J, Emma S, Kristofer Å, Fredrik G
### Created:    2026-02-11
### Revised:    2026-09-25
###
### The final analysis file (cisiAnalysis.csv) was created under MacOS
### Tahoe 26.7, R version 4.6.1, and RStudio 2026.09.0+174


# PACKAGES ----
library(readr)
library(readxl)
library(tidyverse)
library(lubridate)
library(gtsummary)
library(skimr)

### some commands exist in multiple packages, here we define preferred ones that are frequently used
select <- dplyr::select
count <- dplyr::count
recode <- car::recode
rename <- dplyr::rename

# Random number generator (the analysis file was bases on MacOS )
set.seed(12984563)

# the seed needs to be set again just before any slice_sample() use


# DATA MANAGEMENT
# Read data ----
# Demographic variables and person factors
patient <- read_excel("data/PARKreg_patient.xlsx")
diagnosis <- read_excel("data/PARKreg_patient_diagnosis.xlsx")
therapy <- read_excel("data/PARKreg_therapy_parkinson.xlsx")
visit <- read_excel("data/PARKreg_visit_parkinson.xlsx")
updrs3 <- read_excel("data/PARKreg_updrs3.xlsx")

skim(diagnosis)
diagnosis %>%
  count(calc_patient_diagnosis_text) %>%
  print(n = 100)

diagnosis %>%
  count(year(patient_diagnosis_report_date)) %>%
  print(n = 100)

diagnosis %>%
  filter(year(patient_diagnosis_report_date) > 2021) %>%
  count(calc_patient_diagnosis_text) %>%
  print(n = 100)

# Instruments
cisi <- read_excel("data/PARKreg_cisipd.xlsx")

# These can wait
als_mnd_had <- read_excel("data/PARKreg_als_mnd_had.xlsx")
eq5d <- read_excel("data/PARKreg_eq5d.xlsx")
eq5d5l <- read_excel("data/PARKreg_eq5d5l.xlsx")
parkinson_mds_updrs.xlsx <- read_excel("data/PARKreg_parkinson_mds_updrs.xlsx")
parkinson_nmsquery <- read_excel("data/PARKreg_parkinson_nmsquery.xlsx")
pdq8 <- read_excel("data/PARKreg_pdq8.xlsx")
pdss <- read_excel("data/PARKreg_pdss.xlsx")
propd <- read_excel("data/PARKreg_propd.xlsx")
updrs <- read_excel("data/PARKreg_updrs.xlsx")
updrs3 <- read_excel("data/PARKreg_updrs3.xlsx")
work2 <- read_excel("data/PARKreg_work2.xlsx")


# Data cleaning ----
## Patient (unique id), age and sex ----
pat <- patient %>%
  select(RandID, report_patient_age, prs_sex_) %>%
  rename(
    id = RandID,
    age = report_patient_age,
    sex = prs_sex_) %>%
  mutate(
    id = as.numeric(id),
    age = as.numeric(age),
    sex = factor(sex))


##  Diagnosis (unique id), date ----
diagPDonly <- diagnosis %>%
  select(RandID, patient_diagnosis_report_date, calc_patient_diagnosis_text) %>%
  rename(
    id = RandID,
    diagDate = patient_diagnosis_report_date,
    diagText = calc_patient_diagnosis_text) %>%
  mutate(
    id = as.numeric(id),
    diagDate = as.Date(diagDate),
    diagText = as.factor(diagText)) %>%
  filter(diagText == "Parkinsons sjukdom") %>%  # new line
  select(!diagText)

diag <- diagnosis %>%
  select(RandID, patient_diagnosis_report_date,
         calc_patient_diagnosis_text) %>%
  rename(
    id = RandID,
    diagDate = patient_diagnosis_report_date,
    diagText = calc_patient_diagnosis_text) %>%
  mutate(
    id = as.numeric(id),
    diagDate = as.Date(diagDate),
    diagText = as.factor(diagText))

# which unique id's have diagText = Parkinsons
parkinson_ids <- diag %>%
  filter(diagText == "Parkinsons sjukdom") %>%
  distinct(id)

# only include those id's
diagPd <- diag %>%
  filter(id %in% parkinson_ids$id)

# df with id and diagnosis text
diag <- diagPd %>%
  mutate(value = 1) %>% # this I am unsure of what it does
  distinct(id, diagText, .keep_all = TRUE) %>%
  tidyr::pivot_wider(
    id_cols = c(id,diagDate),
    names_from = diagText,
    values_from = value,
    values_fill = 0)

# review the comorbidities
#library(gtsummary)
# print Table 1 style information (not necessary)
#tbl_summary(diag, missing_text = "Missing data",
#            include = c(!id))


## Therapy (kvarstående problem med dubbla rader) ----
drug <- therapy %>%
  select(RandID, drg_name, start_date, end_date) %>%
  rename(
    id = RandID,
    drug = drg_name,
    startDate = start_date,
    endDate = end_date) %>%
  mutate(
    id = as.numeric(id),
    drug = as.factor(drug),
    startDate = as.Date(startDate),
    endDate = as.Date(endDate))


## CISIPD (kvarstående problem med dubbla rader) ----
cisipd <- cisi %>%
  select(
    RandID, cisipd_date, cisipd_motor_signs, cisipd_disability,
    cisipd_motor_complications, cisipd_cognitive_status, calc_cisipd) %>%
  rename(
    id = RandID,
    cisipdDate = cisipd_date,
    motorSigns = cisipd_motor_signs,
    disability = cisipd_disability,
    motorComplications = cisipd_motor_complications,
    cognition = cisipd_cognitive_status,
    cisipdSum = calc_cisipd) %>%
  mutate(
    id = as.numeric(id),
    cisiDate = as.Date(cisipdDate),
    motorSigns = as.numeric(motorSigns),
    disability = as.numeric(disability),
    motorComplications = as.numeric(motorComplications),
    cognition = as.numeric(cognition),
    cisipdSum = as.numeric(cisipdSum))


# Demographics ------------------------------------------------------------

## gender -----
pat %>%
  group_by(id) %>%
  slice(1) %>% # include only the first instance of each id in data
  ungroup() %>%
  count(sex) # 11 missing data

library(ggdist) # for stat_slabinterval in figure
## age ----
pat %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup() %>%
  reframe(median_qi(age, na.rm = T, .width = c(.5,.95)))
pat %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup() %>%
  filter(age < 120) %>%
  ggplot(aes(age)) +
  #geom_histogram(color ="white") +
  stat_slabinterval(aes(slab_fill = after_stat(level)), .width = c(0.5,0.95)) +
  scale_fill_brewer(aesthetics = "slab_fill", na.value = "lightblue1") +
  theme_bw()


# CISI --------------------------------------------------------------------

## range of data collection time points in calendar years ----
cisipd %>%
  filter(cisiDate > 2012) |> # only include 2012 and later
  ggplot(aes(x=cisiDate)) +
  geom_histogram()

# histogram still includes earlier dates for some reason?
cisipd %>%
  filter(cisiDate > 2012) |>
  reframe(summary(cisiDate))

summary(cisipd$cisiDate) # unfiltered

# how many unique id's between 2022 and end of 2025?
set.seed(12984563)
cisipd %>%
  filter(between(cisiDate, as.Date("2022-01-01"), as.Date("2025-12-31"))) %>%
  group_by(id) %>%
  slice_sample(n = 1) %>%
  ungroup() %>%
  nrow() # 3221

# how many of the unique id's between 2022 and end of 2025 have multiple measurements
cisipd %>%
  filter(between(cisiDate, as.Date("2022-01-01"), as.Date("2025-12-31"))) %>%
  group_by(id) %>%
  count() %>%
  filter(n > 1) %>%
  nrow() # 1528

# how many measurements?
cisipd %>%
  filter(between(cisiDate, as.Date("2022-01-01"), as.Date("2025-12-31"))) %>%
  group_by(id) %>%
  count() %>%
  ungroup() %>%
  filter(n > 1) %>%
  summarise(median = median(n),
            max = max(n),
            IQR = IQR(n))
# median = 3, max = 8, IQR = 1

# slice the first instance of each id, then take the first 20 id's
cisi20 <- cisipd %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup() %>%
  slice(1:20)

# just looking at the corresponding id's in diag df
diag20 <- diag %>%
  filter(id %in% cisi20$id) %>%
  select(id,diagDate)

# join the first 20 and review the date variables
cisi20 %>% left_join(diag20, by = "id") %>%
  select(id,cisiDate,diagDate)

# 3/20 have missing diagDate

# join df's cisipd and drug, base the join on cisipd, use one random instance of each id that has multiple occurrences
set.seed(12984563)
drugCisi <- cisipd %>%
  group_by(id) %>%
  slice_sample(n = 1) %>%
  ungroup() %>%
  right_join(drug, by = "id")

# this df contains unique cisi ratings, but multiple drugs for some id's
drugCisiUnique <-
  drugCisi %>%
  filter(between(cisiDate, startDate, endDate))

# n = 5890

# library(gtsummary)
# tbl_summary(drugCisiUnique)

#write_csv(drugCisiUnique, "data/drugCisiUnique.csv")
set.seed(12984563)
cisiAnalysisSample <- drugCisiUnique %>%
  select(id,cisiDate,motorSigns,disability,motorComplications,cognition) %>%
  na.omit() %>%
  group_by(id) %>%
  slice_sample(n = 1) %>%
  ungroup() # n = 2296

# add age and sex variables to analysis sample
cisiAnalysisSample <- cisiAnalysisSample %>%
  left_join(pat, by = "id")

# add diagDate and cisiDate to a temporary df ("2" at the end)
cisiAnalysisSample2 <- cisiAnalysisSample |>
  left_join(diag |>
              select(id,diagDate)
            ) |>
  mutate(diag_cisi_date_yrs = abs(difftime(cisiDate, diagDate, units = "weeks"))/52)

# check for duplicate id's
dupes <- cisiAnalysisSample2 |>
  count(id) |>
  filter(n > 1) |>
  pull(id) # n = 18 duplicates

# subset the first instance of each duplicate to a temporary df
unduped_df <- cisiAnalysisSample2 |>
  filter(id %in% dupes) |>
  arrange(id,diagDate) |>
  group_by(id) |>
  slice(1) |>
  ungroup()

# remove the duplicates in the main df
cisiAnalysisSample2 <- cisiAnalysisSample2 |>
  filter_out(id %in% dupes)

# row bind the main and unduped df and go back to the original df name without "2" at the end
cisiAnalysisSample <- rbind(cisiAnalysisSample2,unduped_df)
# n = 2278
# double check for dupes
cisiAnalysisSample |>
  count(id) |>
  filter(n > 1)

cisiAnalysisSample |>
  filter(is.na(diagDate)) |>
  nrow() # 213 missing

cisiAnalysisSample |>
  filter(is.na(sex)) |>
  nrow() # 1 missing

# removing those with missing diagDate and sex
cisiAnalysisSample <- cisiAnalysisSample |>
  drop_na(diagDate,sex) # 2083

summary(cisiAnalysisSample$diag_cisi_date_yrs)

# per week
ggplot(cisiAnalysisSample,aes(x = diag_cisi_date_yrs)) +
  geom_histogram()

# Saving the final data file for analysis of the CISI
write_csv(cisiAnalysisSample, glue::glue("data/cisiAnalysis_{Sys.Date()}.csv"))

cisiAnalysisSample |>
  select(!c(id,motorSigns,motorComplications,disability,cognition,
            diagDate)) |>
  tbl_summary(label = list(age = "Age",
                           sex = "Sex"))


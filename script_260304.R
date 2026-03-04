### Project: KvalReg
### # PI:       Peter H
### Co-authors: Karl BC, Magnus J, Emma S, Kristofer Å, Fredrik G
### Script:     Kristofer Å
### Created:    2026-02-11
### Revised:    2026-03-04

# PACKAGES ----
library(readr)
library(readxl)
#library(summarytools)
library(tidyverse)
library(lubridate)
library(gtsummary)
library(skimr)
set.seed(12984563)
### some commands exist in multiple packages, here we define preferred ones that are frequently used
select <- dplyr::select
count <- dplyr::count
recode <- car::recode
rename <- dplyr::rename



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
## Patient (unique id) ----
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


##  Diagnosis (unique id) ----
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

parkinson_ids <- diag %>%
  filter(diagText == "Parkinsons sjukdom") %>%
  distinct(id)

diagPd <- diag %>%
  filter(id %in% parkinson_ids$id)

diag <- diagPd %>%
  mutate(value = 1) %>%
  distinct(id, diagText, .keep_all = TRUE) %>%
  tidyr::pivot_wider(
    id_cols = c(id,diagDate),
    names_from = diagText,
    values_from = value,
    values_fill = 0)

# review the comorbidities
gtsummary::tbl_summary(diag)


## Therapy (kvarstående problem emd dubbla rader) ----
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



## CISIPD (kvarstående problem emd dubbla rader) ----
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
  slice(1) %>%
  ungroup() %>%
  count(sex)

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

## range of time? ----
cisipd %>%
  filter(cisiDate > 2012) %>%
  ggplot(aes(x=cisiDate)) +
  geom_histogram()

summary(cisipd$cisiDate)

# how many unique id's between 2022 and end of 2025?
cisipd %>%
  filter(between(cisiDate, as.Date("2022-01-01"), as.Date("2025-12-31"))) %>%
  group_by(id) %>%
  slice_sample(n = 1) %>%
  ungroup() %>%
  nrow()

# how many of the unique id's between 2022 and end of 2025 have multiple measurements
cisipd %>%
  filter(between(cisiDate, as.Date("2022-01-01"), as.Date("2025-12-31"))) %>%
  group_by(id) %>%
  count() %>%
  filter(n > 1) %>%
  nrow()

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

# join df's cisipd and drug, base the join on cisipd, random instance of each id
drugCisi <- cisipd %>%
  group_by(id) %>%
  slice_sample(n = 1) %>%
  ungroup() %>%
  right_join(drug, by = "id")

# this df contains unique cisi ratings, but multiple drugs for some id's
drugCisiUnique <-
  drugCisi %>%
  filter(between(cisiDate, startDate, endDate))

# library(gtsummary)
# tbl_summary(drugCisiUnique)

write_csv(drugCisiUnique, "data/drugCisiUnique.csv")

cisiAnalysisSample <- drugCisiUnique %>%
  select(id,cisiDate,motorSigns,disability,motorComplications,cognition) %>%
  na.omit() %>%
  group_by(id) %>%
  slice_sample(n = 1) %>%
  ungroup()

cisiAnalysisSample <- cisiAnalysisSample %>%
  left_join(pat, by = "id")

# we need to debug the diag object
# test <- diag %>%
#   left_join(cisiAnalysisSample, by = "id") %>%
#   group_by(id) %>%
#   filter(abs(difftime(diagDate, cisiDate, units = "days")) ==
#            min(abs(difftime(diagDate, cisiDate, units = "days"))))

# cisiAnalysisSample <- cisiAnalysisSample %>%
#   left_join(diag, by = "id")

write_csv(cisiAnalysisSample, glue::glue("data/cisiAnalysisSample_{Sys.time()}.csv"))

# To test double registrations for id
test <-
  diag %>%
  count(id) %>%
  filter(n > 1)












# Combine data
combined <- merge(data1, data2, by = "id")
















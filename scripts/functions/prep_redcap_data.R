# /////////////////////////////////////////////////////////////////////////////
# Carlos Rodriguez Ph.D. CU Anschutz Dept. of Family Medicine

# Description: This script will download and process AHRQ Long COVID RedCap
# Project ID 25710 Data. This script is designed to be a centralized data
# processing script that can be used in several reports such as the patient
# accrual report, patient survey report, and patient data collection tables
# (for RPPR).

# Status: Work in progress
# Last updated: 07/14/2026

# /////////////////////////////////////////////////////////////////////////////

# library(magrittr, include = "%<>%")
# library(dfmtbx)
# library(tidyverse)
# library(gtsummary)

# Pull report 176060 as labeled data
# Corresponds to patient_enrollment report in REDCap
data <- pull_redcap_report(
  Sys.getenv("LC_patient"), 
  "176060", 
  "label", 
  "raw", 
  "true")

# T1 Columns with other text
# These all have at least one response with text, and all show up
# data$other_service
# data$conditions_other
# data$other_med

# T5 Columns that won't download. Columns will not export to .xlsx either
# May possibly be due no one having data in there at this point
# data$conditions_other_t5
# data$other_med_t5
# data$other_service_t5

# Capture the column names
names_data <- names(data)

# Drop the demographic select all that apply questions from data so that
# modified columns can be merged in a subsequent step.
data %<>%
  select(
    -starts_with("patient_race"),
    -starts_with("patient_insurance"),
    -starts_with("patient_employ"),
    -redcap_event_name # use raw instead of label to preserve prior functions
)


# Pull report 176060 as raw data for the demographic variables only
demographics <- 
  pull_redcap_report(Sys.getenv("LC_patient"), "176060", "raw", "raw", "false") %>%
  select(
    redcap_event_name,
    starts_with("patient_race"),
    starts_with("patient_insurance"),
    starts_with("patient_employ")
  )

# Merge the primary data set with the demographic data set
data <- bind_cols(data, demographics)

# Re-order the columns to preserve the order in the the data frame data
data %<>%
  select(all_of(names_data))

# Rename promis_record_id to record_id (easier to type)
data %<>%
  rename(record_id = promis_record_id)

# Clear workspace variables
rm(demographics, names_data)

# Create a timepoint variable
data %<>%
  mutate(
    timepoint = case_match(
    redcap_event_name,
    "t5_survey_arm_1" ~ 5,
    "t4_survey_arm_1" ~ 4,
    "t3_survey_arm_1" ~ 3,
    "t2_survey_arm_1" ~ 2,
    "consent_and_t1_sur_arm_1" ~ 1,
    .default = NA)
  ) 


# Check columns
# View(
#   data %>%
#     select(redcap_event_name, record_id, name, anythingelse, study_label, not_enrolled_status) %>%
#     group_by(record_id) %>%
#     fill(everything(), .direction = "updown") %>%
#     ungroup()
# )


# Identify test records as those with the "test" in the name field OR those
# with "test record" in the anything else field
# ids 14, 15, 16, and 18 flagged as test Ids. (CR 12/09/2025)
# ids, 64, 65 flagged as test ids. (CR 2/23/2026)
test_ids <-
  data %>%
    filter(
      grepl("test", name, ignore.case = TRUE) | 
      grepl("test record", anythingelse, ignore.case = TRUE) |
      grepl("test", study_label, ignore.case = TRUE) |
      grepl("test", not_enrolled_status, ignore.case = TRUE)
      ) %>%
    distinct(record_id) %>%  
    pull(record_id)
    


# Identify duplicated record_ids as those with "Duplicated record" in the 
# enrollstatus field
# Ids 10, 35, 36, 38, flagged as duplicate Ids
# Id 69 flagged as duplicate (02/23/2026)
duplicated_ids <- 
  data %>%
    filter(
      grepl("duplicate", enrollstatus, ignore.case = TRUE) |
      grepl("duplicate", study_label, ignore.case = TRUE) |
      grepl("duplicate", not_enrolled_status, ignore.case = TRUE)
      ) %>%
    distinct(record_id) %>%  
    pull(record_id)

# Remove the test and duplicated ids
data %<>%
  filter(!record_id %in% c(test_ids, duplicated_ids))

# Create an updated enroll status column, this coalesces the not_enrolled_status
# and then enrollstatus column, which updates the status to the current status
data %<>%
  mutate(current_enroll_status = coalesce(not_enrolled_status, enrollstatus))

# Check that all current_enroll_status == Enrollment completed also have a time
# stamp for T1 PASC
all_t1_complete <- data %>%
  filter(
    record_id %in% (data %>% filter(current_enroll_status == "Enrollment completed") %>% pull(record_id)),
    timepoint == 1) %>% 
  pull(pasc_symptoms_and_followup_questions_complete) %>%
  table() %>%
  names()

if (all_t1_complete != "Complete"){
  stop("Check completed status of enrolled and T1 PASC.")
}


# Come up with a way to determine where the drop out occured, T1, T2, etc.
data %>%
  pull(current_enroll_status) %>% table() %>% names()

# Modify the responses in current_enroll_status for 
data %<>% 
  mutate(current_enroll_status = gsub("\\(.*", "", current_enroll_status)) %>%
  mutate(current_enroll_status = trimws(current_enroll_status)) 

# Create a dataframe showing the last completed survey for those that left the
# study.
ltfu_df <- data %>%
  filter(record_id %in% (data %>% filter(current_enroll_status == "left study") %>% pull(record_id))) %>%
  filter(timepoint %in% c(1, 2, 3, 4, 5)) %>%
  select(record_id, redcap_event_name, 
    pasc_symptoms_and_followup_questions_complete,
    pasc_symptoms_only_t2_t4_complete,
    pasc_symptoms_and_followup_questions_t5_complete) %>%
  mutate(pasc_status = coalesce(
    pasc_symptoms_and_followup_questions_complete,
    pasc_symptoms_only_t2_t4_complete,
    pasc_symptoms_and_followup_questions_t5_complete)) %>%
  filter(pasc_status == "Complete") %>%
  tail(n = 1) %>%
  select(record_id, redcap_event_name) %>%
  rename(ltfu_surv = redcap_event_name)


# merge any data from the lost to follow up df
data %<>%
  mutate(ltfu = ifelse(record_id %in% ltfu_df$record_id, 1, 0)) %>%
  left_join(ltfu_df, by = "record_id")

rm(ltfu_df)

# Switch those that were designated as left study to enroll status, since 
# their data can still be used for analysis.
data %<>%
  mutate(current_enroll_status = ifelse(grepl("left study", current_enroll_status), "Enrollment completed", current_enroll_status)) %>%
  mutate(enrollstatus = coalesce(current_enroll_status, enrollstatus))


# Clean up the modified enrollstatus column
data %<>%
  mutate(
    enrollstatus = ifelse(grepl("never", enrollstatus), "No response", enrollstatus),
    enrollstatus = ifelse(grepl("declined", enrollstatus), "Declined", enrollstatus),
    enrollstatus = ifelse(grepl("partial", enrollstatus), "Partial", enrollstatus),
    enrollstatus = ifelse(grepl("eligible", enrollstatus), "Not eligible", enrollstatus),
  )

# Capture those that completed the enrollment step and completed a T1 PASC
enrollment_completed_ids <- data %>%
  filter(enrollstatus == "Enrollment completed") %>%
  pull(record_id)

# ids flagged as loss to follow up
ltfu_ids <- data %>%
  filter(grepl("ltfu", study_label, ignore.case = TRUE)) %>%
  pull(record_id)

# This should not longer be needed as there is a dedicated lost to follow up  
# data %<>%
#   mutate(enrollstatus = ifelse(record_id %in% ltfu_ids, "LTFU", enrollstatus))

# Ids that declined
declined_ids <- data %>%
  filter(grepl("declined", study_label, ignore.case = TRUE)) %>%
  pull(record_id)

# This should not longer be needed as Declined is now a value in enrollstatus 
# data %<>%
#   mutate(enrollstatus = ifelse(record_id %in% declined_ids, "Declined", enrollstatus))


# /////////////////////////////// Data Clean Up ///////////////////////////////
# Create a site variable by coalescing mdc_name and pcc_name before filling
# values
data %<>%
  mutate(site_name = coalesce(mdc_name, pcc_name))

# Fill in empty values for pcc_or_mdc, mdc_name, and pcc_name columns
data %<>%
  group_by(record_id) %>%
  fill(pcc_or_mdc, .direction = c("updown")) %>%
  fill(site_name, .direction = c("updown")) %>%
  ungroup()

# Rename pcc_or_mdc to site_type for readability, and make unknown explicit
data %<>%
  rename(site_type = pcc_or_mdc) %>%
  mutate(site_type = ifelse(is.na(site_type), "Unknown", site_type)) %>%
  mutate(site_name = ifelse(is.na(site_name), "Unknown", site_name))

# Patient demographics --------------------------------------------------------

# Age group
data %<>%
  mutate(age_group = case_when(
                               patient_age >= 18 & patient_age < 35 ~ "18-34",
                               patient_age >= 35 & patient_age < 65 ~ "35-64",
                               patient_age >= 65 ~ "65+",
                               TRUE ~ NA_character_
                               ),
         age_group = factor(age_group, levels = c("18-34", "35-64", "65+"), ordered = TRUE))
 

## Gender group
data %<>%
  mutate(gender_group= ifelse(patient_gender %in% c("Non-binary", "Prefer not to answer", "Transgender Male"), "Unknown/Other", patient_gender))


## Race group
# Could be accomplished by pulling numerical data for more than 1 race and label data for coalescing
# Could also use label data and an across() verb with ifelse(is.na())
data %<>%
  # More than 1 race as the sum of all race options
  mutate(mt1_race = rowSums(across(c(patient_race___1:patient_race___7)))) %>%
  mutate(
    patient_race___1 = ifelse(patient_race___1 == 1, "African American or Black", NA),
    patient_race___2 = ifelse(patient_race___2 == 1, "American Indian/Alaska Native", NA),
    patient_race___3 = ifelse(patient_race___3 == 1, "Asian", NA),
    patient_race___4 = ifelse(patient_race___4 == 1, "Native Hawaiian/Other Pacific Islander", NA),
    patient_race___5 = ifelse(patient_race___5 == 1, "White", NA),
    patient_race___6 = ifelse(patient_race___6 == 1, "Other", NA),
    patient_race___7 = ifelse(patient_race___7 == 1, "Prefer not to answer", NA)) %>%
  mutate(race = coalesce(!!! select(., starts_with("patient_race")))) %>%
  mutate(race = ifelse(mt1_race > 1, "More than one race", race)) %>%
  mutate(race = ifelse(patient_ethn == "Hispanic or Latino", "Hispanic or Latino", race))


## Insurance
data %<>%
  mutate(across(patient_insurance___1:patient_insurance___10, ~ factor(.x, levels = c(0,1))))
 

## Education level

## Marital status

## Employment status pre-covid
data %<>%
  mutate(across(patient_employ___1:patient_employ___7, ~ factor(.x, levels = c(0,1))))

## Current employment status
data %<>% 
  mutate(across(patient_employ_2___1:patient_employ_2___7, ~ factor(.x, levels = c(0,1))))
  
# Patient clinical questions --------------------------------------------------
# Asked at T1 and T5 only
# months_symps

# pcq_t1 set to not select other_service, for name hormonization
pcq_t1 <- data %>%
  filter(timepoint == 1) %>%
  select(record_id, timepoint, months_symps:therapies___21) %>%
  select(-conditions_other, -other_med)

# n.b. does not download the other column of conditions, medications, or 
# service for some reason.
pcq_t5 <- data %>%
  filter(timepoint == 5) %>%
  select(record_id, timepoint, months_symps_t5:therapies_t5___21)

# Remove the columns that were just copied to re-introduce after data 
# harmonization
data %<>%
  select(
    -(all_of(names(pcq_t1)[3:41]))) %>%
  select(
    -(all_of(names(pcq_t5)[3:41])))

# Set the names of the pcq_t1 columns to rename the columns in pcq_t5
pcq_names  <- names(pcq_t1)
names(pcq_t5) <- pcq_names

# Reintroduce the pcq items
data %<>%
  left_join(
    bind_rows(pcq_t1, pcq_t5),
    by = c("record_id", "timepoint")
  )

rm(pcq_t1,  pcq_t5)

## Current Diagnosis
data %<>%
  mutate(lcdiagnosis = ifelse(lcdiagnosis == "Yes", 1, 0))

## Which conditions prior to developing covid
data %<>%
  mutate(across(conditions___1:conditions___14, ~ factor(ifelse(.x == "Checked", 1, 0), levels = c(0, 1))))

## Medications
data %<>% 
  mutate(across(medications___1:medications___13, ~ factor(ifelse(.x == "Checked", 1, 0), levels = c(0, 1))))

## Therapies
data %<>%
  mutate(across(therapies___13:therapies___21, ~ factor(ifelse(.x == "Checked", 1, 0), levels = c(0, 1))))

# PROMIS ----------------------------------------------------------------------
# Prep the promis variables. For t1, the names of the columns do not match with
# the columns in the scoring guide. The following code chunk is designed to 
# harmonize the names of the RedCap variables with those in the scoring guide. 
# In addition, labeled variables are converted to numerical for the calculation
# of the raw scores, and other variables are cleaned up


# PROMIS only differ in their headders
# data %>%
#   select(promis_global01:avg_pain)

# data %>%
#   select(promis_global01_v2:avg_pain_v2)

# There are separate instruments for T1, T2-T4, and T5, instead of using the
# same instrument but at different time points.
data %<>%
  mutate(
    promis_1 = coalesce(promis_global01, promis_global01_v2, promis_global01_t5),
    promis_2 = coalesce(promis_global02, promis_global02_v2, promis_global02_t5),
    promis_3 = coalesce(promis_global03, promis_global03_v2, promis_global03_t5),
    promis_4 = coalesce(promis_global04, promis_global04_v2, promis_global04_t5),
    promis_5 = coalesce(promis_global05, promis_global05_v2, promis_global05_t5),
    promis_6 = coalesce(promis_global07, promis_global07_v2, promis_global07_t5),
    promis_7 = coalesce(avg_pain, avg_pain_v2, avg_pain_t5),
    promis_8 = coalesce(avg_fatigue, avg_fatigue_v2, avg_fatigue_t5),
    promis_9 = coalesce(promis_global06, promis_global06_v2, promis_global06_t5),
    promis_10 = coalesce(bothered, bothered_v2, bothered_t5)
  ) %>%
  mutate(across(promis_2:promis_6, ~ as.numeric(substr(.x, 1, 1)))) %>%
  mutate(promis_7 = case_match(
    promis_7,
    "0 No pain" ~ 5,
    c("1", "2", "3") ~ 4,
    c("4", "5", "6") ~ 3,
    c("7", "8", "9") ~ 2,
    "10 Worst pain imagin-able" ~ 1,    
    .default = NA)) %>%
  mutate(promis_8 = case_match(
    promis_8,
      "None" ~ 5,
      "Mild" ~ 4,
      "Moderate" ~ 3, 
      "Severe" ~ 2,
      "Very severe" ~ 1,
      .default = NA)) %>%
  mutate(promis_10 = case_match(
    promis_10,
      "Never" ~ 5,
      "Rarely" ~ 4,
      "Sometimes" ~ 3, 
      "Often" ~ 2,
      "Always" ~ 1,
      .default = NA)) %>%
  mutate(
    across(c(promis_1, promis_9), ~ 
      factor(str_trim(sub("^[^-]*-", "", .x)), levels = c("Excellent", "Very good", "Good", "Fair", "Poor")))) %>%
  mutate(promis_global_phys = rowSums(across(c(promis_7, promis_6, promis_3, promis_8)))) %>%
  mutate(promis_global_ment = rowSums(across(c(promis_2, promis_4, promis_5, promis_10))))

# Remove the columns that are no longer needed
data %<>%
  select(-(starts_with("promis_global0"))) %>%
  select(-(starts_with("avg_pain"))) %>%
  select(-(starts_with("bothered"))) %>%
  select(-(starts_with("avg_fatigue")))

# Load the t-score tables
promis_global_pht <- read_csv("D:\\long_covid\\patient_survey\\scoring_guides\\promis_phys_t-score_table.csv",
  show_col_types = FALSE)

promis_global_mht <- read_csv("D:\\long_covid\\patient_survey\\scoring_guides\\promis_ment_t-score_table.csv",
  show_col_types = FALSE)

# Merge in global physical health scores
data %<>%
  left_join(
    promis_global_pht %>% select(-SE),
    by = c("promis_global_phys" = "Raw Summed Score")
  ) %>%
  rename(promis_pht = "T-Score")

# Merge in the mental health scores
data %<>%
  left_join(
    promis_global_mht %>% select(-SE),
    by = c("promis_global_ment" = "Raw Summed Score")
  ) %>%
  rename(promis_mht = "T-Score")


# PASC ------------------------------------------------------------------------
# t1 and t5 get ps_pasc with symptom severity
# The responses change from T1 to T5 for the symptom questions. The responses
# in T5 resemble those in T2:T4
# T2:T4 only get ps2_pasc with out symptom severity
# ps2_pasc subsets to those who answer yes or I don't know

# These are the 12 symptoms used to calculate a PASC score
# The responses between ps1 and ps2 are a bit different
# With ps2 pasc, we can assess PASC score in the past 3 months
# and in the past 30 days, whereas ps1 is currently.
data %<>%
  mutate(
    ps_sense = coalesce(ps_sense, ps2_sense, ps_sense_t5 ),
    ps_malaise = coalesce(ps_malaise, ps2_malaise, ps_malaise_t5),
    ps_cough = coalesce(ps_cough, ps2_cough, ps_cough_t5),
    ps_think = coalesce(ps_think, ps2_think, ps_think_t5 ),
    ps_thirst = coalesce(ps_thirst, ps2_thirst, ps_thirst_t5),
    ps_heart = coalesce(ps_heart, ps2_heart, ps_heart_t5),
    ps_pain = coalesce(ps_pain, ps2_pain, ps_pain_t5),
    ps_fatigue = coalesce(ps_fatigue , ps2_fatigue, ps_fatigue_t5),
    ps_sex = coalesce(ps_sex, ps2_sex, ps_sex_t5),
    ps_faint = coalesce(ps_faint, ps2_faint, ps_faint_t5),
    ps_gastro = coalesce(ps_gastro, ps2_gastro, ps_gastro_t5),
    ps_nerve = coalesce(ps_nerve, ps2_nerve, ps_nerve_t5),
  )


pasc_symptoms <- c(
"ps_sense",
"ps_malaise",
"ps_cough",
"ps_think",
"ps_thirst",
"ps_heart",
"ps_pain",
"ps_fatigue",
"ps_sex",
"ps_faint",
"ps_gastro",
"ps_nerve")

# These are the weights to multiply each binary symptom value
pasc_symptom_scores <- c(8, 7, 4, 3, 3, 2, 2, 1, 1, 1, 1, 1)

# Create a separate data frame subset to those that received the branching 
# questions.
pasc_data <- data %>%
  filter(
    ps_ptpasc == "Yes" |
    ps2_ptpasc %in% c("Yes", "I don't know of prefer not to answer") |
    ps_ptpasc_t5 == "Yes") %>%
  select(record_id, redcap_event_name, all_of(pasc_symptoms))

# Score the pasc at t1 and t5. For T5, does not discriminate for whether the 
# symptom was experienced in the last 30 days or last 3 months
ps1 <- bind_cols(  
  pasc_data %>%
  filter(
    redcap_event_name %in% c("consent_and_t1_sur_arm_1", "t5_survey_arm_1")) %>%
  select(record_id, redcap_event_name),
    
  pasc_data %>%
  filter(
    redcap_event_name %in% c("consent_and_t1_sur_arm_1", "t5_survey_arm_1")) %>%
  select(all_of(pasc_symptoms)) %>%
  mutate(across(everything(), ~ ifelse(str_detect(.x, "Yes"), 1, 0))) %>%
  sweep(., 2, pasc_symptom_scores, `*`) %>% 
  mutate(pasc_score = rowSums(across(everything()))) %>%
  mutate(pasc_positive = ifelse(pasc_score >= 12, 1, 0)) %>%
  select(pasc_score, pasc_positive)
)

# Score the pasc at t2 - t4 ----
# Symptoms in the past 3mo but not past 30 days
# ps2_3mo <- bind_cols(
#   pasc_data %>%
#   filter(
#     !redcap_event_name %in% c("consent_and_t1_sur_arm_1", "t5_survey_arm_1")) %>%
#   select(record_id, redcap_event_name),
    
#   pasc_data %>%
#   filter(
#     !redcap_event_name %in% c("consent_and_t1_sur_arm_1", "t5_survey_arm_1")) %>%
#   select(all_of(pasc_symptoms)) %>%
#   mutate(across(everything(), ~ ifelse(str_detect(.x, "Yes, but not in the last 30 days"), 1, 0))) %>%
#   sweep(., 2, pasc_symptom_scores, `*`) %>% 
#   mutate(pasc_score_3mo = rowSums(across(everything()))) %>%
#   mutate(pasc_positive_3mo = ifelse(pasc_score_3mo >= 12, 1, 0)) %>%
#   select(pasc_score_3mo, pasc_positive_3mo)
# )

# # Symptoms in the past 30 days
# ps2_30d <- bind_cols(
#   pasc_data %>%
#   filter(
#     !redcap_event_name %in% c("consent_and_t1_sur_arm_1", "t5_survey_arm_1")) %>%
#   select(record_id, redcap_event_name),
    
#   pasc_data %>%
#   filter(
#     !redcap_event_name %in% c("consent_and_t1_sur_arm_1", "t5_survey_arm_1")) %>%
#   select(all_of(pasc_symptoms)) %>%
#   mutate(across(everything(), ~ ifelse(str_detect(.x, "Yes, and I STILL HAVE it (in the last 30 days)"), 1, 0))) %>%
#   sweep(., 2, pasc_symptom_scores, `*`) %>% 
#   mutate(pasc_score_30d = rowSums(across(everything()))) %>%
#   mutate(pasc_positive_30d = ifelse(pasc_score_30d >= 12, 1, 0)) %>%
#   select(pasc_score_30d, pasc_positive_30d)
# )

# Symptoms within the past 3 months or 30 days
ps2 <- bind_cols(
  pasc_data %>%
  filter(
    !redcap_event_name %in% c("consent_and_t1_sur_arm_1", "t5_survey_arm_1")) %>%
  select(record_id, redcap_event_name),

  pasc_data %>%
  filter(
    !redcap_event_name %in% c("consent_and_t1_sur_arm_1", "t5_survey_arm_1")) %>%
  select(all_of(pasc_symptoms)) %>%
  mutate(across(everything(), ~ ifelse(str_detect(.x, "Yes"), 1, 0))) %>%
  sweep(., 2, pasc_symptom_scores, `*`) %>%
  mutate(pasc_score = rowSums(across(everything()))) %>%
  mutate(pasc_positive = ifelse(pasc_score >= 12, 1, 0)) %>%
  select(pasc_score, pasc_positive)
)

# Merge scores back into main data frame
# data %>%
  # left_join(ps1, by = c("redcap_event_name", "record_id")) %>%
  # left_join(ps2_3mo, by = c("redcap_event_name", "record_id")) %>% 
  # left_join(ps2_30d, by = c("redcap_event_name", "record_id")) 

data %<>%
  left_join(
    bind_rows(ps1, ps2), 
    by = c("redcap_event_name", "record_id")
)

rm(ps1, ps2)

# Remove the harmonized symptoms columns from the data set, but keep the 
# branching  *_ptpasc and symptom severity questions
data %<>%
  select(-(ps_fatigue_t5:ps_other_t5))

data %<>%
  select(-(ps2_fatigue:ps2_other))


# Harmonize the symptom severity items from t1 and t5-------------------------

# Create separate data frames of the section of columns containing the symtpom
# severity questions
pasc_t1_severity <- data %>%
  filter(timepoint == 1) %>%
  select(record_id, timepoint, ps_fatigue_burden:ps_sex_burden)

pasc_t5_severity <- data %>%
  filter(timepoint == 5) %>%
  select(record_id, timepoint, ps_fatigue_burden_t5:ps_sex_burden_t5)

# Harmonize the t5 names to the t1 names
names_pasc_t1_severity <- names(pasc_t1_severity)

names(pasc_t5_severity) <- names_pasc_t1_severity

# Drop columns before mergin
data %<>%
  select(-(ps_fatigue_burden:ps_sex_burden)) %>%
  select(-(ps_fatigue_burden_t5:ps_sex_burden_t5))

# stack after renaming
pasc_severity <- bind_rows(pasc_t1_severity, pasc_t5_severity)

# Merge the data 
data %<>%
  left_join(pasc_severity, by = c("record_id", "timepoint"))

rm(pasc_t1_severity, pasc_t5_severity)


# PASC Figure generating function ---------------------------------------------
make_pasc_bar_chart <- function(df) {

  # Set the core symptoms
  core_symptoms <- df %>%
  select(ps_fatigue:ps_sex) %>%
  names()

  # Prepare histogram df, where the frequency and percentage of each symptom
  # is calculated
  pasc_hist <- df %>%
    select(all_of(core_symptoms)) %>%
    mutate(across(everything(), ~ ifelse(str_detect(.x, "Yes"), 1, 0))) %>%
    pivot_longer(cols = everything(), names_to = "symptom", values_to = "yes") %>%
    group_by(symptom) %>%
    summarise(total_endorsed = sum(yes)) %>%
    mutate(symptom = factor(symptom, levels = core_symptoms))

  # Get a denominator all patients
  denominator_all <- df %>%
    nrow()


  # Get a denominator for ps_menstrual:ps_menopause which is a subset of those
  # that are not Male or Transgender Female, because these are branched questions
  # in the RedCap project.
  denominator_subset <- df %>%
    filter(!patient_gender %in% c("Male", "Transgender Female")) %>%
    select(patient_gender, ps_menstrual:ps_menopause) %>%
    nrow()

  pasc_hist %<>%
    mutate(percent = str_c("(",
      round(
        ifelse(symptom %in% c("ps_menstrual", "ps_menopause"), total_endorsed / denominator_subset, total_endorsed / denominator_all),
        2) * 100, "%)")) %>%
    mutate(n_per = str_c(total_endorsed, " ", percent))

# Get an ordered vector of symptoms to organize the bar chart by 
# frequency/percentage
  ordered_symptoms <- pasc_hist %>%
    arrange(total_endorsed) %>%
    pull(symptom)

# Calculate a percentage in parentheses, and then concatenate the frequency and
# proportion into one value that will be used to generate labels for the bar
# chart
  pasc_hist %>%
    mutate(symptom = factor(symptom, levels = ordered_symptoms)) %>%
    ggplot(aes(x = symptom, y = total_endorsed, fill = symptom)) +
    geom_col() +
    geom_text(
      aes(label = n_per), 
            hjust = -0.25,
            size = 3) +  
    theme_minimal() +
    lims(y = c(0, 75)) + # (0,35) works great for PCC and MDC, but not Overall)
    coord_flip() +
    theme(legend.position = "none") +
    labs(x = "Symptom", y = "Number endorsed (percent)")
}

# Experiences of stigma -------------------------------------------------------
# This instrument is the lack of understanding subscale from the illness
# invalidation inventory.

# Only administered at t1 and t5 and is asked in reference to experienecs at
# PCCs and MDCs if a patient reports a visit in those site types within the
# past 3 months.

# Harmonize the names
# Place all values in one column for each item instead of 2
data %<>%
  mutate(
    pcc_appts  = coalesce(pcc_appts, pcc_appts_t5 ),
    serious_pcc  = coalesce(serious_pcc, serious_pcc_t5),
    consequences_pcc  = coalesce(consequences_pcc, consequences_pcc_t5),
    mdc_appts  = coalesce(mdc_appts, mdc_appts_t5 ),
    serious_mdc  = coalesce(serious_mdc, serious_mdc_t5),
    consequences_mdc  = coalesce(consequences_mdc, consequences_mdc_t5),
    talk_mdc   = coalesce(talk_mdc, talk_mdc_t5),
  )

# Remove columns that are no longer needed
data %<>%
  select(-(pcc_appts_t5:talk_mdc_t5))

# Convert NAs to Unknowns
data %<>%
  mutate(across(c(pcc_appts, mdc_appts), ~ fct_na_value_to_level(factor(.x), level = "Unknown")))

# Calculate the LOU pcc scores
lou_pcc <- data %>%
  filter(pcc_appts == "Yes") %>%
  select(record_id, timepoint, serious_pcc:talk_pcc) %>%
  mutate(across(serious_pcc:talk_pcc, ~ case_match(.x, 
      "Never" ~ 1,
      "Rarely" ~ 2,
      "Sometimes" ~ 3, 
      "Often" ~ 4,
      "Very Often" ~ 5,
      .default = NA))) %>%
  mutate(na_counts = rowSums(is.na(across(serious_pcc:talk_pcc)))) %>%   
  mutate(lou_score = rowMeans(across(serious_pcc:talk_pcc), na.rm = TRUE)) %>%
  mutate(lou_score = ifelse(na_counts >= 2, NA, lou_score)) %>%
  select(record_id, timepoint, lou_score) %>%
  rename(lou_score_pcc = lou_score)

# Calculate the LOU mdc scores
lou_mdc <- data %>%
  filter(mdc_appts == "Yes") %>%
  select(record_id, timepoint, serious_mdc:talk_mdc) %>%
  mutate(across(serious_mdc:talk_mdc, ~ case_match(.x, 
      "Never" ~ 1,
      "Rarely" ~ 2,
      "Sometimes" ~ 3, 
      "Often" ~ 4,
      "Very Often" ~ 5,
      .default = NA))) %>%
  mutate(na_counts = rowSums(is.na(across(serious_mdc:talk_mdc)))) %>%   
  mutate(lou_score = rowMeans(across(serious_mdc:talk_mdc), na.rm = TRUE)) %>%
  mutate(lou_score = ifelse(na_counts >= 2, NA, lou_score)) %>%
  select(record_id, timepoint, lou_score) %>%
  rename(lou_score_mdc = lou_score)

# Merge score back to the main data set
data %<>%
  left_join(lou_pcc, by = c("record_id", "timepoint")) %>%
  left_join(lou_mdc, by = c("record_id", "timepoint"))
  
# Set the factor levels 
data %<>%
  mutate(across(serious_pcc:talk_pcc, ~ factor(.x, levels = c( 
      "Never",
      "Rarely",
      "Sometimes", 
      "Often",
      "Very Often"))))

data %<>%
  mutate(across(serious_mdc:talk_mdc, ~ factor(.x, levels = c( 
      "Never",
      "Rarely",
      "Sometimes", 
      "Often",
      "Very Often"))))

# Bind data and convert to long format for modeling
lou_data <- bind_rows(lou_mdc, lou_pcc)

# Remove un-necessary data frames
rm(lou_mdc, lou_pcc)

# could group_by() and fill updown, then slice_head()
lou_data_long <- lou_data %>%
  group_by(record_id, timepoint) %>%
  pivot_longer(cols = lou_score_mdc:lou_score_pcc, values_to = "score", names_to = "type") %>%
  ungroup() %>%
  drop_na(score) %>%
  arrange(record_id)

# Calculate means and SEs for the three individual LOU scale items
lou_pcc_means <- data %>%
  filter(pcc_appts == "Yes") %>%
  select(timepoint, serious_pcc:talk_pcc) %>%
  mutate(across(serious_pcc:talk_pcc, ~ case_match(.x, 
      "Never" ~ 1,
      "Rarely" ~ 2,
      "Sometimes" ~ 3, 
      "Often" ~ 4,
      "Very Often" ~ 5,
      .default = NA))) %>%
  group_by(timepoint) %>%
  summarise(
    mean_serious = mean(serious_pcc),
    se_serious = sd(serious_pcc) / sqrt(n()),
    mean_consequences = mean(consequences_pcc),
    se_consequences = sd(consequences_pcc) / sqrt(n()),
    mean_talk = mean(talk_pcc),
    se_talk = sd(talk_pcc) / sqrt(n()),
  )

lou_mdc_means <- data %>%
  filter(mdc_appts == "Yes") %>%
  select(timepoint, serious_mdc:talk_mdc) %>%
  mutate(across(serious_mdc:talk_mdc, ~ case_match(.x, 
      "Never" ~ 1,
      "Rarely" ~ 2,
      "Sometimes" ~ 3, 
      "Often" ~ 4,
      "Very Often" ~ 5,
      .default = NA))) %>%
  group_by(timepoint) %>%
  summarise(
    mean_serious = mean(serious_mdc),
    se_serious = sd(serious_mdc) / sqrt(n()),
    mean_consequences = mean(consequences_mdc),
    se_consequences = sd(consequences_mdc) / sqrt(n()),
    mean_talk = mean(talk_mdc),
    se_talk = sd(talk_mdc) / sqrt(n()),
  )


# Model the difference at time between PCC and MDC LOU scores
model <- lmerTest::lmer(score ~ type + (1 | record_id), data = (lou_data_long %>% filter(timepoint == 1)))

emms <- emmeans::emmeans(model, ~ type, lmer.df = "satterthwaite") %>% 
  pairs() %>% 
  as_tibble()

# Calculate the diffs and the SEs at time point 1
lou_item_level_long <- data %>%
  filter(timepoint == 1) %>%
  filter(mdc_appts == "Yes" | pcc_appts == "Yes") %>%
  select(record_id, serious_mdc, consequences_mdc, talk_mdc, serious_pcc, consequences_pcc, talk_pcc) %>%
  mutate(across(serious_mdc:talk_pcc, ~ case_match(.x, 
    "Never" ~ 1,
    "Rarely" ~ 2,
    "Sometimes" ~ 3, 
    "Often" ~ 4,
    "Very Often" ~ 5,
    .default = NA))) %>%
  pivot_longer(cols = serious_mdc:talk_pcc, values_to = "value", names_to = "item") %>%
  mutate(record_id = factor(record_id))


display_se <- function(df) {
  model <- lmerTest::lmer(value ~ item + (1 | record_id), data = df)

  emmeans::emmeans(model, ~ item, lmer.df = "satterthwaite") %>% 
    pairs() %>% 
    as_tibble()

}

# Get SEs for the differences between types -----------------------------------
# This section is commented out to prevent output from showing up in the
# patient accrual report, but it didn't do anything. And it still runs fine in
# the master t1 template .qmd docs.
# serious (produces is singular warning)
# Likely due to a small number of participants with 2 items
lou_item_level_long %>%
  filter(item %in% c("serious_mdc", "serious_pcc")) %>%
  display_se()

# Test the serious item of the LOU scale
# test_data <- lou_item_level_long %>% filter(item %in%  c("serious_mdc", "serious_pcc"))
# test_model <- lmerTest::lmer(value ~ item + (1 | record_id), data = test_data)

# consequences
lou_item_level_long %>%
  filter(item %in% c("consequences_mdc", "consequences_pcc")) %>%
  display_se()

# talk
lou_item_level_long %>%
  filter(item %in% c("talk_mdc", "talk_pcc")) %>%
  display_se()
# -----------------------------------------------------------------------------


# Health Related Social Needs (HRSN) from American Community Health Survey ----
# Only administered at t1 and t5

# Contains 16 columns
hrsn_t1 <- data %>%
  filter(timepoint == 1) %>%
  select(record_id, timepoint, living_sit:lonely)

# Contains 21 columns, 
hrsn_t5 <- data %>%
  filter(timepoint == 5) %>%
  select(record_id, timepoint, living_sit_t5:lonely_change_t5)

change_t5_columns <- hrsn_t5 %>%
  select(record_id, timepoint, ends_with("change_t5"))

hrsn_t5 %<>%
  select(-ends_with("change_t5"))


# Drop the columns

# Drop the HRSN columns, since they will be merged back in
# 16 variables
data %<>%
  select(-(living_sit:lonely)) %>%
  select(-(living_sit_t5:lonely_change_t5))


# Names of the t1 columns
hrsn_t1_names <- names(hrsn_t1)

# Set the new names of the t5 columns
names(hrsn_t5) <- hrsn_t1_names


# Merge harmonized columns back into data
data %<>%
  left_join(
    bind_rows(hrsn_t1, hrsn_t5), 
    by = c("record_id", "timepoint")) %>%
  left_join(change_t5_columns, by = c("record_id", "timepoint"))

# Remove un-necessary data
rm(hrsn_t1, hrsn_t5, change_t5_columns)

# Prep living_probs___*
data %<>%
  mutate(
    across(
      living_probs___1:living_probs___8, 
      ~ factor(ifelse(.x == "Checked", 1, 0), levels = c(0, 1))))

# Prep food_worry:food_last
data %<>%
  mutate(
    across(
      food_worry:food_last, 
      ~ factor(.x, levels = c("Often true", "Sometimes true", "Never true"))))

# Prep transportation
data %<>%
  mutate(across(transport, ~ factor(.x, levels = c("Yes", "No"))))

# Prep help_today
data %<>%
  mutate(
    help_daytoday = factor(
      help_daytoday, 
      levels = c("I don't need any help", 
                 "I get all the help I need", 
                 "I could use a little more help", 
                 "I need a lot more help")))

# Prep lonely
data %<>%
  mutate(
    lonely = factor(
      lonely, 
      levels = c("Never", 
                 "Rarely", 
                 "Sometimes", 
                 "Often", 
                 "Always")))

# Summary of needs by domain
# Pull redcap data in numeric format
hrsn_data_t1 <- 
  pull_redcap_report(Sys.getenv("LC_patient"), "176060", "raw", "raw", "false") %>%
  filter(redcap_event_name == "consent_and_t1_sur_arm_1") %>%
  select(
    promis_record_id, 
    redcap_event_name,
    living_sit:lonely) %>%
  rename(record_id = promis_record_id)

hrsn_data_t5 <- 
  pull_redcap_report(Sys.getenv("LC_patient"), "176060", "raw", "raw", "false") %>%
  filter(redcap_event_name == "t5_survey_arm_1") %>%
  select(
    promis_record_id, 
    redcap_event_name,
    living_sit_t5:lonely_t5) %>%
  rename(record_id = promis_record_id) %>%
  select(-ends_with("change_t5"))

names(hrsn_data_t5) <- names(hrsn_data_t1)

hrsn_data <- bind_rows(hrsn_data_t1, hrsn_data_t5)

rm(hrsn_data_t1, hrsn_data_t5)

hrsn_data %<>%
  mutate(
    living_sit = ifelse(living_sit %in% c(2,3), 1, 0),
    living_prob = rowSums(across(living_probs___1:living_probs___7)),
    living_need = ifelse((living_sit + living_prob) > 0, 1, 0),
    food_worry = ifelse(food_worry %in% c(1, 2), 1, 0),
    food_last = ifelse(food_last %in% c(1, 2), 1, 0),
    food_need = ifelse((food_worry + food_last) > 0, 1, 0),
    transport_need = ifelse(transport == 1, 1, 0),
    utlities_need = ifelse(utlities %in% c(1, 2), 1, 0),
    fin_strain_need = ifelse(fin_strain %in% c(1, 2), 1, 0),
    help_daytoday = ifelse(help_daytoday %in% c(3, 4), 1, 0),
    lonely = ifelse(lonely > 3, 1, 0),
    social_need = ifelse((help_daytoday + lonely) > 0, 1, 0)) %>%
  mutate(hrsn_need_score = rowSums(across(c(living_need, food_need, transport, utlities, fin_strain, social_need)), na.rm = TRUE)) %>%
  filter(redcap_event_name %in% c("consent_and_t1_sur_arm_1", "t5_survey_arm_1")) %>%
  select(
    record_id,
    redcap_event_name,
    living_need,
    food_need,
    transport_need,
    utlities_need,
    fin_strain_need,
    social_need,
    hrsn_need_score) %>%
  mutate(across(living_need:social_need, ~ factor(.x, levels = c(1, 0))))

# Merge back into main data frame
data %<>%
  left_join(hrsn_data, by = c("record_id", "redcap_event_name"))

# Turn NA values to factor levels
data %<>%
  mutate(
    across(
      c(living_sit, food_worry:lonely), 
      ~ fct_na_value_to_level(.x, level = "Unknown")))

rm(hrsn_data)

# Experiences with care -------------------------------------------------------
data %>%
  select(ends_with("_pcc")) %>%
  names()

# Order the factors for PCC
data %>%
  select(appt_pcc, explain_pcc:courtesy_pcc) %>%
  mutate(
    across(explain_pcc:medinfo_pcc,
    ~ factor(.x, levels = c(
      "Yes, definitely",
      "Yes, somewhat",
      "No"))))


# Order the factors for MDC
data %>%
  select(ends_with("_mdc")) %>%
  names()

data %>%
  select(appt_mdc, explain_pcc:courtesy_pcc) %>%
  mutate(
    across(explain_pcc:medinfo_pcc,
    ~ factor(.x, levels = c(
      "Yes, definitely",
      "Yes, somewhat",
      "No"))))


# Disability ------------------------------------------------------------------
# Only administered at t2
data %<>%
  rename(disability = dsiability) %>%
  mutate(disability = fct_na_value_to_level(factor(disability), level = "Unknown"))

# data %>%
#   filter(timepoint == 2) %>% 
#   select(disability) %>%
#   tbl_summary()
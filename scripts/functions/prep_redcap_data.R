# /////////////////////////////////////////////////////////////////////////////
# Carlos Rodriguez Ph.D. CU Anschutz Dept. of Family Medicine
# Description: This script will download and process AHRQ Long COVID RedCap
# Project ID 25710 Data. This script is designed to be a centralized data
# processing script that can be used in several reports such as patient 
# accrual, patient survey, patient data collection tables, and others.

# Status: Work in progress
# Last updated: 05/30/2026
# /////////////////////////////////////////////////////////////////////////////

# Pull report 176060 as labeled data
# Corresponds to patient_enrollment report in REDCap
data <- pull_redcap_report(Sys.getenv("LC_patient"), "176060", "label", "raw", "true")

# Capture the column names
names_data <- names(data)

# Drop the demographic select all that apply questions
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

# Identify test records as those with the "test" in the name field OR those
# with "test record" in the anything else field
# ids 14, 15, 16, and 18 flagged as test Ids. (CR 12/09/2025)
# ids, 64, 65 flagged as test ids. (CR 2/23/2026)
test_ids <-
  data %>%
    filter(
      grepl("test", name, ignore.case = TRUE) | 
      grepl("test record", anythingelse, ignore.case = TRUE) |
      grepl("test", study_label, ignore.case = TRUE)
      ) %>%
    pull(record_id)


# Identify duplicated record_ids as those with "Duplicated record" in the 
# enrollstatus field
# Ids 10, 35, 36, 38, flagged as duplicate Ids
# Id 69 flagged as duplicate (02/23/2026)
duplicated_ids <- 
  data %>%
    filter(
      grepl("duplicate", enrollstatus, ignore.case = TRUE) |
      grepl("duplicate", study_label, ignore.case = TRUE)
      ) %>%
    pull(record_id)

# Remove the test and duplicated ids
data %<>%
  filter(!record_id %in% c(test_ids, duplicated_ids))


# Capture those that completed the enrollment step
enrollment_completed_ids <- data %>%
  filter(enrollstatus == "Enrollment completed") %>%
  pull(record_id)

# ids flagged as loss to follow up
ltfu_ids <- data %>%
  filter(grepl("ltfu", study_label, ignore.case = TRUE)) %>%
  pull(record_id)

data %<>%
  mutate(enrollstatus = ifelse(record_id %in% ltfu_ids, "LTFU", enrollstatus))

# Ids that declined
declined_ids <- data %>%
  filter(grepl("declined", study_label, ignore.case = TRUE)) %>%
  pull(record_id)

data %<>%
  mutate(enrollstatus = ifelse(record_id %in% declined_ids, "Declined", enrollstatus))


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

# Patient demographics

## Age group
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

# Since the instruments only vary by the header, the columns can be colaesced
# and renamed
data %<>%
  mutate(
    promis_1 = coalesce(promis_global01, promis_global01_v2),
    promis_2 = coalesce(promis_global02, promis_global01_v2),
    promis_3 = coalesce(promis_global03, promis_global03_v2),
    promis_4 = coalesce(promis_global04, promis_global04_v2),
    promis_5 = coalesce(promis_global05, promis_global05_v2),
    promis_6 = coalesce(promis_global07, promis_global07_v2),
    promis_7 = coalesce(avg_pain, avg_pain_v2),
    promis_8 = coalesce(avg_fatigue, avg_fatigue_v2),
    promis_9 = coalesce(promis_global06, promis_global06_v2),
    promis_10 = coalesce(bothered, bothered_v2)
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
# t2 - t4 only get ps2_pasc with out symptom severity
# ps2_pasc subsets to those who answer yes or I don't know

# These are the 12 symptoms used to calculate a PASC score
# The responses between ps1 and ps2 are a bit different
# With ps2 pasc, we can assess PASC score in the past 3 months
# and in the past 30 days, whereas ps1 is currently.
data %<>%
  mutate(
    ps_sense = coalesce(ps_sense, ps2_sense),
    ps_malaise = coalesce(ps_malaise, ps2_malaise),
    ps_cough = coalesce(ps_cough, ps2_cough),
    ps_think = coalesce(ps_think, ps2_think ),
    ps_thirst = coalesce(ps_thirst, ps2_thirst),
    ps_heart = coalesce(ps_heart, ps2_heart),
    ps_pain = coalesce(ps_pain, ps2_pain),
    ps_fatigue = coalesce(ps_fatigue , ps2_fatigue),
    ps_sex = coalesce(ps_sex , ps2_sex),
    ps_faint = coalesce(ps_faint , ps2_faint),
    ps_gastro = coalesce(ps_gastro, ps2_gastro),
    ps_nerve = coalesce(ps_nerve , ps2_nerve),
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

# *** Could drop ps2 symptoms here

# These are the weights to multiply each binary symptom value
pasc_symptom_scores <- c(8, 7, 4, 3, 3, 2, 2, 1, 1, 1, 1, 1)

# Create a separate data frame subset to those that received the branching 
# question.
pasc_data <- data %>%
  filter(
    ps_ptpasc == "Yes" |
    ps2_ptpasc %in% c("Yes", "I don't know of prefer not to answer")) %>%
  select(record_id, redcap_event_name, all_of(pasc_symptoms))

# Score the pasc at t1 and t5
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

# Score the pasc at t2 - t4
# Symptoms in the past 3mo but not past 30 days
ps2_3mo <- bind_cols(
  pasc_data %>%
  filter(
    !redcap_event_name %in% c("consent_and_t1_sur_arm_1", "t5_survey_arm_1")) %>%
  select(record_id, redcap_event_name),
    
  pasc_data %>%
  filter(
    !redcap_event_name %in% c("consent_and_t1_sur_arm_1", "t5_survey_arm_1")) %>%
  select(all_of(pasc_symptoms)) %>%
  mutate(across(everything(), ~ ifelse(str_detect(.x, "Yes, but not in the last 30 days"), 1, 0))) %>%
  sweep(., 2, pasc_symptom_scores, `*`) %>% 
  mutate(pasc_score_3mo = rowSums(across(everything()))) %>%
  mutate(pasc_positive_3mo = ifelse(pasc_score_3mo >= 12, 1, 0)) %>%
  select(pasc_score_3mo, pasc_positive_3mo)
)

# Symptoms in the past 30 days
ps2_30d <- bind_cols(
  pasc_data %>%
  filter(
    !redcap_event_name %in% c("consent_and_t1_sur_arm_1", "t5_survey_arm_1")) %>%
  select(record_id, redcap_event_name),
    
  pasc_data %>%
  filter(
    !redcap_event_name %in% c("consent_and_t1_sur_arm_1", "t5_survey_arm_1")) %>%
  select(all_of(pasc_symptoms)) %>%
  mutate(across(everything(), ~ ifelse(str_detect(.x, "Yes, and I STILL HAVE it (in the last 30 days)"), 1, 0))) %>%
  sweep(., 2, pasc_symptom_scores, `*`) %>% 
  mutate(pasc_score_30d = rowSums(across(everything()))) %>%
  mutate(pasc_positive_30d = ifelse(pasc_score_30d >= 12, 1, 0)) %>%
  select(pasc_score_30d, pasc_positive_30d)
)


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
# commented out because no scoring 3mo and 30d separately was not the intended approach

# data %>%
  # left_join(ps1, by = c("redcap_event_name", "record_id")) %>%
  # left_join(ps2_3mo, by = c("redcap_event_name", "record_id")) %>% 
  # left_join(ps2_30d, by = c("redcap_event_name", "record_id")) 

data %<>%
  left_join(
    bind_rows(ps1, ps2), 
    by = c("redcap_event_name", "record_id")
)


# Experiences of stigma -------------------------------------------------------
# This instrument is the lack of understanding subscale from the illness
# invalidation inventory.

# Only administered at t1 and t5 and is asked in reference to experienecs at
# PCCs and MDCs if a patient reports a visit in those site types within the
# past 3 months.
data %<>%
  mutate(across(c(pcc_appts, mdc_appts), ~ fct_na_value_to_level(factor(.x), level = "Unknown")))

# Create a data frame for the PCC referenced questions
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


data %<>%
  left_join(lou_pcc, by = c("record_id", "timepoint")) %>%
  left_join(lou_mdc, by = c("record_id", "timepoint"))
  

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

# could groub_by() and fill updown, then slice_head()
lou_data_long <- lou_data %>%
  group_by(record_id, timepoint) %>%
  pivot_longer(cols = lou_score_mdc:lou_score_pcc, values_to = "score", names_to = "type") %>%
  ungroup() %>%
  drop_na(score) %>%
  arrange(record_id)


lou_pcc_data <- data %>%
  filter(pcc_appts == "Yes") %>%
  select(serious_pcc:talk_pcc) %>%
  mutate(across(serious_pcc:talk_pcc, ~ case_match(.x, 
      "Never" ~ 1,
      "Rarely" ~ 2,
      "Sometimes" ~ 3, 
      "Often" ~ 4,
      "Very Often" ~ 5,
      .default = NA))) %>%
  summarise(
    mean_serious = mean(serious_pcc),
    se_serious = sd(serious_pcc) / sqrt(n()),
    mean_consequences = mean(consequences_pcc),
    se_consequences = sd(consequences_pcc) / sqrt(n()),
    mean_talk = mean(talk_pcc),
    se_talk = sd(talk_pcc) / sqrt(n()),
  )

lou_mdc_data <- data %>%
  filter(mdc_appts == "Yes") %>%
  select(serious_mdc:talk_mdc) %>%
  mutate(across(serious_mdc:talk_mdc, ~ case_match(.x, 
      "Never" ~ 1,
      "Rarely" ~ 2,
      "Sometimes" ~ 3, 
      "Often" ~ 4,
      "Very Often" ~ 5,
      .default = NA))) %>%
  summarise(
    mean_serious = mean(serious_mdc),
    se_serious = sd(serious_mdc) / sqrt(n()),
    mean_consequences = mean(consequences_mdc),
    se_consequences = sd(consequences_mdc) / sqrt(n()),
    mean_talk = mean(talk_mdc),
    se_talk = sd(talk_mdc) / sqrt(n()),
  )


# Get the differences between the two LOU referenced measures
bind_rows(lou_mdc_data, lou_pcc_data) %>%
  select(starts_with("mean")) %>%
  summarise(
    diff_serious = diff(mean_serious),
    diff_consequences = diff(mean_consequences),
    diff_talk = diff(mean_talk),
    
  )


# Health Related Social Needs (HRSN) from American Community Health Survey
# Only administer at t1 and t5

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
hrsn_data <- 
  pull_redcap_report(Sys.getenv("LC_patient"), "176060", "raw", "raw", "false") %>%
  select(
    promis_record_id, 
    redcap_event_name,
    living_sit:lonely) %>%
  rename(record_id = promis_record_id)

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


# LEFT OFF HERE
# Any other instrument that was not adminisitered at t1

# Disability ------------------------------------------------------------------
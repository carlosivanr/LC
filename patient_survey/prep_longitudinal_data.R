# -----------------------------------------------------------------------------
# Carlos Rodriguez, PhD. CU Dept. of Family Medicine
# 01-12-2026

# Prep Longitudinal Data

# Description - This script is designed to download, prep and clean the RedCap
# data from the AHRQ Long COVID project.

# This script is part 1 of 3 in a series of code files that produce reports
# for the patient survey data.

# -----------------------------------------------------------------------------

library(magrittr, include = "%<>%")
library(tidyverse)
library(gtsummary)
library(dfmtbx)
# renv::install("carlosivanr/dfmtbx") # To update


# ///////////////////////////// Pull RedCap Data //////////////////////////////

# Pull report 176060 as labeled data
# Corresponds to patient_enrollment report in REDCap
data <- pull_redcap_report(Sys.getenv("LC_patient"), "176060", "label", "raw", "true")

# Capture the column names
names_data <- names(data)

# Drop the demographic select all that apply questions since these are pulled 
# separately as raw instead of label data
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

# Identify test records as those with the "test" in the name field OR those
# with "test record" in the anything else field
test_ids <- 
  data %>%
    filter(
      grepl("test", name, ignore.case = TRUE) | 
      grepl("test record", anythingelse, ignore.case = TRUE)) %>%
    pull(record_id)

# Identify duplicated record_ids as those with "Duplicated record" in the 
# enrollstatus field
duplicated_ids <- 
  data %>%
    filter(enrollstatus == "Duplicate record") %>%
    pull(record_id)

# Remove the test and duplicated ids from the pulled data
data %<>%
  filter(!record_id %in% c(test_ids, duplicated_ids))


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


#  Create a dataframe consisting of those patients that have consented 
# (although some consent but may not have completed all instruments).
tab_data <- data %>%
  select(record_id, enrollstatus, patient_consent_form_complete, healthrelated_social_needs_complete) %>%
  group_by(record_id) %>%
  fill(enrollstatus, patient_consent_form_complete, healthrelated_social_needs_complete, .direction = "updown") %>%
  slice_head() %>%
  ungroup() %>%
  # Convert missing values in enrollstatus to Unknown/ could be not yet screened or contacted but no response
  mutate(
    across(enrollstatus:healthrelated_social_needs_complete, ~ ifelse(is.na(.x), "Unknown", .x))) %>%
  filter(patient_consent_form_complete == 1 | patient_consent_form_complete == "Complete")

# Remove the patient_preenrollm_arm_1 and screening_and_enro_arm_1 events.
# This should also remove any identifying information from the data pull.
data %<>%
  filter(!redcap_event_name %in% c("patient_preenrollm_arm_1", "screening_and_enro_arm_1"))

# Subset the data to only those that have provided consent
data %<>%
  filter(record_id %in% tab_data$record_id)

# Patient age gender and sexual orientation
data %<>%
  mutate(age_group = case_when(
                               patient_age >= 18 & patient_age < 35 ~ "18-34",
                               patient_age >= 35 & patient_age < 65 ~ "35-64",
                               patient_age >= 65 ~ "65+",
                               TRUE ~ NA_character_
                               ),
         age_group = factor(age_group, levels = c("18-34", "35-64", "65+"), ordered = TRUE)) %>%
  group_by(record_id) %>%
  fill(age_group) %>%
  ungroup()

# Redcap event name to something more friendly for displaying/coding
data %<>%
  mutate(
    redcap_event_name = substr(redcap_event_name, start = 1, stop = 2),
    redcap_event_name = ifelse(redcap_event_name == "co", "t1", redcap_event_name))

# Conditions
data %<>% 
  mutate(
    across(conditions___1:conditions___14, 
    ~ factor(ifelse(.x == "Checked", 1, 0), levels = c(0, 1))))

# Medications, only asked at t1
data %<>%
  mutate(
    across(medications___1:medications___13, 
    ~ factor(ifelse(redcap_event_name == "t1" & .x == "Checked", 1, 0), levels = c(0, 1))))

# Therapies
data %<>%
  # select(redcap_event_name, therapies___13:therapies___21) %>%
  mutate(
    across(therapies___13:therapies___21, 
    ~ factor(ifelse(redcap_event_name == "t1" & .x == "Checked", 1, 0), levels = c(0, 1))))


# PROMIS ----------------------------------------------------------------------
# Prep the promis variables. The names of the columns do not match with the
# column names in the scoring guide. The following code chunk is designed to 
# harmonize the names of the RedCap variables with those in the scoring guide. 
# In addition, labeled variables are converted to numerical for the calculation
# of the raw scores, and other variables are cleaned up

data %<>%
  # select(promis_global01:avg_pain) %>%
  rename(
    promis_global09r = promis_global06,
    promis_global06 = promis_global07,
    promis_global10r = bothered,
    promis_global08r = avg_fatigue,
    promis_global07r = avg_pain) %>%
  mutate(across(promis_global02:promis_global05, ~ as.numeric(substr(.x, 1, 1)))) %>%
  mutate(promis_global06 = as.numeric(substr(promis_global06, 1, 1))) %>%
  mutate(promis_global07r = case_match(
    promis_global07r,
    "0 No pain" ~ 5,
    c("1", "2", "3") ~ 4,
    c("4", "5", "6") ~ 3,
    c("7", "8", "9") ~ 2,
    "10 Worst pain imagin-able" ~ 1,    
    .default = NA)) %>%
  mutate(promis_global08r = case_match(
    promis_global08r,
      "None" ~ 5,
      "Mild" ~ 4,
      "Moderate" ~ 3, 
      "Severe" ~ 2,
      "Very severe" ~ 1,
      .default = NA)) %>%
  mutate(promis_global10r = case_match(
    promis_global10r,
      "Never" ~ 5,
      "Rarely" ~ 4,
      "Sometimes" ~ 3, 
      "Often" ~ 2,
      "Always" ~ 1,
      .default = NA)) %>%
  mutate(across(c(promis_global01, promis_global09r), ~ 
  factor(str_trim(sub("^[^-]*-", "", .x)), levels = c("Excellent", "Very good", "Good", "Fair", "Poor")))) %>%
  mutate(promis_global_phys = rowSums(across(c(promis_global07r, promis_global06, promis_global03, promis_global08r)))) %>%
  mutate(promis_global_ment = rowSums(across(c(promis_global02, promis_global04, promis_global05, promis_global10r))))


# Calculate the PROMIS T-scores ------------------------------------------------
# load the t-score tables
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
# These are the 12 symptoms used to calculate a PASC score
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


# -----------------------------------------------------------------------------
# Create a separate data frame of the t1 pasc variables since they are named
# differently than the subsequent time points
t1_pasc <- 
  data %>% 
  filter(redcap_event_name == "t1") %>%
  select(record_id, redcap_event_name, starts_with("ps"), -starts_with("ps2"))


# Create a separate data frame of the non-t1 pasc variables and then rename the
# columns for harmonization. This will contain time points beyond t1.
t2_pasc <- 
  data %>% 
  filter(redcap_event_name != "t1") %>%
  select(record_id, redcap_event_name, starts_with("ps2"))

t2_pasc_names <- names(t2_pasc)

new_t2_pasc_names <- str_replace(t2_pasc_names, "ps2_", "ps_")

names(t2_pasc) <- new_t2_pasc_names


# Stack the two subsets together in one data frame for scoring
pasc_scores <- bind_rows(t1_pasc, t2_pasc)

# Create a data frame of the scores, subset to only those that responded "Yes"
# to ps_ptpasc
pasc_scores <- pasc_scores %>%
  filter(ps_ptpasc == "Yes") %>%
  bind_cols(
    (pasc_scores %>%
      filter(ps_ptpasc == "Yes") %>%
      select(all_of(pasc_symptoms)) %>%
      mutate(across(everything(), ~ ifelse(str_detect(.x, "Yes"), 1, 0))) %>%
      sweep(., 2, pasc_symptom_scores, `*`) %>%
      mutate(pasc_score = rowSums(across(everything()))) %>%
      mutate(pasc_positive = ifelse(pasc_score >= 12, 1, 0)) %>%
      select(pasc_score, pasc_positive))) %>%
  select(record_id, redcap_event_name, pasc_score, pasc_positive)
  

# Merge the scores back to the main data frame
data <- data %>%
  left_join(pasc_scores, by = c("record_id", "redcap_event_name"))


# PASC burden, frequency and severity -----------------------------------------
# Import data as numerical since there are inconsistencies in the 
# values/response options for each symptom, so designing an algorithm like 
# mutate(across(cols)) to convert the label values to numeric would not be 
# wise. Burden, frequency and severerity are only asked at T1.
pasc_severity_data <- 
  pull_redcap_report(Sys.getenv("LC_patient"), "176060", "raw", "raw", "false") %>%
  select(
    promis_record_id, 
    redcap_event_name,
    starts_with("ps_")) %>%
  filter(
    redcap_event_name == "consent_and_t1_sur_arm_1", 
    promis_record_id %in% (data %>% filter(redcap_event_name == "t1") %>% pull(record_id)))


## Burden ---------------------------------------------------------------------
# Create a df of the burden dimension columns
burden_df <- pasc_severity_data %>%
  select(ends_with("burden"))

# Get the names of the burden columns, to rename them so that there is common naming scheming
# to display burden, frequency and severity.
burden_names <- names(burden_df)

# Remove everything after the 1st underscore from the right 
burden_names_modified <- sub("_[^_]*$", "", burden_names)

# Set the new names
names(burden_df) <- burden_names_modified

# Convert to long format
burden_df_long <- burden_df %>%
  pivot_longer(cols = everything(), names_to = "symptom", values_to = "response") %>%
  mutate(dimension = "burden")

# Severity --------------------------------------------------------------------
severity_df <- pasc_severity_data %>%
  select(ends_with("sevdepaul"))

# Get the names
severity_names <- names(severity_df)

# Remove everything after the 1st underscore from the right
severity_names_modified <- sub("_[^_]*$", "", severity_names)

# Set the new names
names(severity_df) <- severity_names_modified

# Convert to long format
severity_df_long <- severity_df %>%
  pivot_longer(cols = everything(), names_to = "symptom", values_to = "response") %>%
  mutate(dimension = "severity")


# Frequency -------------------------------------------------------------------
frequency_df <- pasc_severity_data %>%
  select(ends_with("freqdepaul"))

frequency_names <- names(frequency_df)

frequency_names_modified <-  sub("_[^_]*$", "", frequency_names)

names(frequency_df) <- frequency_names_modified

frequency_df_long <- frequency_df %>%
  pivot_longer(cols = everything(), names_to = "symptom", values_to = "response") %>%
  mutate(dimension = "frequency")

# The following symptoms did not have data for severity nor frequency
# "ps_sense"     "ps_headache"  "ps_itching"   "ps_bald"      "ps_menstrual" "ps_menopause" "ps_fertility" "ps_sex" 


# Generate a long data frame of all of the symptom dimention means
pasc_sx_means <- bind_rows(
    burden_df_long,
    frequency_df_long,
    severity_df_long) %>%
  mutate(response = ifelse(response == -88, NA, response)) %>%
  group_by(symptom, dimension) %>%
  summarise(mean = mean(response, na.rm = TRUE), .groups = "drop")


# Stigma score ----------------------------------------------------------------
# Calculated as the mean of the item serious_pcc through talk_pcc for those
# that selected "Yes" to pcc_appts only. If more the 2 of the items are missing
# then the score is set to NA.
data <- bind_cols(
  data,
  (data %>%  
  # filter(pcc_appts == "Yes") %>% # Removed to use bind_cols() instead of a 
    # left_join() approach.
  select(serious_pcc:talk_pcc) %>%
  mutate(across(everything(), ~ case_match(.x, 
      "Never" ~ 1,
      "Rarely" ~ 2,
      "Sometimes" ~ 3, 
      "Often" ~ 4,
      "Very Often" ~ 5,
      .default = NA))) %>%
  mutate(na_counts = rowSums(is.na(across(everything())))) %>%   
  mutate(stigma_score = rowMeans(across(serious_pcc:talk_pcc), na.rm = TRUE)) %>%
  mutate(stigma_score = ifelse(na_counts >= 2, NA, stigma_score)) %>%
  select(-na_counts, -(serious_pcc:talk_pcc)) # These columns are removed 
    # because the na_counts isn't needed, and the categorical responses are
    # needed for tabulation of individual questions.
  )
)

# Mean score of "lack of understanding" scale from Illness Invalidation -------
# Inventory for MDC
data <- bind_cols(
  data,
  (data %>%
  # filter(mdc_appts == "Yes") %>%
  select(serious_mdc:talk_mdc) %>%
  mutate(across(everything(), ~ case_match(.x, 
      "Never" ~ 1,
      "Rarely" ~ 2,
      "Sometimes" ~ 3, 
      "Often" ~ 4,
      "Very Often" ~ 5,
      .default = NA))) %>%
  mutate(na_counts = rowSums(is.na(across(everything())))) %>%   
  mutate(undrstn_score = rowMeans(across(serious_mdc:talk_mdc), na.rm = TRUE)) %>%
  mutate(undrstn_score = ifelse(na_counts >= 2, NA, undrstn_score)) %>%
  select(-na_counts, -(serious_mdc:talk_mdc))
  )
)

# Health Related Social Needs -------------------------------------------------
## Problems
data %<>%
  mutate(across(living_probs___1:living_probs___8, ~ factor(ifelse(.x == "Checked", 1, 0), levels = c(0, 1))))

## Food
data %<>%
  mutate(across(food_worry:food_last, ~ factor(.x, levels = c("Often true", "Sometimes true", "Never true"))))

## Transportation
data %<>%
  mutate(across(transport, ~ factor(.x, levels = c("Yes", "No"))))

# Pull HRSN data as numerical values, filter to those in the t1 / tab_data (all consented so far)
hrsn_data <- 
  pull_redcap_report(Sys.getenv("LC_patient"), "176060", "raw", "raw", "false") %>%
  select(
    promis_record_id, 
    redcap_event_name,
    living_sit:lonely) %>%
  filter(
    redcap_event_name == "consent_and_t1_sur_arm_1", 
    promis_record_id %in% (data %>% filter(redcap_event_name == "t1") %>% pull(record_id)))

# Recode the numerical values into binary values according to scoring guide
hrsn_data %<>%
  mutate(
    living_sit = ifelse(living_sit %in% c(2,3), 1, 0),
    living_prob = rowSums(across(living_probs___1:living_probs___7)),
    living_need = ifelse((living_sit + living_prob) > 0, 1, 0),
    food_worry = ifelse(food_worry %in% c(1, 2), 1, 0),
    food_last = ifelse(food_last %in% c(1, 2), 1, 0),
    food_need = ifelse((food_worry + food_last) > 0, 1, 0),
    transport = ifelse(transport == 1, 1, 0),
    utlities = ifelse(utlities %in% c(1, 2), 1, 0),
    fin_strain = ifelse(fin_strain %in% c(1, 2), 1, 0),
    help_daytoday = ifelse(help_daytoday %in% c(3, 4), 1, 0),
    lonely = ifelse(lonely > 3, 1, 0),
    social_need = ifelse((help_daytoday + lonely) > 0, 1, 0))

# Calculate the hrsn_need_score
hrsn_data %<>%
  mutate(hrsn_need_score = rowSums(across(c(living_need, food_need, transport, utlities, fin_strain, social_need))))
# /////////////////////////////////////////////////////////////////////////////
# Carlos Rodriguez Ph.D. CU Anschutz Dept. of Family Medicine

# Description: This script was designed to be analyze the employee survey 
# practice culture responses by clinic.

# Status: Work in progress
# Last updated: 08/17/2026

# /////////////////////////////////////////////////////////////////////////////

# Load packages
library(magrittr, include = "%<>%")
library(dfmtbx)
library(tidyverse)
library(gtsummary)
library(readxl)

# Load the invites per clinic
invites_per_clinic <- read_csv("D:\\long_covid\\employee_survey\\number_of_invites_per_clinic.csv")

# Load practice culture per clinic
practice_culture <- read_xlsx("D:\\long_covid\\employee_survey\\practice_culture_by_clinic.xlsx") %>%
  select(practice_name:max) %>%
  rename(n = `n response`)

# Join the number of invites per clinic.
practice_culture <- left_join(
  practice_culture,
  invites_per_clinic %>% rename(invites = n),
  by = "practice_name"
)

# Plot culture average mean by number of responses in the clinic
practice_culture %>%
  ggplot(., aes(x = n, y = mean)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  facet_wrap(~ variable)

# Plot culture average mean by number of invites in the clinic
practice_culture %>%
  ggplot(., aes(x = invites, y = mean)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  facet_wrap(~ variable)

# Set the practice culture variables
pc_vars <- c("CultureAvg", "pc_chs_subscore", "pc_imp_subscore", "pc_rel_subscore")

# Correlation with the number of responses
cor_tests_n <- pc_vars %>%
  map(., 
    ~ cor.test(
      practice_culture %>% filter(variable == .x) %>% pull(n),
      practice_culture %>% filter(variable == .x) %>% pull(mean))
    )


# Correlation with the number of invites
cor_tests_invites <- pc_vars %>%
  map(., 
    ~ cor.test(
      practice_culture %>% filter(variable == .x) %>% pull(invites),
      practice_culture %>% filter(variable == .x) %>% pull(mean))
    )


# Display the individual subscales and overall average 
View(
  cor_tests_n %>%
  map_dfr(.f = ~ tibble(
  r.value = round(.x$estimate, 4),
  p.value = round(.x$p.value, 4))
 ) %>%
mutate(variable = pc_vars) %>%
select(variable, everything())
)

View(
  cor_tests_invites %>%
  map_dfr(.f = ~ tibble(
  r.value = round(.x$estimate, 4),
  p.value = round(.x$p.value, 4))
 ) %>%
mutate(variable = pc_vars) %>%
select(variable, everything())
)


# Correlations between practice culture and practice enrollments
# Load practice enrollments
practice_enrollments <- read_csv("D:\\long_covid\\patient_survey_accrual\\enrolled_per_month.csv", show_col_types = FALSE)

# Data prep to get the total number of enrollments per clinic from the last
# row after grouping
practice_enrollments_counts <- practice_enrollments %>%
  # filter(site_name %in% practice_culture$practice_name) %>%
  drop_na(year_mon) %>%
  group_by(site_name) %>%
  slice_tail() %>%
  select(-total_enrolled, -year_mon) %>%
  rename(practice_name = site_name)

# Merged practice culture and the enrollment values
merged_df <- left_join(
  practice_culture %>% filter(variable == "CultureAvg"),
  (practice_enrollments %>%
  # filter(site_name %in% practice_culture$practice_name) %>%
  drop_na(year_mon) %>%
  group_by(site_name) %>%
  slice_tail() %>%
  select(-total_enrolled, -year_mon) %>%
  rename(practice_name = site_name)),
  by = "practice_name") %>%
  mutate(cumulative_enrolled = ifelse(is.na(cumulative_enrolled), 0, cumulative_enrolled))


merged_df %>%
  ggplot(., aes(x = mean, y = cumulative_enrolled)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

# Test relationship between number enrolled and practice culture
cor.test(
    merged_df$mean,
    merged_df$cumulative_enrolled
  )

# index  <- merged_df$cumulative_enrolled > 0
# cor.test(
#     merged_df$mean[index],
#     merged_df$cumulative_enrolled[index]
#   )
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez PhD. CU Anschutz Dept. of Family Medicine
# 06-17-2025
# modified 07-09-2025 to place output in a finalized Egnyte directory
# Run the participant accrual report

# This file is meant to be launched via a .bat file to automate its execution on
# regular basis.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(here)
library(quarto)
library(stringr)

# Set file_in to the copied and renamed clinic-specific .qmd file. Serves as
# an input to the quarto_render() function
file_in <- here(
  "patient_survey_accrual/patient_accrual_report.qmd"
)

# Render the report
quarto_render(
  input = file_in
)

# Place a copy of the most recent report in the Egnyte folder -----------------
from <- here(
  "patient_survey_accrual/patient_accrual_report.html"
)

print(getwd())

print(from)

to <- str_c(
  "Z:/Shared/DFM/AHRQ_Long COVID/Patient Survey Accrual Reports/patient_survey_accrual_report_", 
  Sys.Date(),
  ".html"
)

print(to)

file.exists(to)

# The names of the arguments and the paths are the same. Needs from = from and
# to = to to make it explicity that the keyword is set to the input path.
file.copy(from = from, to = to, overwrite = TRUE)
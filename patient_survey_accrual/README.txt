This directory contains files for generating the patient accrual report.

1. Windows Task Scheduler is configured to launch automate_patient_accrual.bat
  every Monday at 8:30AM.
2. automate_patient_accrual.bat sets the PATH to R 4.4.1 to ensure the correct
  version of R is used, otherwise renv will fail. It then proceeds to launch 
  an R script (render_patient_accrual_report.R) and divert output to a log file.
3. render_patient_accrual_report.R renders the patient_accrual_report.qmd file 
  to generate an html report, moves the output to an Egnyte directory, and then
  appends the date to the file name.
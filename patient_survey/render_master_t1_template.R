# /////////////////////////////////////////////////////////////////////////////
# 
# 
# 
# /////////////////////////////////////////////////////////////////////////////

# Load libraries ---------------------------------------------------------------
library(tidyverse)
library(here)
library(furrr)

# Set the input clinics, output format parameters, and number of cores ---------
# by_vars <- c("by_timepoint")

by_vars <- c("by_clinic", "by_site_type", "by_timepoint")

format <- "docx"

cores <- 5

# Create reports function ------------------------------------------------------
create_reports <- function(by_var) {

  # Relative to the root project directory, set the path to the master layout
  # .qmd file
  layout <-  "./patient_survey/master_t1_template_by_variable.qmd"

  # Create a variable-specific .qmd file. Copy and rename the master layout
  # because the same file can't be read in multiple future sessions in parallel.
  file.copy(
    from = layout,
    to = here("patient_survey", str_c(sub(" ", "", by_var), "_temp.qmd")),
    overwrite = TRUE
  )

  # Set file_in to the copied and renamed clinic-specific .qmd file. Serves as
  # an input to the quarto_render() function
  file_in <- here("patient_survey", str_c(sub(" ", "", by_var), "_temp.qmd"))

  # Set file_out to the file name of the rendered report. Serves as a parameter
  # in the output-file option of the clinic-specific .qmd file.
  file_out <- str_c(by_var, ".", format)

  # Render the report, this will output the report to the root directory.
  quarto::quarto_render(
    input = file_in,
    execute_params = list(by_variable = by_var),
    output_format = "docx",
    output_file = file_out
  )

  # The reports are initially placed in the root project directory. Copy the
  # output .docx file to the reports directory
  file.copy(
    from = here(file_out),
    # to = here("deliverables/reports", str_c(by_var, ".", format)),
    to = here("patient_survey", by_var),
    overwrite = TRUE
  )

  # Remove the output .docx file from the root project directory. Uses a
  # relative path
  file.remove(here(file_out))

  # Remove the copy of the master layout .qmd file. Uses an absolute Path
  file.remove(file_in)

}

# Set furrr options ------------------------------------------------------------
options(future.rng.onMisuse = "ignore")
plan(multisession, workers = cores)

# Render reports in parallel ---------------------------------------------------
system.time(by_vars %>% future_walk(~ create_reports(.x)))

# Render reports in serial -----------------------------------------------------
# system.time(by_vars %>% walk(~ create_reports(.x)))
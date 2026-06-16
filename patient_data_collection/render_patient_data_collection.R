# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(here)
library(quarto)
library(stringr)

# %% 
# Set file_in to the copied and renamed clinic-specific .qmd file. Serves as
# an input to the quarto_render() function
file_in <- here(
  "patient_data_collection/patient_data_collection.qmd"
)

# Render the report
quarto_render(
  input = file_in
)


# Out file
file_out <- here(
    str_c("patient_data_collection/patient_data_collection_", Sys.Date(), ".docx")
)

# Rename
file.rename(
  from = here("patient_data_collection/patient_data_collection.docx"), 
  to = file_out
)


# Send to email
library(Microsoft365R)

# Authenticate and get your Outlook object
outlook <- get_business_outlook()  # or get_personal_outlook()

 
# Create and send an email
email <- outlook$create_email(
  # body = "Patient data collection",
  subject = "AHRQ Long COVID: Patient data collection report",
  to = c(
    "carlos.i.rodriguez@cuanschutz.edu",
    "ISABELLA.NOWAKOWSKI@CUANSCHUTZ.EDU",
    "SARAH.JOLLEY@CUANSCHUTZ.EDU",
    "marissa.morales@uchealth.org"
    )
)

# Add the attachment
email$add_attachment(file_out)

# Send
email$send()
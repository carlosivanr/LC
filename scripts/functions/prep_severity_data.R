# /////////////////////////////////////////////////////////////////////////////
# Get PASC Severity Data
# /////////////////////////////////////////////////////////////////////////////

# Display a table of the burden, severity, and frequency 
# To display a table where each column represents the mean of burden, frequency, and severity
# might be good to get a long data frame with 3 variables, symptom, value, dimension

# Import data as numerical since there are inconsistencies in the values/response options for each
# symptom, so designing an algorithm like mutate(across(cols)) to convert the label values to numeric
# would not be wise
pasc_severity_data <- 
  pull_redcap_report(Sys.getenv("LC_patient"), "176060", "raw", "raw", "false") %>%
  select(
    promis_record_id, 
    redcap_event_name,
    starts_with("ps_")) %>%
  rename(record_id = promis_record_id) %>%
  filter(redcap_event_name %in% c("consent_and_t1_sur_arm_1", "t5_survey_arm_1"))

# The burden, severity, and frequency scores are all on 5 point scales, but 
# the values will range from 0-4 or 1-5. This will harmonize the scale for 
# plotting.

# This currently only modifies the frequency and severity columns, but should
# be modified to also modify the burden columns, and then change how df_long 
# is modified to handle the NAs (-88).
# Rename the headache and itching symptoms to harmonize to a common nomenclature so they can be plotted via tiles
pasc_severity_data <- pasc_severity_data %>%
  rename(
    ps_headache_freqdepaul = ps_headache_freq,
    ps_headache_sevdepaul = ps_headache_sev,
    ps_itching_burden = ps_itching_itchburden,
    ps_itching_freqdepaul = ps_itching_itchfreq,
    ps_itching_sevdepaul = ps_itching_itchsev) %>%
  mutate(across(starts_with("ps_"), ~ifelse(.x == -88, NA, .x))) %>%
  mutate(across(ends_with("sevdepaul") | ends_with("freqdepaul"), ~ .x + 1))


# Prepare the pasc severity data for plotting ---------------------------------
make_pasc_tile_plot <- function(df_in, t) {
  # df is the input pasc severity data, 
  # t is the timepoint either ps or ps2

  # Dimensions RedCap field name suffixes
  dimensions <- c("burden", "sevdepaul", "freqdepaul")

  # Initialize an empty data frame
  pasc_dims_long <- data.frame()

    for (d in dimensions) {
    print(d)
    print(t)

    # Select the severity dimension names 
    df <- df_in %>%
      select(starts_with(t) & ends_with(d))

    # Set the names to avector
    names_df <- names(df)

    # Remove the suffix
    names_df_modified <- sub("_[^_]*$", "", names_df)

    # Set the modified names to the df
    names(df) <- names_df_modified

    df_long <- df %>%
      pivot_longer(cols = everything(), names_to = "symptom", values_to = "response") %>%
      mutate(dimension = d)

    pasc_dims_long <- pasc_dims_long %>% bind_rows(df_long)

    }

  # Summarise the for loop output
  pasc_summarised <- pasc_dims_long %>%
    mutate(response = ifelse(response == -88, NA, response)) %>%
    group_by(symptom, dimension) %>%
    summarise(mean = mean(response, na.rm = TRUE), .groups = "drop")

  # Make plot
  plt <- pasc_summarised %>%
  mutate(symptom = factor(symptom, levels = fct_recode(ordered_symptoms, "ps_eyedry" = "ps_dryeyes") )) %>%
  ggplot(., aes(x = dimension, y = symptom, fill = mean)) +
  geom_tile(color = "white", linewidth = 0.5, width = 0.5) +
  geom_text(aes(label = scales::number(mean, accuracy = 0.01)), color = "white", size = 3.5) +
  scale_fill_gradient(
    low = "steelblue",
    high = "tomato",
    guide = "none") +
  labs(
    title = "", 
    x = "Dimension", 
    y = "Symptom") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    # axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right") +
  scale_y_discrete(expand = c(0, 0)) +            # remove y padding
  coord_fixed(ratio = .3) +                       # taller cells → slimmer columnslook
  theme_minimal(base_size = 12)                   # add grid lines
  
  return(plt)

}
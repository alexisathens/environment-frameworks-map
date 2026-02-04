library(tidyverse)
library(readxl)
library(magrittr)
library(here)

## Read in framework implementation files:
# ESSAT > Framework for the Development of Environment Statistics (FDES) > found at https://unstats.un.org/unsd/envstats/fdes/essat.cshtml
# CISAT > Global Set of Climate Change Statistics and Indicators (GS) > found at https://unstats.un.org/unsd/envstats/Climate%20Change/cisat.cshtml

# ------------------------------------------------------------------------------
# FDES: Read and clean all component sheets from ESSAT
# ------------------------------------------------------------------------------

clean_fdes_component <- function(sheet_name, file_path) {

  # Read raw data (no headers - we'll handle manually)
  raw <- read_excel(file_path, sheet = sheet_name, col_names = FALSE)

  # Extract component number and name from first row
  component_full <- raw[[1, 1]]
  component_num <- str_extract(component_full, "^Component (\\d+)", group = 1)
  component_name <- str_extract(component_full, "^Component \\d+:\\s*(.+)$", group = 1)

  # Select and rename the key columns we need
  # Col 1 (index 0): hierarchy labels (subcomponent, topic, subtopic)
  # Col 2 (index 1): statistic description
  # Col 3 (index 2): category of measurement
  # Col 4 (index 3): potential aggregations
  df <- raw %>%
    select(col_0 = 1, col_1 = 2, col_2 = 3, col_3 = 4) %>%
    slice(-(1:4))  # Remove header rows

  # Initialize hierarchy tracking
  current_subcomponent <- NA_character_
  current_subcomponent_name <- NA_character_
  current_topic <- NA_character_
  current_topic_name <- NA_character_
  current_subtopic <- NA_character_
  current_subtopic_name <- NA_character_

  # Process each row to extract hierarchy
  result <- df %>%
    mutate(
      # Detect row types based on patterns in col_0
      is_subcomponent = str_detect(col_0, "^Sub-component \\d+\\.\\d+", negate = FALSE) & !is.na(col_0),
      is_topic = str_detect(col_0, "^Topic \\d+\\.\\d+\\.\\d+", negate = FALSE) & !is.na(col_0),
      is_subtopic = str_detect(col_0, "^[a-z]\\.", negate = FALSE) & !is.na(col_0),
      is_numbered_statistic = str_detect(col_1, "^\\d+\\.", negate = FALSE) & !is.na(col_1)
    )

  # Build output with forward-filled hierarchy
  output <- list()

  for (i in seq_len(nrow(result))) {
    row <- result[i, ]

    # Step 1: Update hierarchy based on row type (order matters!)
    if (row$is_subcomponent) {
      current_subcomponent <- str_extract(row$col_0, "\\d+\\.\\d+")
      current_subcomponent_name <- str_extract(row$col_0, "^Sub-component \\d+\\.\\d+:\\s*(.+)$", group = 1)
      current_topic <- NA_character_
      current_topic_name <- NA_character_
      current_subtopic <- NA_character_
      current_subtopic_name <- NA_character_
      next  # Subcomponent rows never have statistics
    }

    if (row$is_topic) {
      current_topic <- str_extract(row$col_0, "\\d+\\.\\d+\\.\\d+")
      current_topic_name <- str_extract(row$col_0, "^Topic \\d+\\.\\d+\\.\\d+:\\s*(.+)$", group = 1)
      current_subtopic <- NA_character_
      current_subtopic_name <- NA_character_
      next  # Topic rows never have statistics
    }

    if (row$is_subtopic) {
      current_subtopic <- str_extract(row$col_0, "^([a-z])\\.", group = 1)
      current_subtopic_name <- str_extract(row$col_0, "^[a-z]\\.\\s*(.+)$", group = 1)
      # Don't use 'next' here - subtopic rows may also have a numbered statistic!
    }

    # Step 2: Capture statistics
    if (row$is_numbered_statistic) {
      # This is a numbered statistic (e.g., "1. Carbon dioxide (CO2)")
      stat_num <- str_extract(row$col_1, "^(\\d+)\\.", group = 1)
      stat_name <- str_extract(row$col_1, "^\\d+\\.\\s*(.+)$", group = 1)

      output[[length(output) + 1]] <- tibble(
        component = component_num,
        component_name = component_name,
        subcomponent = current_subcomponent,
        subcomponent_name = current_subcomponent_name,
        topic = current_topic,
        topic_name = current_topic_name,
        subtopic = current_subtopic,
        subtopic_name = current_subtopic_name,
        statistic_num = stat_num,
        statistic_name = stat_name,
        category_of_measurement = row$col_2#,
        #aggregations = row$col_3
      )
    } else if (row$is_subtopic && (!is.na(row$col_2) || !is.na(row$col_3))) {
      # This is a subtopic that IS the statistic (no numbered sub-items)
      # e.g., "b. Imports of minerals" with measurement info but no numbered stat
      output[[length(output) + 1]] <- tibble(
        component = component_num,
        component_name = component_name,
        subcomponent = current_subcomponent,
        subcomponent_name = current_subcomponent_name,
        topic = current_topic,
        topic_name = current_topic_name,
        subtopic = current_subtopic,
        subtopic_name = current_subtopic_name,
        statistic_num = NA_character_,
        statistic_name = current_subtopic_name,  # Use subtopic name as statistic
        category_of_measurement = row$col_2#,
        #aggregations = row$col_3
      )
    }
  }

  bind_rows(output)
}

# Read all 6 components
essat_path <- here("Frameworks/PartII_ESSAT.xlsm")
component_sheets <- paste("Component", 1:6)

fdes <- map(component_sheets, ~clean_fdes_component(.x, essat_path)) %>%
  bind_rows() %>%
  # Create a unique indicator ID based on hierarchy
  mutate(
    indicator_id = paste(
      #component,
      #subcomponent,
      topic,
      subtopic,
      statistic_num,
      sep = "."
    ) %>% str_replace_all("\\.NA", "")
  ) %>%
  relocate(indicator_id)

# Preview the cleaned data
glimpse(fdes)

#write_csv(fdes, here("Frameworks/fdes_flat.csv"))

# ------------------------------------------------------------------------------
# Global Set: Read and clean from CISAT
# ------------------------------------------------------------------------------

clean_gs <- function(file_path) {

  # Read raw data (no headers - we'll handle manually)
  raw <- read_excel(file_path, sheet = "Self assessment tool", col_names = FALSE)

  # Select and rename key columns, skip header rows (first 5 rows)
  # Col 2 (B): Area (DRIVERS, IMPACTS, etc.)
  # Col 3 (C): Topic
  # Col 4 (D): Indicator number
  # Col 5 (E): Indicator name
  # Col 6 (F): Statistic
  df <- raw %>%
    select(area = 2, topic = 3, indicator_num = 4, indicator_name = 5, statistic = 6) %>%
    slice(-(1:5))  # Remove header rows

  # Filter out Area header values that aren't actual areas
  valid_areas <- c("DRIVERS", "IMPACTS", "VULNERABILITY", "MITIGATION", "ADAPTATION")

  # Forward-fill area and topic using tidyr::fill()
  df <- df %>%
    mutate(
      area = if_else(area %in% valid_areas, area, NA_character_)
    ) %>%
    fill(area, .direction = "down")

  # Forward-fill topic (but reset when area changes or new topic appears)
  df <- df %>%
    mutate(
      # Detect topic rows (non-NA topic that isn't a number)
      is_topic_row = !is.na(topic) & is.na(suppressWarnings(as.numeric(topic))),
      topic = if_else(is_topic_row, topic, NA_character_)
    ) %>%
    fill(topic, .direction = "down") %>%
    select(-is_topic_row)

  # Forward-fill indicator info for rows that only have statistics
  df <- df %>%
    mutate(
      # Convert indicator_num to character for consistency
      indicator_num = as.character(indicator_num),
      # Keep only numeric indicator numbers
      indicator_num = if_else(str_detect(indicator_num, "^\\d+$"), indicator_num, NA_character_)
    ) %>%
    fill(indicator_num, indicator_name, .direction = "down")

  # Keep only rows that have meaningful content (statistic or indicator name)
  # Filter to rows where we have valid indicator info
  df <- df %>%
    filter(!is.na(indicator_num)) %>%
    # Clean up: if statistic is NA or empty, use indicator name
    mutate(
      statistic = if_else(
        is.na(statistic) | statistic == "" | statistic == "Equivalent to the indicator",
        indicator_name,
        statistic
      )
    ) %>%
    # Remove duplicate rows (where statistic equals indicator and no additional info)
    distinct()

  # Create indicator ID
  df <- df %>%
    mutate(
      indicator_id = paste0("GS.", indicator_num)
    ) %>%
    relocate(indicator_id)

  df
}

gs <- clean_gs(here("Frameworks/CISAT_Part_2.xlsx"))

# Preview the cleaned data
glimpse(gs)

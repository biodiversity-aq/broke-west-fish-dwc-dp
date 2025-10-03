library(tidyverse)
library(jsonlite)

transform_rmt_count_to_long <- function(df){
  #' Convert wide count table into long table
  #' 
  #' @param df A tibble of the count table with taxon as columns and trawl event as rows
  #' @return A list of 2 tibbles, one is survey_target, the other is survey_target_abundance table
  trawl <- df %>%
    pivot_longer(cols = 5:ncol(df),
                 names_to = "verbatimIdentification",
                 values_to = "organismQuantity",
                 values_drop_na = TRUE) %>%
    mutate(
      organismQuantityType = case_when(organismQuantity >= 1 ~ "individuals", TRUE ~ "individual"),
      life_stage = case_when(
        str_detect(verbatimIdentification, " all$") ~ "",
        str_detect(verbatimIdentification, " Larvae$") ~ "Larvae",
        str_detect(verbatimIdentification, " Postmeta$") ~ "Postmeta",
        TRUE ~ ""
      ),
      lifeStage = life_stage, # create extra lifeStage column for occurrence table
      # use field name "taxon" for surveyTargetType vocabulary later
      taxon = str_remove_all(verbatimIdentification, "\\s*(all|Larvae|Postmeta|sp\\.)$"),
      station_number_cleaned = str_remove_all(`Station number`, "\\s*"), # Remove white spaces
      surveyID = str_c("BROKE_WEST_RMT_", station_number_cleaned, "_RMT8"),
      # need to cast body size to string otherwise double cannot be in the same column with string after pivot_long
      # only use size range for RMT8 here because there is no count data for RMT1 
      `minimum body size` = "0.85",
      `maximum body size` = "3"
    )
  
  trawl_taxa <- wm_records_names(unique(trawl$taxon)) %>%
    # the taxon match returns list of lists, this function creates a table of the following fields out of the lists
    map_df(~ select(.x, scientificname, lsid, AphiaID, rank, kingdom)) %>%
    rename(taxon = scientificname, taxonID = lsid, taxonRank = rank) %>%
    mutate(taxon_rank = taxonRank) # create extra taxonRank column for occurrence table
  
  trawl_count <- trawl %>% 
    left_join(trawl_taxa, by = "taxon") %>%
    mutate(surveyTargetID = case_when(
      lifeStage != "" ~ str_c(surveyID, AphiaID, lifeStage, sep = "_"),
      TRUE ~ str_c(surveyID, AphiaID, sep = "_")))
  
  trawl_long <- trawl_count %>%
    pivot_longer(
      cols = c("taxon", "life_stage", "taxon_rank", "minimum body size", "maximum body size"),
      names_to = "surveyTargetType",
      values_to = "surveyTargetValue",
      values_drop_na = TRUE
    ) %>%
    mutate(
      surveyTargetUnit = case_when(
        surveyTargetType == "minimum body size" ~ "mm",
        surveyTargetType == "maximum body size" ~ "m",
        TRUE ~ NA
      ),
      surveyTargetUnitIRI = case_when(
        surveyTargetUnit == "mm" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UXMM/",
        surveyTargetUnit == "m" ~ "http://vocab.nerc.ac.uk/collection/P06/current/ULAA/",
        TRUE ~ NA
      ),
      surveyTargetValueIRI = case_when(
        surveyTargetType == "taxon" ~ taxonID,
        surveyTargetValue == "Genus" ~ "http://rs.tdwg.org/dwc/terms/genus",
        surveyTargetValue == "Species" ~ "http://rs.tdwg.org/dwc/terms/specificEpithet",
        surveyTargetValue == "Family" ~ "http://rs.tdwg.org/dwc/terms/family",
        TRUE ~ NA
      ),
      surveyTargetType = case_when(
        surveyTargetType == "life_stage" ~ "lifeStage",
        surveyTargetType == "taxon_rank" ~ "taxonRank",
        TRUE ~ surveyTargetType
      ),
      surveyTargetTypeIRI = case_when(
        surveyTargetType == "taxon" ~ "http://rs.tdwg.org/dwc/terms/taxon",
        surveyTargetType == "lifeStage" ~ "http://rs.tdwg.org/dwc/terms/lifeStage",
        surveyTargetType == "taxonRank" ~ "http://rs.tdwg.org/dwc/terms/taxonRank",
        surveyTargetType == "maximum body size" ~ "http://vocab.nerc.ac.uk/collection/P01/current/OBSMAXLX/",
        surveyTargetType == "minimum body size" ~ "http://vocab.nerc.ac.uk/collection/P01/current/OBSMINLX/",
        TRUE ~ NA
      ),
      includeOrExclude = "include",
      isSurveyTargetFullyReported = "true"
    ) %>% filter(!(surveyTargetType == "lifeStage" & surveyTargetValue == "")) # remove empty lifeStage row
  
  survey_target <- trawl_long %>%
    select(surveyTargetID, surveyID, surveyTargetType, surveyTargetTypeIRI, surveyTargetValue, surveyTargetValueIRI, surveyTargetUnit, surveyTargetUnitIRI)
  
  occurrence <- trawl_long %>%
    filter(surveyTargetType == "taxon" & organismQuantity != 0) %>%
    rename(
      scientificName = surveyTargetValue, 
      eventID = surveyID) %>%
    mutate(
      occurrenceStatus = "detected",
      recordedBy = "Anton Van de Putte",
      recordedByID = "https://orcid.org/0000-0003-1336-5554",
      identifiedBy = "Anton Van de Putte",
      identifiedByID = "https://orcid.org/0000-0003-1336-5554",
    )
  return(list(survey_target = survey_target, occurrence = occurrence))
}

save_and_write <- function(data_list, output_dir = here("data", "output")) {
  # Ensure the output directories exist
  dir.create(here(output_dir, "rda"), recursive = TRUE, showWarnings = FALSE)
  dir.create(here(output_dir, "tsv"), recursive = TRUE, showWarnings = FALSE)
  
  # Iterate over each dataset in the list
  for (name in names(data_list)) {
    # Get the current dataset
    current_data <- data_list[[name]]
    
    # Save as .rda with the correct name
    # Create a temporary environment
    temp_env <- new.env()
    # Assign the data to the environment with the desired name
    assign(name, current_data, envir = temp_env)
    # Save the object from the environment
    save(list = name, file = here(output_dir, "rda", paste0(name, ".rda")), 
         envir = temp_env)
    
    # Save as .tsv
    write_tsv(current_data, here(output_dir, "tsv", paste0(name, ".txt")), na = "")
  }
}

filter_dataframe_by_json <- function(df, json_url) {
  # Read JSON from the URL
  json_data <- fromJSON(json_url)
  
  # Extract field names safely
  field_names <- json_data$fields$name %>% na.omit() %>% unique()
  
  # Filter the dataframe to keep only matching columns
  filtered_df <- df %>% select(any_of(field_names))
  
  return(filtered_df)
}
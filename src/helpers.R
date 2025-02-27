library(tidyverse)
library(jsonlite)

save_and_write <- function(data_list, output_dir = here("data", "output")) {
  # Ensure the output directories exist
  dir.create(here(output_dir, "rda"), recursive = TRUE, showWarnings = FALSE)
  dir.create(here(output_dir, "tsv"), recursive = TRUE, showWarnings = FALSE)
  
  # Iterate over each dataset in the list
  for (name in names(data_list)) {
    data <- data_list[[name]]
    
    # Save as .rda
    save(data, file = here(output_dir, "rda", paste0(name, ".rda")))
    
    # Save as .tsv
    write_tsv(data, here(output_dir, "tsv", paste0(name, ".txt")), na = "")
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
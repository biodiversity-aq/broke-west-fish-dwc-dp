library(tidyverse)
library(jsonlite)

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
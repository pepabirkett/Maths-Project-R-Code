# Load required libraries
library(dplyr)

# Function to standardize function word files
standardize_function_words <- function(input_folder, output_folder) {
  
  # Create output folder if it doesn't exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  
  # Get list of files in the input folder
  file_list <- list.files(input_folder, full.names = TRUE)
  
  # Process each file
  for (file in file_list) {
    # Load function word vector
    function_word_vector <- as.numeric(unlist(strsplit(readLines(file), ",")))
    
    # Standardize (mean = 0, sd = 1)
    standardized_vector <- scale(function_word_vector)
    
    # Convert to a single line for saving
    standardized_text <- paste(standardized_vector, collapse = ",")
    
    # Define output file path
    output_file <- file.path(output_folder, basename(file))
    
    # Write standardized vector to the output folder
    writeLines(standardized_text, output_file)
    
    cat("Standardized:", basename(file), "\n")
  }
  
  cat("All files have been standardized and saved to", output_folder, "\n")
}

# Usage
input_folder <- "C:/Users/12145/OneDrive/Desktop/year_4/Mathematics_Project/dataset/normalized_movie_dialogue_fw"        # Folder with original function word files
output_folder <- "C:/Users/12145/OneDrive/Desktop/year_4/Mathematics_Project/dataset/normalized&standarized_movie_dialogue_fw"      # Folder to save standardized files

# Run the function
standardize_function_words(input_folder, output_folder)



# Load necessary library
library(stringr)

# Function to normalize function word counts for all files in a folder
normalize_all_function <- function(input_folder, output_folder) {
  # Create output folder if it doesn't exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  
  # Get a list of all files in the input folder
  input_files <- list.files(input_folder, full.names = TRUE)
  
  # Loop through each file in the folder
  for (file_path in input_files) {
    # Read the data from the file
    # Assuming the file contains a single line of comma-separated values
    word_counts <- as.numeric(unlist(strsplit(readLines(file_path), ",")))
    
    # Normalize the word counts
    total_sum <- sum(word_counts)
    normalized_counts <- word_counts / total_sum
    
    # Create output file path
    output_file <- file.path(output_folder, basename(file_path))
    
    # Write the normalized counts to the output file
    writeLines(paste(normalized_counts, collapse = ","), output_file)
  }
  
  print("Normalization complete. Files saved to the output folder.")
}

# Example usage
input_folder = "C:/Users/12145/OneDrive/Desktop/year_4/Mathematics_Project/dataset/The_Counselor"
output_folder = "C:/Users/12145/OneDrive/Desktop/year_4/Mathematics_Project/dataset/The_Counselor/The_Counselor_normalized_fw.txt"
# Normalize all files in the folder
normalize_all_function(input_folder, output_folder)


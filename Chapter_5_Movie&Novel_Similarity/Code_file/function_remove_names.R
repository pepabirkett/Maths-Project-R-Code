# Load necessary packages
library(stringr)
library(readr)

# Define the function to clean dialogues
clean_dialogues <- function(input_file, output_file) {
  # Read the text file line-by-line
  text_lines <- read_lines(input_file)
  
  # Initialize a vector to store cleaned dialogues
  cleaned_dialogues <- c()
  
  # Process each line
  for (line in text_lines) {
    # Check if the line contains a colon (indicating a name and dialogue)
    if (str_detect(line, ":")) {
      # Remove everything before and including the first colon
      dialogue <- str_replace(line, "^[^:]+: ?", "")
      cleaned_dialogues <- c(cleaned_dialogues, dialogue)  # Add the cleaned dialogue
    } else {
      # Keep the line as is if it doesn't contain a colon (e.g., stage directions or empty lines)
      cleaned_dialogues <- c(cleaned_dialogues, line)
    }
  }
  
  # Write the cleaned dialogues to the output file
  write_lines(cleaned_dialogues, output_file)
  print(paste("Cleaned dialogues saved to:", output_file))
}

# Usage
input_file <- "C:/Users/12145/OneDrive/Desktop/year_4/Mathematics_Project/Mathproject_code/Sherlock_holmes/scripts/TV_series_scripts/A_study_in_pink.txt"
output_file <- 'C:/Users/12145/OneDrive/Desktop/year_4/Mathematics_Project/Mathproject_code/Sherlock_holmes/dialogues/TVseries_dia/dia_A_study_in_pink.txt'
# Run the function
clean_dialogues(input_file, output_file)

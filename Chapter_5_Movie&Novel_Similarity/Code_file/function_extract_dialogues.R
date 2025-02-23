# Load necessary packages
library(stringr)
library(readr)

extract_dialogues <- function(input_file, output_file) {
  # Read the entire text as a single string
  text <- read_file(input_file)
  
  # Regex pattern to match text inside:
  # - straight double quotes: "..."
  # - straight single quotes: '...'
  # - curly double quotes: “...”
  # - curly single quotes: ‘...’
  #
  # Explanation:
  #   (pattern1|pattern2|pattern3|pattern4)
  #     pattern1 -> ‘[^’]+’
  #     pattern2 -> “[^”]+”
  #     pattern3 -> "[^"]+"
  #     pattern4 -> '[^']+'
  #
  # We capture *one* of these four possibilities each time.
  
  pattern <- "(‘[^’]+’|“[^”]+”|\"[^\"]+\"|'[^']+')"
  
  # Extract all matches
  dialogues <- str_extract_all(text, pattern)[[1]]
  
  # Strip off leading/trailing quotes
  # We remove ‘, ’, “, ”, " and ' from the start or end of the string
  cleaned_dialogues <- str_replace_all(dialogues, "^[‘“\"']", "")
  cleaned_dialogues <- str_replace_all(cleaned_dialogues, "[’”\"']$", "")
  
  # Write the cleaned dialogues to the output file
  write_lines(cleaned_dialogues, output_file)
  
  print(paste("Dialogues extracted and saved to:", output_file))
}


# # Define function to extract all dialogues word-by-word
# extract_dialogues <- function(input_file, output_file) {
#   # Read the entire text as a single string
#   text <- read_file(input_file)
#   
#   # Find all quoted segments (dialogues) in the text
#   dialogues <- str_extract_all(text, "\"[^\"]+\"")[[1]]
#   
#   # Remove the quotation marks from the dialogues
#   cleaned_dialogues <- str_replace_all(dialogues, "\"", "")
#   
#   # Write the cleaned dialogues to the output file
#   write_lines(cleaned_dialogues, output_file)
#   print(paste("Dialogues extracted and saved to:", output_file))
# }

# Usage

### Example 1
input_file <-" C:/Users/12145/OneDrive/Desktop/year_4/Mathematics_Project/dataset/normalized_pairs_dialogue_fw.rds"
output_file <- "C:/Users/12145/OneDrive/Desktop/year_4/Mathematics_Project/Mathproject_code/Sherlock_holmes/dialogues/TVseries_dia/dialogues_A_STUDY_IN_SCARLET.txt"
#output_file <- "C:/Users/12145/OneDrive/Desktop/year_4/test_A_beautiful_mind.txt"

extract_dialogues(input_file, output_file)

### Example 2
input_file <- "C:/Users/12145/OneDrive/Desktop/year_4/Mathematics_Project/dataset/American_Psycho/book.txt"
output_file <- "C:/Users/12145/OneDrive/Desktop/year_4/Mathematics_Project/dataset/American_Psycho/book_dialogue.txt"

# Run the function
extract_dialogues(input_file, output_file)



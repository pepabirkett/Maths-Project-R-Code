# Remember to install the package if you havent already
# Load necessary packages
library(stringr)
library(dplyr)
library(readr)

# Load frequent words from file
load_frequent_words <- function(file_path) {
  frequent_words <- read_lines(file_path)
  return(frequent_words)
}

# Clean and tokenize text
clean_and_tokenize <- function(text) {
  text <- str_replace_all(text, "[^\\x20-\\x7E]", "") # Remove non-ASCII
  text <- str_replace_all(text, "[,\\.]", " ")        # Replace punctuation with spaces
  text <- str_remove_all(text, "[[:punct:]]")         # Remove all punctuation
  text <- str_replace_all(text, "\\s+", " ")          # Replace multiple spaces with single space
  text <- str_trim(text)                              # Trim leading and trailing whitespace
  text <- tolower(text)                               # Convert to lowercase
  words <- unlist(str_split(text, " "))               # Split text by spaces into words
  return(words)
}

# Create function word vector
create_function_word_vector <- function(wordlist, frequent_words) {
  word_counts <- table(wordlist)                      # Count occurrences of each word
  frequent_word_vec <- sapply(frequent_words, function(w) word_counts[w])
  frequent_word_vec[is.na(frequent_word_vec)] <- 0    # Replace NA with 0 for words not found
  total_words <- length(wordlist)
  frequent_word_count <- sum(frequent_word_vec)
  frequent_word_vec <- c(frequent_word_vec, total_words - frequent_word_count) # Add non-function word count
  names(frequent_word_vec) <- c(frequent_words, "non_function_words") # Name each element
  return(frequent_word_vec)
}

# Main function to process a single file and output results to a text file
process_single_file <- function(input_file, output_file, frequent_words_file) {
  # Load frequent words
  frequent_words <- load_frequent_words(frequent_words_file)
  
  # Read file content
  file_text <- read_file(input_file)                 # Read file contents
  words <- clean_and_tokenize(file_text)             # Clean and tokenize text
  frequent_word_vec <- create_function_word_vector(words, frequent_words)  # Get function word vector
  
  # Write the result to an output file
  write_lines(paste(frequent_word_vec, collapse = ", "), output_file)
}




# Usage

### Example 1



### Example 2
# Specify the path to the input file, output file, and frequent words file
input_file <- "C:/Users/12145/OneDrive/Desktop/year_4/Mathematics_Project/dataset/The_Counselor/The_Counselor_(2013)_Movie_Script.txt"
output_file <- "C:/Users/12145/OneDrive/Desktop/year_4/Mathematics_Project/dataset/The_Counselor/The_Counselor_fw.txt"
frequent_words_file <- "C:/Users/12145/OneDrive/Desktop/year_4/Mathematics_Project/Mathproject_code/wordfile.txt"

# Run the function
process_single_file(input_file, output_file, frequent_words_file)




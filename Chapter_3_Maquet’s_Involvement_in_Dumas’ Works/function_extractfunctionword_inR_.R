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

clean_and_tokenize <- function(text) {
  text <- str_replace_all(text, "[^\\x20-\\x7E\\x80-\\xFF]", "") # Keep ASCII and extended characters
  text <- str_replace_all(text, "[,\\.]", " ")                  # Replace punctuation with spaces
  text <- str_remove_all(text, "[[:punct:]]")                   # Remove all punctuation (except accents)
  text <- str_replace_all(text, "\\s+", " ")                    # Replace multiple spaces with a single space
  text <- str_trim(text)                                        # Trim leading and trailing whitespace
  text <- tolower(text)                                         # Convert to lowercase
  words <- unlist(str_split(text, " "))                         # Split text by spaces into words
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
  frequent_word_vec <- frequent_word_vec / total_words # Normalise counts by total word count
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
# Specify the path to the input file, output file, and frequent words file

# # Pascal Bruno - AD's Early Novels
# input_file <- "C:/Users/cobet/Documents/corpus_files/Alexandre Dumas/Pascal-bruno-(1837)-AD.txt"
# output_file <- "C:/Users/cobet/Documents/corpus_files/Function-Words/Pascal-Bruno-Function-Word-Count.txt"
# frequent_words_file <- "C:/Users/cobet/Documents/corpus_files/Function-Words/most-frequent-french-words-of-the-written-language-19th-20th-century.txt"

# # Pauline - AD's Early Novels
# input_file <- "C:/Users/cobet/Documents/corpus_files/Alexandre Dumas/Pauline-(1838)-AD.txt"
# output_file <- "C:/Users/cobet/Documents/corpus_files/Function-Words/Pauline-Function-Word-Count.txt"
# frequent_words_file <- "C:/Users/cobet/Documents/corpus_files/Function-Words/most-frequent-french-words-of-the-written-language-19th-20th-century.txt"

# # Acte - AD's Early Novels
# input_file <-"C:/Users/cobet/Documents/corpus_files/Alexandre Dumas/Acte-(1838-39)-AD.txt"
# output_file <- "C:/Users/cobet/Documents/corpus_files/Function-Words/Acte-Function-Word-Count.txt"
# frequent_words_file <- "C:/Users/cobet/Documents/corpus_files/Function-Words/most-frequent-french-words-of-the-written-language-19th-20th-century.txt"

# # Le Capitaine Paul - AD's Early Novels
# input_file <-"C:/Users/cobet/Documents/corpus_files/Alexandre Dumas/Le-capitaine-paul-(1838)-AD.txt"
# output_file <- "C:/Users/cobet/Documents/corpus_files/Function-Words/Le-capitaine-paul-Function-Word-Count.txt"
# frequent_words_file <- "C:/Users/cobet/Documents/corpus_files/Function-Words/most-frequent-french-words-of-the-written-language-19th-20th-century.txt"

# # Le Capitaine Pamphile
# input_file <- "C:/Users/cobet/Documents/corpus_files/Alexandre Dumas/Le-capitaine-pamphile-(1839)-AD.txt"
# output_file <- "C:/Users/cobet/Documents/corpus_files/Function-Words/Le-capitaine-pamphile-Function-Word-Count.txt"
# frequent_words_file <- "C:/Users/cobet/Documents/corpus_files/Function-Words/most-frequent-french-words-of-the-written-language-19th-20th-century.txt"

# # Le Chevalier DHarmental
# input_file <- "C:/Users/cobet/Documents/corpus_files/AD-AM Collab/Le-chevalier-dharmental-(1842)-AD-AM.txt"
# output_file <- "C:/Users/cobet/Documents/corpus_files/Function-Words/Le-chevalier-dharmental-Function-Word-Count.txt"
# frequent_words_file <- "C:/Users/cobet/Documents/corpus_files/Function-Words/most-frequent-french-words-of-the-written-language-19th-20th-century.txt"

# # Le Comte de Monte-Cristo
# input_file <- "C:/Users/cobet/Documents/corpus_files/AD-AM Collab/Le-Comte-de-Monte-Cristo-(1844)-AD-AM.txt"
# output_file <- "C:/Users/cobet/Documents/corpus_files/Function-Words/Le-Comte-de-Monte-Cristo-Function-Word-Count.txt"
# frequent_words_file <- "C:/Users/cobet/Documents/corpus_files/Function-Words/most-frequent-french-words-of-the-written-language-19th-20th-century.txt"

# # Les Trois Musquetaires
# input_file <- "C:/Users/cobet/Documents/corpus_files/AD-AM Collab/Les-trois-mousquetaires-(1844)-AD-AM.txt"
# output_file <- "C:/Users/cobet/Documents/corpus_files/Function-Words/Les-Trois-Mousquetaires-Function-Word-Count.txt"
# frequent_words_file <- "C:/Users/cobet/Documents/corpus_files/Function-Words/most-frequent-french-words-of-the-written-language-19th-20th-century.txt"

# # La Tulipe Noire
# input_file <- "C:/Users/cobet/Documents/corpus_files/AD-AM Collab/la-tulipe-noire-(1850)-AD-AM.txt"
# output_file <- "C:/Users/cobet/Documents/corpus_files/Function-Words/La-Tulipe-Noire-Function-Word-Count.txt"
# frequent_words_file <- "C:/Users/cobet/Documents/corpus_files/Function-Words/most-frequent-french-words-of-the-written-language-19th-20th-century.txt"

# # La Reine Margot
# input_file <- "C:/Users/cobet/Documents/corpus_files/AD-AM Collab/La-reine-margot-(1845)-AD-AM.txt"
# output_file <- "C:/Users/cobet/Documents/corpus_files/Function-Words/La-Reine-Margot-Function-Word-Count.txt"
# frequent_words_file <- "C:/Users/cobet/Documents/corpus_files/Function-Words/most-frequent-french-words-of-the-written-language-19th-20th-century.txt"

# Le Vicomte de Bragelonne
input_file <- "C:/Users/cobet/Documents/corpus_files/AD-AM Collab/Le-vicomte-de-bragelonne-(1848)-AD-AM.txt"
output_file <- "C:/Users/cobet/Documents/corpus_files/Function-Words/Le-Vicomte-De-Bragelonne-Word-Count.txt"
frequent_words_file <- "C:/Users/cobet/Documents/corpus_files/Function-Words/most-frequent-french-words-of-the-written-language-19th-20th-century.txt"


# # La Belle Gabrielle
# input_file <- "C:/Users/cobet/Documents/corpus_files/Auguste Maquet/La-belle-gabrielle-(1854-55)-AM.txt"
# output_file <- "C:/Users/cobet/Documents/corpus_files/Function-Words/La-Belle-Gabrielle-Function-Word-Count.txt"
# frequent_words_file <- "C:/Users/cobet/Documents/corpus_files/Function-Words/most-frequent-french-words-of-the-written-language-19th-20th-century.txt"

# Run the function
process_single_file(input_file, output_file, frequent_words_file)

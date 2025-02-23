# Load necessary libraries
library(stringr)

# Define a function to generate adapted and random pairs
generate_pair_34pairs <- function(book_folder, movie_folder, output_file) {
  # List all files in the folders
  book_files <- list.files(book_folder, full.names = TRUE)
  movie_files <- list.files(movie_folder, full.names = TRUE)
  
  # Extract base file names (without paths or extensions)
  book_names <- str_remove(basename(book_files), "\\.[a-zA-Z0-9]+$")
  movie_names <- str_remove(basename(movie_files), "\\.[a-zA-Z0-9]+$")
  
  # Initialize adapted and random pair lists
  adapted_pairs <- list()
  random_pairs <- list()
  
  # Create adapted pairs
  for (i in seq_along(book_names)) {
    if (book_names[i] %in% movie_names) {
      # Find the corresponding movie file
      matching_movie_index <- which(movie_names == book_names[i])
      if (length(matching_movie_index) > 0) {
        adapted_pairs[[length(adapted_pairs) + 1]] <- list(
          book = book_files[i],
          movie = movie_files[matching_movie_index]
        )
      } else {
        print(paste("No matching movie found for book:", book_names[i]))
      }
    } else {
      print(paste("Book has no matching movie:", book_names[i]))
    }
  }
  
  # Create random pairs
  for (i in seq_along(movie_files)) {
    # Find the corresponding book (if it exists)
    corresponding_book_index <- which(book_names == movie_names[i])
    if (length(corresponding_book_index) > 0) {
      corresponding_book <- book_files[corresponding_book_index]
    } else {
      corresponding_book <- NULL
    }
    
    # Get all books except the corresponding one
    random_books <- setdiff(book_files, corresponding_book)
    
    # Ensure there are books to sample from
    if (length(random_books) > 0) {
      random_book <- sample(random_books, 1)
      random_pairs[[length(random_pairs) + 1]] <- list(
        book = random_book,
        movie = movie_files[i]
      )
    } else {
      print(paste("No random book available for movie:", movie_names[i]))
    }
  }
  
  # Debugging: Print the lengths of the pair lists
  print(paste("Number of adapted pairs:", length(adapted_pairs)))
  print(paste("Number of random pairs:", length(random_pairs)))
  
  # Save pairs to an output file
  saveRDS(list(adapted_pairs = adapted_pairs, random_pairs = random_pairs), output_file)
  
  print(paste("Pairs generated and saved to:", output_file))
}



generate_pairs_1122_pairs <- function(book_folder, movie_folder, output_file) {
  # List all files in the folders
  book_files <- list.files(book_folder, full.names = TRUE)
  movie_files <- list.files(movie_folder, full.names = TRUE)
  
  # Extract and standardize base file names
  book_names <- tolower(str_remove_all(basename(book_files), "\\.[a-zA-Z0-9]+$"))
  movie_names <- tolower(str_remove_all(basename(movie_files), "\\.[a-zA-Z0-9]+$"))
  
  # Initialize adapted and random pair lists
  adapted_pairs <- list()
  random_pairs <- list()
  
  # Create adapted pairs
  for (i in seq_along(book_names)) {
    if (book_names[i] %in% movie_names) {
      matching_movie_index <- which(movie_names == book_names[i])
      if (length(matching_movie_index) > 0) {
        adapted_pairs[[length(adapted_pairs) + 1]] <- list(
          book = book_files[i],
          movie = movie_files[matching_movie_index]
        )
      }
    }
  }
  
  # Create random pairs
  for (i in seq_along(movie_files)) {
    corresponding_book_index <- which(book_names == movie_names[i])
    corresponding_book <- ifelse(length(corresponding_book_index) > 0, book_files[corresponding_book_index], NULL)
    
    # Get all books except the corresponding one
    random_books <- setdiff(book_files, corresponding_book)
    
    # Pair the movie with every random book
    for (random_book in random_books) {
      random_pairs[[length(random_pairs) + 1]] <- list(
        book = random_book,
        movie = movie_files[i]
      )
    }
  }
  
  # Save pairs to an output file
  saveRDS(list(adapted_pairs = adapted_pairs, random_pairs = random_pairs), output_file)
  print(paste("Number of adapted pairs:", length(adapted_pairs)))
  print(paste("Number of random pairs:", length(random_pairs)))
  print(paste("Pairs generated and saved to:", output_file))
}



# Example usage
book_folder <- "C:/Users/12145/OneDrive/Desktop/year_4/Mathematics_Project/dataset/normalized&standarized_books_dialogue_fw"  # Replace with the path to your book folder
movie_folder <- "C:/Users/12145/OneDrive/Desktop/year_4/Mathematics_Project/dataset/normalized&standarized_movie_dialogue_fw"  # Replace with the path to your movie folder
output_file <- "C:/Users/12145/OneDrive/Desktop/year_4/Mathematics_Project/dataset/normalized&standarized_1122pairs_dialogue_fw.rds"  # Replace with the desired output file path

# Generate pairs

#generate_pair_34pairs(book_folder, movie_folder, output_file)

generate_pairs_1122_pairs(book_folder, movie_folder, output_file)


# Load the .rds file
pairs <- readRDS("C:/Users/12145/OneDrive/Desktop/year_4/Mathematics_Project/dataset/normalized_pairs_dialogue_fw.rds")

# Access the adapted and random pairs
adapted_pairs <- pairs$adapted_pairs
random_pairs <- pairs$random_pairs

# Print the first adapted pair
print(adapted_pairs[[2]])

# Print the first random pair
print(random_pairs[[3]])

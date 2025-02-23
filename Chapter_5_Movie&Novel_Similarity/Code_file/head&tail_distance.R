# Function to load function word vectors with filenames
load_vectors_with_names <- function(folder) {
  file_list <- list.files(folder, full.names = TRUE)  # Get full file paths
  file_names <- list.files(folder)  # Get only the filenames (for labeling)
  
  # Read each file and store as a dataframe row
  vectors <- do.call(rbind, lapply(file_list, function(file) {
    as.numeric(unlist(strsplit(readLines(file), ",")))  # Convert text to numeric vector
  }))
  
  # Convert to dataframe and add filenames as a column
  vectors_df <- as.data.frame(vectors)
  vectors_df$FileName <- file_names  # Add filenames
  
  return(vectors_df)
}

# Load function word vectors from books and movies
book_vectors <- load_vectors_with_names("C:/Users/12145/OneDrive/Desktop/year_4/Mathematics_Project/dataset/normalized&standarized_books_dialogue_fw")
movie_vectors <- load_vectors_with_names("C:/Users/12145/OneDrive/Desktop/year_4/Mathematics_Project/dataset/normalized&standarized_movie_dialogue_fw")

# Merge book and movie function word vectors based on filename
adapted_pairs <- merge(book_vectors, movie_vectors, by = "FileName", suffixes = c("_book", "_movie"))

# Create a new column for labels
adapted_pairs$Label <- "Adapted"

set.seed(123)  # For reproducibility
random_pairs <- data.frame(
  FileName = paste0("", movie_vectors$FileName),  # Keep movie names but mark as random
  movie_vectors[, -ncol(movie_vectors)],  # Keep function word data
  book_vectors[sample(nrow(book_vectors)), -ncol(book_vectors)]  # Shuffle book vectors
)

# Add a "Random" label
random_pairs$Label <- "Random"

colnames(random_pairs) <- colnames(adapted_pairs)

# Combine Adapted and Random pairs into one dataframe
novel_movie_pairs <- rbind(adapted_pairs, random_pairs)

# Use filenames as labels for visualization
distance_data <- data.frame(
  Distance = c(adapted_distances, random_distances),
  Pair_Type = novel_movie_pairs$Label,  # Use the "Adapted" or "Random" label
  Pair_Name = novel_movie_pairs$FileName  # Use filenames as pair names
)

# Plot with filenames labeled for extreme values
ggplot(distance_data, aes(x = Pair_Type, y = Distance, label = Pair_Name)) +
  geom_jitter(aes(color = Pair_Type), width = 0.2, size = 2) +
  geom_text(data = distance_data %>% arrange(desc(Distance)) %>% head(20), aes(label = Pair_Name), vjust = -1, color = "red", size = 2) +
  labs(title = "Function Word Distance Across Novel-Movie Pairs", y = "Distance", x = "Pair Type") +
  theme_minimal()


# Sort distances and find the top 5 largest and smallest
top_5_adapted <- distance_data %>%
  filter(Pair_Type == "Adapted") %>%
  arrange(desc(Distance)) %>%
  head(5)

top_5_random <- distance_data %>%
  filter(Pair_Type == "Random") %>%
  arrange(desc(Distance)) %>%
  head(5)

bottom_5_adapted <- distance_data %>%
  filter(Pair_Type == "Adapted") %>%
  arrange(Distance) %>%
  head(5)

bottom_5_random <- distance_data %>%
  filter(Pair_Type == "Random") %>%
  arrange(Distance) %>%
  head(5)

# Print results
print("Top 5 highest adapted distances:")
print(top_5_adapted)

print("Top 5 highest random distances:")
print(top_5_random)

print("Bottom 5 lowest adapted distances:")
print(bottom_5_adapted)

print("Bottom 5 lowest random distances:")
print(bottom_5_random)



### To see which specific function words contribute to the m# Sort and get the most similar (smallest distance) and most different (largest distance) pairs
most_similar_pairs <- distance_data %>% arrange(Distance) %>% head(5)  # Smallest distances
most_different_pairs <- distance_data %>% arrange(desc(Distance)) %>% head(5)  # Largest distances

# Print results
print("Most similar novel-movie pairs:")
print(most_similar_pairs)

print("Most different novel-movie pairs:")
print(most_different_pairs)

# Extract function word vectors for most similar and most different pairs
similar_books <- book_vectors[book_vectors$FileName %in% most_similar_pairs$Pair_Name, ]
similar_movies <- movie_vectors[movie_vectors$FileName %in% most_similar_pairs$Pair_Name, ]
different_books <- book_vectors[book_vectors$FileName %in% most_different_pairs$Pair_Name, ]
different_movies <- movie_vectors[movie_vectors$FileName %in% most_different_pairs$Pair_Name, ]
# Ensure each dataset has filenames
similar_books$FileName <- rownames(similar_books)
similar_movies$FileName <- rownames(similar_movies)
different_books$FileName <- rownames(different_books)
different_movies$FileName <- rownames(different_movies)
# Combine book and movie function word counts for comparison
compare_data <- rbind(similar_books, similar_movies)
                      #, different_books, different_movies)
# Move FileName column to the first position
compare_data <- compare_data[, c(ncol(compare_data), 1:(ncol(compare_data)-1))]  
compare_data <- compare_data[, -which(names(compare_data) == "V71")]  # Remove non-function words
compare_data_long <- melt(compare_data, id.vars = "FileName")  # No more missing column error
ggplot(compare_data_long, aes(x = variable, y = FileName, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +  # Change colors for better visibility
  labs(title = "Function Word Usage Heatmap", x = "Function Word", y = "Book-Movie Pair") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),  # Increase text size for x-axis
    axis.text.y = element_text(size = 10),  # Increase text size for y-axis
    plot.title = element_text(hjust = 0.5, size = 14)  # Center and enlarge title
  )



pairs <- readRDS("C:/Users/12145/OneDrive/Desktop/year_4/Mathematics_Project/dataset/normalized&standarized_34pairs_dialogue_fw.rds")
adapted_pairs <- pairs$adapted_pairs  # Extract the adapted pairs
# Find the pair where the book path contains "to_kill_a_mockingbird"
tkam_pair <- adapted_pairs[sapply(adapted_pairs, function(x) grepl("to_kill_a_mockingbird", x$book))]

# Find the pair for "To Kill a Mockingbird"
tkam_pair <- adapted_pairs[sapply(adapted_pairs, function(x) grepl("to_kill_a_mockingbird", x$book))]

# Find the pair for "To Kill a Mockingbird"
tkam_pair <- adapted_pairs[sapply(adapted_pairs, function(x) grepl("to_kill_a_mockingbird", x$book))]

# Check if we found a valid pair
if (length(tkam_pair) > 0) {
  
  # Extract file paths
  book_path <- tkam_pair[[1]]$book
  movie_path <- tkam_pair[[1]]$movie
  
  # Print file paths to verify
  print(paste("Book path:", book_path))
  print(paste("Movie path:", movie_path))
  
  # Read text files instead of RDS
  book_text <- readLines(book_path, warn = FALSE)
  movie_text <- readLines(movie_path, warn = FALSE)
  
  # Print first few lines to check if correctly read
  print(head(book_text))
  print(head(movie_text))
  
} else {
  print("To Kill a Mockingbird not found in adapted_pairs!")
}

# Compute Euclidean Distance
compute_euclidean_distance <- function(vec1, vec2) {
  sqrt(sum((vec1 - vec2)^2))
}

# Convert extracted text into numeric vectors
book_vector <- as.numeric(strsplit(book_text, ",")[[1]])
movie_vector <- as.numeric(strsplit(movie_text, ",")[[1]])

# Ensure both vectors are of the same length
if (length(book_vector) == length(movie_vector)) {
  final_distance <- compute_euclidean_distance(book_vector, movie_vector)
  print(paste("Final Euclidean Distance:", final_distance))
} else {
  print("Error: The book and movie vectors have different lengths!")
}


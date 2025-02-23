
# Define folder paths
book_folder <- "C:/Users/12145/OneDrive/Desktop/year_4/Mathematics_Project/dataset/normalized_books_dialogue_fw"
movie_folder <- "C:/Users/12145/OneDrive/Desktop/year_4/Mathematics_Project/dataset/normalized_movie_dialogue_fw"

# Function to load function word vectors from text files
load_vectors <- function(folder) {
  file_list <- list.files(folder, full.names = TRUE)
  
  # Read each file and store as a row in a dataframe
  vectors <- do.call(rbind, lapply(file_list, function(file) {
    as.numeric(unlist(strsplit(readLines(file), ",")))  # Convert text to numeric vector
  }))
  
  return(as.data.frame(vectors))
}

# Load book and movie function word vectors
book_vectors <- load_vectors(book_folder)
movie_vectors <- load_vectors(movie_folder)

# Ensure they have the same order (same book-movie pairs)
colnames(book_vectors) <- colnames(movie_vectors) <- paste0("word_", 1:71)

# Combine into a single dataset for clustering
combined_word_vectors <- rbind(book_vectors, movie_vectors)


# Compute Euclidean Distance for all pairs
euclidean_dist_matrix <- as.matrix(dist(combined_word_vectors, method = "euclidean"))

# Compute Cosine Distance
#install.packages('lsa')
library(lsa)
cosine_dist_matrix <- 1 - cosine(combined_word_vectors)  # Convert similarity to distance


set.seed(123)  # Ensure reproducibility
# Choose the distance matrix (Euclidean or Cosine)
distance_matrix <- euclidean_dist_matrix  # or cosine_dist_matrix

# Apply K-Means with k=2 (since we expect two groups)
kmeans_result <- kmeans(distance_matrix, centers = 2, nstart = 25)

adapted_pairs_new <- adapted_pairs
random_pairs_new <- sample(random_pairs, size = length(adapted_pairs_new), replace = FALSE)
adapted_pairs_new$Label <- "Adapted"
random_pairs_new$Label <- "Random"
# random_pairs_new <- random_pairs
# random_pairs_new$Label <- "Random"

novel_movie_pairs <- rbind(adapted_pairs_new, random_pairs_new)
# Add cluster labels to dataset
novel_movie_pairs$Cluster <- as.factor(kmeans_result$cluster)


# Print results
print(table(novel_movie_pairs$Cluster))  # Check distribution of clusters
#Almost all (66 pairs) were grouped into Cluster 2. Only 2 pairs were assigned to Cluster 1.

# Load necessary library
library(ggplot2)
library(factoextra)  # For visualization

pairs <- readRDS("C:/Users/12145/OneDrive/Desktop/year_4/Mathematics_Project/dataset/normalized&standarized_34pairs_dialogue_fw.rds")

adapted_pairs <- pairs$adapted_pairs
random_pairs <- pairs$random_pairs
# Function to read function word vectors from file
read_function_words <- function(file_path) {
  as.numeric(unlist(strsplit(readLines(file_path), ",")))  # Convert to numeric vector
}

# Extract function word vectors for adapted pairs (concatenating instead of averaging)
adapted_vectors <- lapply(adapted_pairs, function(pair) {
  book_vector <- read_function_words(pair$book)  # Read book function words
  movie_vector <- read_function_words(pair$movie)  # Read movie function words
  return(c(book_vector, movie_vector))  # Concatenation of vectors (140D)
})

# Extract function word vectors for random pairs
random_vectors <- lapply(random_pairs, function(pair) {
  book_vector <- read_function_words(pair$book)
  movie_vector <- read_function_words(pair$movie)
  return(c(book_vector, movie_vector))  # Concatenation of vectors (140D)
})

# Convert to a matrix
adapted_matrix <- do.call(rbind, adapted_vectors)
random_matrix <- do.call(rbind, random_vectors)

# Combine all pairs into one dataset (each row = 140D vector)
clustering_data <- rbind(adapted_matrix, random_matrix)


set.seed(123)  # Ensure reproducibility
kmeans_result <- kmeans(clustering_data, centers = 2, nstart = 25)

# Add cluster labels to dataset
cluster_labels <- as.factor(kmeans_result$cluster)

# Create labels (1 = Adapted, 2 = Random)
labels <- c(rep("Adapted", length(adapted_pairs)), rep("Random", length(random_pairs)))

# Compare clusters to actual labels
table(labels, cluster_labels)

# Calculate clustering accuracy
sum(cluster_labels == labels) / length(labels)

library(ggplot2)

# Perform PCA for visualization (not clustering)
pca_result <- prcomp(clustering_data, scale. = TRUE)
pca_df <- data.frame(pca_result$x[, 1:2])  # Use first two principal components

# Add cluster information
pca_df$Cluster <- as.factor(kmeans_result$cluster)
pca_df$Pair_Type <- as.factor(labels)  # Adapted or Random

# Plot PCA results
ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster, shape = Pair_Type)) +
  geom_point(size = 3) +
  labs(title = "K-Means Clustering of Novel-Movie Pairs",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()

# 
# pca_result <- prcomp(clustering_data, scale. = TRUE)
# pca_df <- data.frame(pca_result$x[, 1:2], Cluster = as.factor(kmeans_result$cluster))
# 
# ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
#   geom_point() +
#   labs(title = "PCA Visualization of Novel-Movie Pairs")
# 
# # 

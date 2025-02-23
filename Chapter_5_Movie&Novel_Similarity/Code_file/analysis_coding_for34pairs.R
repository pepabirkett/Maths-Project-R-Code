pairs <- readRDS("C:/Users/12145/OneDrive/Desktop/year_4/Mathematics_Project/dataset/normalized&standarized_34pairs_dialogue_fw.rds")
adapted_pairs <- pairs$adapted_pairs
random_pairs <- pairs$random_pairs

# Function to calculate cosine distance
cosine_distance <- function(vector1, vector2) {
  sum(vector1 * vector2) / (sqrt(sum(vector1^2)) * sqrt(sum(vector2^2)))
}

euclidean_distance <- function(vector1, vector2) {
  if (length(vector1) != length(vector2)) {
    stop("Vectors must have the same length")
  }
  sqrt(sum((vector1 - vector2)^2))
}

# Cosine Distance
# Calculate distances for adapted pairs
# adapted_distances <- sapply(adapted_pairs, function(pair) {
#   book_vector <- as.numeric(unlist(strsplit(readLines(pair$book), ",")))
#   movie_vector <- as.numeric(unlist(strsplit(readLines(pair$movie), ",")))
#   1 - sum(book_vector * movie_vector) / 
#     (sqrt(sum(book_vector^2)) * sqrt(sum(movie_vector^2)))  # Cosine Distance
#   
#   
# })


# Cosine distance
# # Calculate distances for random pairs
# random_distances <- sapply(random_pairs, function(pair) {
#   book_vector <- as.numeric(unlist(strsplit(readLines(pair$book), ",")))
#   movie_vector <- as.numeric(unlist(strsplit(readLines(pair$movie), ",")))
#   1 - sum(book_vector * movie_vector) / 
#     (sqrt(sum(book_vector^2)) * sqrt(sum(movie_vector^2)))  # Cosine Distance
#   #1 - sqrt(sum((book_vector - movie_vector)^2))
# })


#Euclidean distances
adapted_distances <- sapply(adapted_pairs, function(pair) {
  book_vector <- as.numeric(unlist(strsplit(readLines(pair$book), ",")))
  movie_vector <- as.numeric(unlist(strsplit(readLines(pair$movie), ",")))
  
  # Ensure both vectors are of the same length
  if (length(book_vector) == length(movie_vector)) {
    return(sqrt(sum((book_vector - movie_vector)^2)))  # Euclidean Distance
  } else {
    return(NA)  # Return NA if lengths do not match
  }
})

# Calculate distances for random pairs
random_distances <- sapply(random_pairs, function(pair) {
  book_vector <- as.numeric(unlist(strsplit(readLines(pair$book), ",")))
  movie_vector <- as.numeric(unlist(strsplit(readLines(pair$movie), ",")))
  
  # Ensure both vectors are of the same length
  if (length(book_vector) == length(movie_vector)) {
    return(sqrt(sum((book_vector - movie_vector)^2)))  # Euclidean Distance
  } else {
    return(NA)  # Return NA if lengths do not match
  }
})



print(adapted_distances)
print(random_distances)

library(ggplot2)

# Combine the data
distance_data <- data.frame(
  Distance = c(adapted_distances, random_distances),
  Pair_Type = c(rep("Adapted Pair", length(adapted_distances)), rep("Random Pair", length(random_distances)))
)

# Create a box plot
ggplot(distance_data, aes(x = Pair_Type, y = Distance, fill = Pair_Type)) +
  geom_boxplot() +
  labs(title = "Comparison of Distances", x = "Pair Type", y = "Distance") +
  theme_minimal()

# Create a Histogram
ggplot(distance_data, aes(x = Distance, fill = Pair_Type)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
  labs(title = "Distribution of Distances", x = "Distance", y = "Frequency") +
  theme_minimal()

# compare the 95% confidence intervals for both groups
#install.packages('DescTools')
library(DescTools)

adapted_ci <- MeanCI(adapted_distances, conf.level = 0.95)
random_ci <- MeanCI(random_distances, conf.level = 0.95)
print(adapted_ci)
print(random_ci)
# visualize the confidence intervals
library(ggplot2)
# Create a data frame for visualization
ci_data <- data.frame(
  Pair_Type = c("Adapted", "Random"),
  Mean = c(0.4177785 , 0.4701165 ),
  Lower_CI = c(0.3570819  , 0.4105136  ),
  Upper_CI = c(0.4784751  , 0.5297194 )
)
# Plot the confidence intervals
ggplot(ci_data, aes(x = Pair_Type, y = Mean, color = Pair_Type)) +
  geom_point(size = 3) +  # Plot means
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, size = 1) +  # Add error bars
  labs(title = "Mean Distances with 95% Confidence Intervals",
       x = "Pair Type",
       y = "Mean Distance") +
  theme_minimal() +
  theme(legend.position = "none")


# Randomly sample 34 random pairs from the larger group multiple times and compare their distributions to the adapted pairs
set.seed(123)  # For reproducibility

# Bootstrap sampling
bootstrap_differences <- replicate(1000, {
  random_sample <- sample(random_distances, 34, replace = TRUE)
  mean(random_sample) - mean(adapted_distances)
})

# Plot bootstrap distribution
hist(bootstrap_differences, main = "Bootstrap Differences", xlab = "Mean Difference (Random - Adapted)")
abline(v = 0, col = "red", lwd = 2)


# T-test (34 adapted pairs and 1122 random pairs): to see whether the difference in distances between adapted and random pairs is statistically significant.
t.test(adapted_distances, random_distances)


# T-test (34 adapted pairs and 34 random pairs)
set.seed(123)  # Ensure reproducibility
# Randomly sample 34 random distances
random_sample_distances <- sample(random_distances, size = length(adapted_distances), replace = FALSE)
# Perform t-test on resampled data
t.test(adapted_distances, random_sample_distances)


library(stringr)
library(ggplot2)


function_words <- c(
  "the", "and", "of", "a", "i", "to", "in", "he", "my", "that",
  "with", "is", "his", "was", "all", "it", "on", "for", "but",
  "from", "she", "her", "as", "they", "me", "not", "by", "be", 
  "this", "so", "no", "you", "or", "when", "at", "are", "we", 
  "what", "then", "little", "now", "their", "him", "have", "like", 
  "had", "which", "were", "man", "if", "one", "thy", "did", "who", 
  "an", "more", "nor", "how", "there", "where", "see", "night", 
  "away", "joy", "sweet", "day", "never", "still", "can", "our", 
  "will", "could", "up", "has", "would", "thee", "till", "old", 
  "love", "oh", "heart", "poor", "upon", "o", "came", "said", "boy", 
  "every", "your", "thou", "into", "heard"
)


clean_text <- function(text) {
  text <- tolower(text)
  text <- str_replace_all(text, "[^a-z\\s]", " ")  # Remove punctuation
  text <- str_squish(text)  # Remove extra spaces
  return(text)
}


root_dir <- "RomanticCorpus"


data_list <- list()
poet_labels <- c()
poem_names <- c()


poet_folders <- list.dirs(root_dir, recursive = FALSE)
for (poet in poet_folders) {
  poem_files <- list.files(poet, full.names = TRUE, pattern = "*.txt")
  poet_name <- basename(poet)
  
  for (poem in poem_files) {
    text <- paste(readLines(poem, warn = FALSE), collapse = " ")
    text <- clean_text(text)
    words <- unlist(strsplit(text, "\\s+"))
    
    
    function_counts <- table(factor(words, levels = function_words))
    
    
    data_list[[poem]] <- as.numeric(function_counts)
    poet_labels <- c(poet_labels, poet_name)
    poem_names <- c(poem_names, basename(poem))
  }
}


word_matrix <- do.call(rbind, data_list)
colnames(word_matrix) <- function_words
rownames(word_matrix) <- poem_names  

# normalize rows (each poem sums to 1)
word_matrix <- word_matrix / rowSums(word_matrix)

# standardize columns (subtract mean, divide by SD)
word_matrix <- scale(word_matrix, center = TRUE, scale = TRUE)

head(word_matrix)


library(stats)


poem_names <- rownames(word_matrix)  


labels <- factor(ifelse(grepl("^(W|WB|C)", poem_names), "Early", "Late"))  


euclidean_distance <- function(a, b) {
  sqrt(sum((a - b)^2))
}


loo_results <- data.frame(Poem = poem_names, True_Label = labels, Predicted_Label = NA,
                          Distance_Early = NA, Distance_Late = NA, Correct = NA, stringsAsFactors = FALSE)

# Leave-One-Out Cross Validation
for (i in 1:nrow(word_matrix)) {

  train_data <- word_matrix[-i, ]
  train_labels <- labels[-i]
  
  test_data <- word_matrix[i, , drop = FALSE]  # Left-out poem
  test_label <- labels[i]  # True label
  
  # normalize 
  train_means <- colMeans(train_data)
  train_sds <- apply(train_data, 2, sd)
  
  train_data <- scale(train_data, center = train_means, scale = train_sds)
  
 
  test_data <- sweep(test_data, 2, train_means, "-")
  test_data <- sweep(test_data, 2, train_sds, "/")
  
  
  centroid_early <- colMeans(train_data[train_labels == "Early", , drop = FALSE])
  centroid_late <- colMeans(train_data[train_labels == "Late", , drop = FALSE])
  
  
  dist_to_early <- euclidean_distance(test_data, centroid_early)
  dist_to_late <- euclidean_distance(test_data, centroid_late)
  
  
  predicted_label <- ifelse(dist_to_early < dist_to_late, "Early", "Late")
  
  
  loo_results$Predicted_Label[i] <- predicted_label
  loo_results$Distance_Early[i] <- dist_to_early
  loo_results$Distance_Late[i] <- dist_to_late
  loo_results$Correct[i] <- (predicted_label == test_label)
}


print(loo_results)


accuracy <- mean(loo_results$Correct)
print(paste("LOOCV Accuracy:", accuracy))


library(dplyr)


loo_results$Poet <- gsub("[0-9]+\\.txt", "", loo_results$Poem)  


poet_summary <- loo_results %>%
  group_by(Poet, Predicted_Label) %>%
  summarise(Count = n()) %>%
  tidyr::spread(Predicted_Label, Count, fill = 0)  


colnames(poet_summary) <- c("Poet", "Classified_as_Early", "Classified_as_Late")


print(poet_summary)



contingency_table <- table(loo_results$Poet, loo_results$Predicted_Label)


print(contingency_table)


chi_test <- chisq.test(contingency_table)


print(chi_test)


fisher_test <- fisher.test(contingency_table)


print(fisher_test)




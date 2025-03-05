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

# Function to clean text
clean_text <- function(text) {
  text <- tolower(text)
  text <- str_replace_all(text, "’", "'")  
  text <- str_replace_all(text, "\\d+", " ")  
  text <- str_replace_all(text, "'s\\b", "")  
  text <- str_replace_all(text, "[^a-z\\s]", " ")  
  text <- str_squish(text)  
  return(text)
}


root_dir <- "RomanticCorpus"

# Initialize lists
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

# normalise rows and cols
word_matrix <- word_matrix / rowSums(word_matrix)


word_matrix <- scale(word_matrix, center = TRUE, scale = TRUE)


distance_matrix <- dist(word_matrix, method = "euclidean")  
mds_result <- cmdscale(distance_matrix, k = 2)


df_mds <- data.frame(
  X = mds_result[, 1],
  Y = mds_result[, 2],
  Poet = factor(poet_labels),  
  Poem = poem_names
)

ggplot(df_mds, aes(x = X, y = Y, color = Poet, label = Poem)) +
  geom_point(size = 3) +
  geom_text(aes(label = Poem), vjust = 1.5, size = 3, show.legend = FALSE) +
  labs(title = "MDS Plot of Poem Word Frequencies", x = "MDS Dimension 1", y = "MDS Dimension 2") +
  theme_minimal()


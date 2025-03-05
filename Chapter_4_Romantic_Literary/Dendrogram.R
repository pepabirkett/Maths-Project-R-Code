#install.packages("dendextend") 

library(dendextend)
library(stats)

distance_matrix <- dist(word_matrix, method = "euclidean")

hclust_result <- hclust(distance_matrix, method = "ward.D")
plot(hclust_result)

dend <- as.dendrogram(hclust_result, hang = 0.1)


labels <- labels(dend)


get_color <- function(label) {
  if (grepl("^TSE|^WCW|^EP", label)) {
    return("plum")
  } else if (grepl("^WB", label)) {
    return("indianred")
  } else if (grepl("^W", label)) {
    return("hotpink")
  } else if (grepl("^B", label)) {
    return("deepskyblue")
  } else if (grepl("^C", label)) {
    return("seagreen")
  } else if (grepl("^K", label)) {
    return("lightpink")
  } else if (grepl("^S", label)) {
    return("coral")
  } else {
    return("black")  
  }
}


label_colors <- sapply(labels, get_color)
dend <- color_labels(dend, col = label_colors)


plot(dend, main = "Dendrogram of Poem Matrix", xlab = "Texts", ylab = "Closeness")


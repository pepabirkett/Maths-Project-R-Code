library(RColorBrewer)

M <- loadCorpus("C:/Users/cobet/Documents/corpus_files/SupervisedWCollab/", "frequentwords100")



names <- M$authornames; books <- M$booknames; M <- M$features
names
books
# temp stores normalised texts altogether in one matrix
temp <- NULL
book_titles <- NULL # store book titles for labelling
for (i in 1:length(M)) {
  for (j in 1:nrow(M[[i]])) {
    M[[i]][j,] <-  M[[i]][j,]/sum( M[[i]][j,] )
  }
  temp <- rbind(temp, M[[i]])
  book_titles <- c(book_titles, books[[i]]) # Store book titles
}

for (i in 1:ncol(temp)) {
  temp[,i] <- (temp[,i] - mean(temp[,i]))/sd(temp[,i])
}

inds <- NULL
for (i in 1:length(M)) {
  inds <- c(inds, rep(i, nrow(M[[i]])))
}

d <- dist(temp)
pts <- cmdscale(d)

names <- c("Dumas", "Collab", "Hugo", "Maquet", "Stendhal", "Le Vicomte Chapters")
colours <- c("red", "blue", "green", "purple", "orange", "black") # Assign colours to authors
author_colours <- colours[inds] # Assign colours based on index of the author

# MDS plot details
par(mar=c(5,4,4,10), xpd=TRUE) # extra margin RHS

plot(pts, col=author_colours, pch=19, main="MDS Plot of Books by Author")
text(pts[,1],pts[,2],label=book_titles,cex=0.8, pos = 4)

legend("topright", inset=c(-0.3, 0), legend=names, col=colours, pch=19, title="Authors", bty="n")

# Using ggplot to make it prettier
mds_df <- data.frame(
  Dim1 = pts[,1],
  Dim2 = pts[,2],
  Author = factor(inds, labels = names),
  Book = book_titles
)

ggplot(mds_df, aes(x = Dim1, y = Dim2, colour = Author)) +
  geom_point(size = 3, alpha = 0.7) + 
  geom_text(aes(label = Book), vjust = -1, size = 3) +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "MDS Plot of Books by Author", x = "MDS Dimension 1", y = "MDS DImension 2") +
  theme_minimal() +
  theme(legend.title = element_text(face = "bold"))


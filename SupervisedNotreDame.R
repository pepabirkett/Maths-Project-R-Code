M <- loadCorpus("C:/Users/cobet/Documents/corpus_files/Texts/Functionwords/", "frequentwords100")

M$authornames

# Remove unknown chapters from corpus and Dumas texts (not very close on MDS and he was known to have numerous ghost writers)
M <- within(M, {
  features <- features[-c(5)]
  booknames <- booknames[-c(5)]
  authornames <- authornames[-c(5)]
})

M$authornames

traindata <- M$features
testdata <- NULL
testlabels <- NULL

for (i in 1:length(traindata)) {
  testind <- sample(1:nrow(traindata[[i]]), 1) # randomly select a book by this
                                               # author by choosing a row (=book)
  
  testdata <- rbind(testdata, traindata[[i]][testind, ]) # add this book to the
                                                         # test set
  testlabels <- c(testlabels, i)                         # assign corresponding author index
  
  traindata[[i]] <- traindata[[i]][-testind,,drop=FALSE] # discard book from training set
}

# preds <- discriminantCorpus(traindata, testdata)
# sum(preds == testlabels)/length(testlabels)

predsKNN <- KNNCorpus(traindata, testdata)
sum(predsKNN==testlabels)/length(testlabels)


# predictions <- NULL
KNNpredictions <- NULL
truth <- NULL
features <- M$features
for (i in 1:length(features)) {
  for (j in 1:nrow(features[[i]])) {
    testdata <- matrix(features[[i]][j,], nrow=1)
    traindata <- features
    traindata[[i]] <- traindata[[i]][-j,,drop=FALSE]
    
    pred <- discriminantCorpus(traindata, testdata)
    predictions <- c(predictions, pred)
    
    pred <- KNNCorpus(traindata, testdata)
    KNNpredictions <- c(KNNpredictions, pred)
    truth <- c(truth, i)
  }
}

# sum(predictions==truth)/length(truth)
sum(KNNpredictions==truth)/length(truth)

# confusionMatrix(as.factor(predictions), as.factor(truth))

confusionMatrix(as.factor(KNNpredictions), as.factor(truth))

M1$authornames

train2 <- M1$features[-5]
test2 <- M1$features[5]
test2 <- test2[[1]]

discriminantCorpus(train2, test2)
KNNCorpus(train2, test2)

M1$booknames

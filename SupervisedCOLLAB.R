# predicting collab books

M1 <- loadCorpus("C:/Users/cobet/Documents/corpus_files/Texts/Functionwords-Collab/", "frequentwords100")

M1$authornames

train2 <- M1$features[-5]
test2 <- M1$features[5]
test2 <- test2[[1]]

discriminantCorpus(train2, test2)
KNNCorpus(train2, test2)

M1$booknames[[5]]

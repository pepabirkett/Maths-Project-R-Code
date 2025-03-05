# JUST DUMAS V MAQUET

M1 <- loadCorpus("C:/Users/cobet/Documents/corpus_files/Texts/Functionwords-LeVicomte/", "frequentwords100")

M1$authornames

train2 <- M1$features[-3]
test2 <- M1$features[3]
test2 <- test2[[1]]

discriminantCorpus(train2, test2)
KNNCorpus(train2, test2)

M1$booknames


# ALL AUTHORS

M1 <- loadCorpus("C:/Users/cobet/Documents/corpus_files/Texts/Functionwords-LeVicomte/", "frequentwords100")

M1$authornames

train2 <- M1$features[-3]
test2 <- M1$features[3]
test2 <- test2[[1]]

discriminantCorpus(train2, test2)
KNNCorpus(train2, test2)

M1$booknames

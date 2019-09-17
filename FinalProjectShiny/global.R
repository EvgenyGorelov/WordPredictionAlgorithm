library(data.table)
library(stringr)

MaxNngram <- 6
#dtNgramsSort<-rep(list((data.table(ngramInp=character(), answer=character(), freq=integer(), key="ngramInp"))), MaxNngram)
dtNgramsSort <- readRDS("dtNgramsSortF6.sav")

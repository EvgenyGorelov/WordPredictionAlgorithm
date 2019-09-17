library(quanteda)
library(readtext)
library(data.table)
setwd("~/Rt/Course10_NLP/code")

if (!file.exists("Profanity.txt")) {
ProfanityURL<-"https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
download.file(ProfanityURL, "Profanity.txt")
rm(ProfanityURL)
 }
ProfanityText<-readLines("Profanity.txt")
tknP<-tokens_tolower(tokens(ProfanityText))
#ntype(tknP) # number of words in every bad sentense
MaxProfLen<-max(ntype(tknP)) # max number of words in all bad sentenses # 6
#tknP[ntype(tknP)==6]
#ngrP6<-tokens_ngrams(tknP[ntype(tknP)==6], n = 6)
ngramsProfanityL<-lapply(1:MaxProfLen, function(x) tokens_ngrams(tknP[ntype(tknP)==x], n = x))
#if(exists("ngramsProfanity")) rm(ngramsProfanity) 
ngramsProfanity<-list()
ngramsProfanity<-unlist(lapply(1:MaxProfLen, function(x) c(ngramsProfanity, unlist(ngramsProfanityL[[x]], recursive = FALSE))), recursive = FALSE)
rm(ngramsProfanityL, tknP, MaxProfLen, ProfanityText)

#################
######### REDO:
#################

myCorpus <- corpus(readtext("../final/en_US/en_US.twitter.txt"))
myTokens <- tokens(myCorpus, remove_punct=TRUE, remove_symbols = TRUE, remove_numbers =TRUE, 
                  remove_separators = TRUE, remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE)

myTokens <- tokens_tolower(myTokens)
myTokens <- tokens_select(myTokens, names(data_int_syllables))
myTokens <- tokens_select(myTokens, pattern = ngramsProfanity, selection = 'remove')

ngr2<-tokens_ngrams(myTokens, n = 2)
ngr2 <- tokens_select(ngr2, pattern = ngramsProfanity, selection = 'remove')

dfm2<-dfm(ngr2)
rm(ngr2)

#dt2<-data.table(ngram=character(), freq=integer(), key="ngram")
dt2<-data.table(ngram=dfm2@Dimnames$features, freq=dfm2@x)
rm(dfm2)

saveRDS(dt2, "dt2twitter.sav")
rm(dt2, myTokens, myCorpus)
##################
myCorpus <- corpus(readtext("../final/en_US/en_US.blogs.txt"))
myTokens <- tokens(myCorpus, remove_punct=TRUE, remove_symbols = TRUE, remove_numbers =TRUE, 
                   remove_separators = TRUE, remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE)

myTokens <- tokens_tolower(myTokens)
myTokens <- tokens_select(myTokens, names(data_int_syllables))
myTokens <- tokens_select(myTokens, pattern = ngramsProfanity, selection = 'remove')

ngr2<-tokens_ngrams(myTokens, n = 2)
ngr2 <- tokens_select(ngr2, pattern = ngramsProfanity, selection = 'remove')

dfm2<-dfm(ngr2)
rm(ngr2)

#dt2<-data.table(ngram=character(), freq=integer(), key="ngram")
dt2<-data.table(ngram=dfm2@Dimnames$features, freq=dfm2@x)
rm(dfm2)

saveRDS(dt2, "dt2blogs.sav")
rm(dt2, myTokens, myCorpus)
##################
myCorpus <- corpus(readtext("../final/en_US/en_US.news.txt"))
myTokens <- tokens(myCorpus, remove_punct=TRUE, remove_symbols = TRUE, remove_numbers =TRUE, 
                   remove_separators = TRUE, remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE)

myTokens <- tokens_tolower(myTokens)
myTokens <- tokens_select(myTokens, names(data_int_syllables))
myTokens <- tokens_select(myTokens, pattern = ngramsProfanity, selection = 'remove')

ngr2<-tokens_ngrams(myTokens, n = 2)
ngr2 <- tokens_select(ngr2, pattern = ngramsProfanity, selection = 'remove')

dfm2<-dfm(ngr2)
rm(ngr2)

#dt2<-data.table(ngram=character(), freq=integer(), key="ngram")
dt2<-data.table(ngram=dfm2@Dimnames$features, freq=dfm2@x)
rm(dfm2)

saveRDS(dt2, "dt2news.sav")
rm(dt2, myTokens, myCorpus)
###########

# 33333333333333333333333  etc

################
myCorpus <- corpus(readtext("../final/en_US/en_US.twitter.txt"))
myTokens <- tokens(myCorpus, remove_punct=TRUE, remove_symbols = TRUE, remove_numbers =TRUE, 
                   remove_separators = TRUE, remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE)

myTokens <- tokens_tolower(myTokens)
myTokens <- tokens_select(myTokens, names(data_int_syllables))
myTokens <- tokens_select(myTokens, pattern = ngramsProfanity, selection = 'remove')

ngr2<-tokens_ngrams(myTokens, n = 5)
ngr2 <- tokens_select(ngr2, pattern = ngramsProfanity, selection = 'remove')

dfm2<-dfm(ngr2)
rm(ngr2)

#dt2<-data.table(ngram=character(), freq=integer(), key="ngram")
dt2<-data.table(ngram=dfm2@Dimnames$features, freq=dfm2@x)
rm(dfm2)

saveRDS(dt2, "dt5twitter.sav")
rm(dt2, myTokens, myCorpus)

##################
####  55555555555555555
##########################


myCorpus <- corpus(readtext("../final/en_US/en_US.blogs.txt"))
myTokens <- tokens(myCorpus, remove_punct=TRUE, remove_symbols = TRUE, remove_numbers =TRUE, 
                   remove_separators = TRUE, remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE)

myTokens <- tokens_tolower(myTokens)
myTokens <- tokens_select(myTokens, names(data_int_syllables))
myTokens <- tokens_select(myTokens, pattern = ngramsProfanity, selection = 'remove')

ngr2<-tokens_ngrams(myTokens, n = 5)

ngr2 <- tokens_select(ngr2, pattern = ngramsProfanity, selection = 'remove')
dfm2<-dfm(ngr2)
rm(ngr2)
#dt2<-data.table(ngram=character(), freq=integer(), key="ngram")
dt2<-data.table(ngram=dfm2@Dimnames$features, freq=dfm2@x)
rm(dfm2)

saveRDS(dt2, "dt5blogs.sav")
rm(dt2, myTokens, myCorpus)
##################
myCorpus <- corpus(readtext("../final/en_US/en_US.news.txt"))
myTokens <- tokens(myCorpus, remove_punct=TRUE, remove_symbols = TRUE, remove_numbers =TRUE, 
                   remove_separators = TRUE, remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE)

myTokens <- tokens_tolower(myTokens)
myTokens <- tokens_select(myTokens, names(data_int_syllables))
myTokens <- tokens_select(myTokens, pattern = ngramsProfanity, selection = 'remove')

ngr2<-tokens_ngrams(myTokens, n = 5)
ngr2 <- tokens_select(ngr2, pattern = ngramsProfanity, selection = 'remove')

dfm2<-dfm(ngr2)
rm(ngr2)

#dt2<-data.table(ngram=character(), freq=integer(), key="ngram")
dt2<-data.table(ngram=dfm2@Dimnames$features, freq=dfm2@x)
rm(dfm2)

saveRDS(dt2, "dt5news.sav")
rm(dt2, myTokens, myCorpus)
###########
###########
###########
# collect files:

library(data.table)
setwd("~/Rt/Course10_NLP/code")


dtAdd <- readRDS("dt2blogs.sav")
dtNgrams <- readRDS("dt2twitter.sav")
dtNgrams <- rbind(dtNgrams, dtAdd)
rm(dtAdd)
#gc()
dtNgrams <- dtNgrams[, lapply(.SD, sum), by = ngram]
#gc()
dtAdd <- readRDS("dt2news.sav")
#gc()
dtNgrams <- rbind(dtNgrams, dtAdd)
rm(dtAdd)
#gc()
dtNgrams <- dtNgrams[, lapply(.SD, sum), by = ngram]
#gc()
saveRDS(dtNgrams, "dt2gramsFilter.sav")

####################
## 5-grams:
####################

library(dplyr)

dtNgrams <- readRDS("dt5blogs.sav")
dtNgrams<-dtNgrams %>% filter(freq > 1)
gc()
dtAdd <- readRDS("dt5news.sav")
dtAdd<-dtAdd %>% filter(freq > 1)
dtNgrams <- rbind(dtNgrams, dtAdd)
rm(dtAdd)
gc()
dtNgrams <- as.data.table(dtNgrams)
dtNgrams <- dtNgrams[, lapply(.SD, sum), by = ngram]
gc()
dtAdd <- readRDS("dt5twitter.sav")
dtAdd<-dtAdd %>% filter(freq > 1)
dtNgrams <- rbind(dtNgrams, dtAdd)
rm(dtAdd)
gc()
dtNgrams <- as.data.table(dtNgrams)
dtNgrams <- dtNgrams[, lapply(.SD, sum), by = ngram]

Pred4gram<-data.table(ngramInp=sub("_[^_]*$", "", dtNgrams$ngram), answer=sub("^[^_]*_[^_]*_[^_]*_[^_]*_", "", dtNgrams$ngram), freq=dtNgrams$freq)
setorder(Pred4gram, -freq)

saveRDS(Pred4gram, "Pred4FilterMin2.sav")

qSBO<-Pred4gram %>% group_by(ngramInp) %>% filter(freq == max(freq)) %>% arrange(ngramInp)

saveRDS(qSBO, "Pred4sboFilterMin2.sav")


#my_db_file <- "PredGrams.sqlite"
#my_db <- src_sqlite(my_db_file)
#copy_to(my_db, Pred4gram, temporary=FALSE)

#########
#clean from internal trash:
dtTemp<-readRDS("Pred2sboFilterMin4.sav")
Pred2gram<-data.table(ngramInp=dtTemp$ngramInp, answer=dtTemp$answer, freq=dtTemp$freq)
setorder(Pred2gram, -freq)
object.size(Pred2gram)  
saveRDS(Pred2gram,"Pred2sboFilterMin4.sav")
########
##########
# Preparing .sav for Shiny App !!!!!!!
###################


library(data.table)
setwd("~/Rt/Course10_NLP/code")
MaxNngram <- 6
dtNgramsSort<-rep(list((data.table(ngramInp=character(), answer=character(), freq=integer(), key="ngramInp"))), MaxNngram)
#dtTemp <- readRDS("Pred")

dt1<-as.data.table(readRDS("Pred1sboFilter.sav"))
dt2<-as.data.table(readRDS("Pred2sboFilterMin2.sav"))
dt3<-as.data.table(readRDS("Pred3sboFilterMin2.sav"))
dt4<-as.data.table(readRDS("Pred4sboFilterMin2.sav"))
dt5<-as.data.table(readRDS("Pred5sboFilterMin2.sav"))
dt6<-as.data.table(readRDS("Pred6sboFilterMin2.sav"))

setorder(dt1, -freq);setorder(dt2, -freq);setorder(dt3, -freq);
setorder(dt4, -freq);setorder(dt5, -freq);setorder(dt6, -freq);


dtNgramsSort[[1]]<-data.table(ngramInp=dt1$ngramInp, answer=dt1$answer)
dtNgramsSort[[2]]<-data.table(ngramInp=dt2$ngramInp, answer=dt2$answer)
dtNgramsSort[[3]]<-data.table(ngramInp=dt3$ngramInp, answer=dt3$answer)
dtNgramsSort[[4]]<-data.table(ngramInp=dt4$ngramInp, answer=dt4$answer)
dtNgramsSort[[5]]<-data.table(ngramInp=dt5$ngramInp, answer=dt5$answer)
dtNgramsSort[[6]]<-data.table(ngramInp=dt6$ngramInp, answer=dt6$answer)


saveRDS(dtNgramsSort,"dtNgramsSortF6.sav", compress=FALSE)

#con <- file("../final/en_US/en_US.twitter.txt", "r")

#con <- file("./train.txt", "r")
#lines5<-readLines(con)
#close(con)





dtNgrFunc<-function(conn, MaxNngramF, chunk) { 
lines5<-readLines(conn, chunk)
myCorpus<-corpus(lines5)
rm(lines5)
myCorpus<-corpus_reshape(myCorpus, to = 'sentences')
tknAll<-tokens(myCorpus, remove_punct=TRUE, remove_symbols = TRUE, remove_numbers =TRUE, 
               remove_separators = TRUE, remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE)
rm(myCorpus)
###########################
#tknAll<-tokens_select(tknAll, pattern = c(ngramsProfanity, stopwords('en')), selection = 'remove')
###########################
ngr<-lapply(1:MaxNngramF, function(x) tokens_select(tokens_ngrams(tknAll, n = x), pattern = ngramsProfanity, selection = 'remove'))
#ngr<-tokens_select(ngr, pattern = ngramsProfanity, selection = 'remove')
rm(tknAll)
dfM<-sapply(ngr, dfm)
rm(ngr)
dfmNgrams<-sapply(dfM, colSums)
rm(dfM)
dtNgrams<-lapply(1:MaxNngramF, function(x) rbind(dtNgrams[[x]], data.table(ngram=names(dfmNgrams[[x]]), freq=dfmNgrams[[x]])))
rm(dfmNgrams)
lapply(1:MaxNngramF, function(x) dtNgrams[[x]][, lapply(.SD, sum), by = ngram])
}

#wc -l ../final/en_US/en_US.twitter.txt 
#2 360 148 ../final/en_US/en_US.twitter.txt
#wc -l ../final/en_US/en_US.blogs.txt 
#899 288 ../final/en_US/en_US.blogs.txt
#wc -l ../final/en_US/en_US.news.txt 
#1 010 242 ../final/en_US/en_US.news.txt

#MaxNngram<-4
MaxNngram<-8

# make an empty data.table with corresponding structure:
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

con1 <- file("../final/en_US/en_US.twitter.txt", "r")

for(i in 1:100) {                            #  !!!! twitter chunk of 20000 takes 2+ GB RAM to process !!!
  dtNgrams<-dtNgrFunc(con1, MaxNngram,  5000) 
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_Twitter1.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

for(i in 101:200) {                            #  !!!! twitter chunk of 20000 takes 2+ GB RAM to process !!!
  dtNgrams<-dtNgrFunc(con1, MaxNngram,  5000) 
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_Twitter2.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

for(i in 201:300) {                            #  !!!! twitter chunk of 20000 takes 2+ GB RAM to process !!!
  dtNgrams<-dtNgrFunc(con1, MaxNngram,  5000) 
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_Twitter3.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

for(i in 301:473) {                            #  !!!! twitter chunk of 20000 takes 2+ GB RAM to process !!!
  dtNgrams<-dtNgrFunc(con1, MaxNngram,  5000) 
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_Twitter4.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

close(con1)



con2 <- file("../final/en_US/en_US.blogs.txt", "r")

for(i in 1:70) {
  dtNgrams<-dtNgrFunc(con2, MaxNngram,  4000)
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_Blogs1.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)


for(i in 71:80) {
  dtNgrams<-dtNgrFunc(con2, MaxNngram,  4000)
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_Blogs2.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

for(i in 81:90) {
  dtNgrams<-dtNgrFunc(con2, MaxNngram,  4000)
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_Blogs3.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

for(i in 91:110) {
  dtNgrams<-dtNgrFunc(con2, MaxNngram,  4000)
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_Blogs4.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

for(i in 111:130) {
  dtNgrams<-dtNgrFunc(con2, MaxNngram,  4000)
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_Blogs5.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

for(i in 131:150) {
  dtNgrams<-dtNgrFunc(con2, MaxNngram,  4000)
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_Blogs6.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

for(i in 151:170) {
  dtNgrams<-dtNgrFunc(con2, MaxNngram,  4000)
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_Blogs7.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

for(i in 171:190) {
  dtNgrams<-dtNgrFunc(con2, MaxNngram,  4000)
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_Blogs8.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

for(i in 191:210) {
  dtNgrams<-dtNgrFunc(con2, MaxNngram,  4000)
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_Blogs9.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

for(i in 211:225) {
  dtNgrams<-dtNgrFunc(con2, MaxNngram,  4000)
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_Blogs10.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

close(con2)



con3 <- file("../final/en_US/en_US.news.txt", "r")

for(i in 1:60) {
  dtNgrams<-dtNgrFunc(con3, MaxNngram,  1000)
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_News1.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

for(i in 61:120) {
  dtNgrams<-dtNgrFunc(con3, MaxNngram,  1000)
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_News2.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

for(i in 121:180) {
  dtNgrams<-dtNgrFunc(con3, MaxNngram,  1000)
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_News3.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

for(i in 181:240) {
  dtNgrams<-dtNgrFunc(con3, MaxNngram,  1000)
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_News4.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

for(i in 241:300) {
  dtNgrams<-dtNgrFunc(con3, MaxNngram,  1000)
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_News5.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

for(i in 301:360) {
  dtNgrams<-dtNgrFunc(con3, MaxNngram,  1000)
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_News6.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

for(i in 361:420) {
  dtNgrams<-dtNgrFunc(con3, MaxNngram,  1000)
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_News7.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

for(i in 421:480) {
  dtNgrams<-dtNgrFunc(con3, MaxNngram,  1000)
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_News8.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

for(i in 481:540) {
  dtNgrams<-dtNgrFunc(con3, MaxNngram,  1000)
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_News9.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

for(i in 541:600) {
  dtNgrams<-dtNgrFunc(con3, MaxNngram,  1000)
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_News10.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

for(i in 601:660) {
  dtNgrams<-dtNgrFunc(con3, MaxNngram,  1000)
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_News11.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

for(i in 661:720) {
  dtNgrams<-dtNgrFunc(con3, MaxNngram,  1000)
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_News12.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

for(i in 721:880) {
  dtNgrams<-dtNgrFunc(con3, MaxNngram,  1000)
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_News13.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

for(i in 881:940) {
  dtNgrams<-dtNgrFunc(con3, MaxNngram,  1000)
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_News14.sav")
rm(dtNgrams)
dtNgrams<-rep(list((data.table(ngram=character(), freq=integer(), key="ngram"))), MaxNngram)

for(i in 941:1011) {
  dtNgrams<-dtNgrFunc(con3, MaxNngram,  1000)
  print(i)
}

saveRDS(dtNgrams, "dtNgrams_8full_News15.sav")
rm(dtNgrams)

close(con3)

##########################
###  Add and sum all ngrams
#############################
dtNgrams <- readRDS("dtNgrams_8full_Twitter1.sav")

dtNgrAddSum<-function(ngrAdd, MaxNngramF) {
  dtNgramsAdd<-readRDS(ngrAdd)
  dtNgrams<-lapply(1:MaxNngramF, function(x) rbind(dtNgrams[[x]], dtNgramsAdd[[x]]))
  rm(dtNgramsAdd)
  lapply(1:MaxNngramF, function(x) dtNgrams[[x]][, lapply(.SD, sum), by = ngram])
}

dtNgrams <- dtNgrAddSum("dtNgrams_8full_Twitter2.sav", MaxNngram)
dtNgrams <- dtNgrAddSum("dtNgrams_8full_Twitter3.sav", MaxNngram)
saveRDS(dtNgrams, "dtNgrams_8fullTwitter1to3.sav")

dtNgrams <- readRDS("dtNgrams_8full_Blogs1.sav")
dtNgrams <- dtNgrAddSum("dtNgrams_8full_Blogs2.sav", MaxNngram)
dtNgrams <- dtNgrAddSum("dtNgrams_8full_Blogs3.sav", MaxNngram)
saveRDS(dtNgrams, "dtNgrams_8fullBlogs1to3.sav")

dtNgrams <- readRDS("dtNgrams_8full_Blogs4.sav")
dtNgrams <- dtNgrAddSum("dtNgrams_8full_Blogs5.sav", MaxNngram)
dtNgrams <- dtNgrAddSum("dtNgrams_8full_Blogs6.sav", MaxNngram)
dtNgrams <- dtNgrAddSum("dtNgrams_8full_Blogs7.sav", MaxNngram)

saveRDS(dtNgrams, "dtNgrams_8fullBlogs4to7.sav")

dtNgrams <- readRDS("dtNgrams_8full_Blogs8.sav")
dtNgrams <- dtNgrAddSum("dtNgrams_8full_Blogs9.sav", MaxNngram)
dtNgrams <- dtNgrAddSum("dtNgrams_8full_Blogs10.sav", MaxNngram)

saveRDS(dtNgrams, "dtNgrams_8fullBlogs8to10.sav")

dtNgrams <- readRDS("dtNgrams_8full_News1.sav")
dtNgrams <- dtNgrAddSum("dtNgrams_8full_News2.sav", MaxNngram)
dtNgrams <- dtNgrAddSum("dtNgrams_8full_News3.sav", MaxNngram)
dtNgrams <- dtNgrAddSum("dtNgrams_8full_News4.sav", MaxNngram)
dtNgrams <- dtNgrAddSum("dtNgrams_8full_News5.sav", MaxNngram)

saveRDS(dtNgrams, "dtNgrams_8fullNews1to5.sav")


dtNgrams <- readRDS("dtNgrams_8full_News6.sav")
dtNgrams <- dtNgrAddSum("dtNgrams_8full_News7.sav", MaxNngram)
dtNgrams <- dtNgrAddSum("dtNgrams_8full_News8.sav", MaxNngram)
dtNgrams <- dtNgrAddSum("dtNgrams_8full_News9.sav", MaxNngram)
dtNgrams <- dtNgrAddSum("dtNgrams_8full_News10.sav", MaxNngram)

saveRDS(dtNgrams, "dtNgrams_8fullNews6to10.sav")

dtNgrams <- readRDS("dtNgrams_8full_News11.sav")
dtNgrams <- dtNgrAddSum("dtNgrams_8full_News12.sav", MaxNngram)
dtNgrams <- dtNgrAddSum("dtNgrams_8full_News13.sav", MaxNngram)

saveRDS(dtNgrams, "dtNgrams_8fullNews11to13.sav")

dtNgrams <- readRDS("dtNgrams_8full_News14.sav")
dtNgrams <- dtNgrAddSum("dtNgrams_8full_News15.sav", MaxNngram)

saveRDS(dtNgrams, "dtNgrams_8fullNews14to15.sav")

#################
#### Extract Ngrams overall frequency
################

dtOneNgrAddSum<-function(SizeNgram) {   ##### Works only with 1 and 2-grams
  files<-c("dtNgrams_8fullTwitter1to3.sav", "dtNgrams_8full_Twitter4.sav", 
           "dtNgrams_8fullBlogs1to3.sav", "dtNgrams_8fullBlogs4to7.sav",
           "dtNgrams_8fullBlogs8to10.sav", "dtNgrams_8fullNews1to5.sav",
           "dtNgrams_8fullNews6to10.sav", "dtNgrams_8fullNews11to13.sav",
           "dtNgrams_8fullNews14to15.sav")
  dtTemp<-readRDS(files[1])
  dtMy<-dtTemp[[SizeNgram]]
  rm(dtTemp)
  print(1)
  
  dtTemp<-readRDS(files[2])
  dtMyT<-dtTemp[[SizeNgram]]
  rm(dtTemp)
  dtMy<-rbind(dtMy, dtMyT)
  print(2)
  
  dtMy<-dtMy[, lapply(.SD, sum), by = ngram]
  print("2a")  
  
  dtTemp<-readRDS(files[3])
  dtMyT<-dtTemp[[SizeNgram]]
  rm(dtTemp)
  dtMy<-rbind(dtMy, dtMyT)
  print(3)
  
  dtMy<-dtMy[, lapply(.SD, sum), by = ngram]
  print("3a")
  
  dtTemp<-readRDS(files[4])
  dtMyT<-dtTemp[[SizeNgram]]  
  rm(dtTemp)  
  dtMy<-rbind(dtMy, dtMyT)
  print(4)
  
  dtMy<-dtMy[, lapply(.SD, sum), by = ngram]
  print("4a")
  
  dtTemp<-readRDS(files[5])
  dtMyT<-dtTemp[[SizeNgram]]  
  rm(dtTemp)  
  dtMy<-rbind(dtMy, dtMyT)
  print(5)
  
  dtMy<-dtMy[, lapply(.SD, sum), by = ngram]
  print("5a")
  
  dtTemp<-readRDS(files[6])
  dtMyT<-dtTemp[[SizeNgram]]
  rm(dtTemp)
  dtMy<-rbind(dtMy, dtMyT)
  print(6)
  
  dtMy<-dtMy[, lapply(.SD, sum), by = ngram]
  print("6a")
  
  dtTemp<-readRDS(files[7])
  dtMyT<-dtTemp[[SizeNgram]]
  rm(dtTemp)
  dtMy<-rbind(dtMy, dtMyT)
  print(7)
  
  dtMy<-dtMy[, lapply(.SD, sum), by = ngram]
  print("7a")
  
  dtTemp<-readRDS(files[8])
  dtMyT<-dtTemp[[SizeNgram]]
  rm(dtTemp)
  dtMy<-rbind(dtMy, dtMyT)
  print(8)
  
  dtMy<-dtMy[, lapply(.SD, sum), by = ngram]
  print("8a")
  
  dtTemp<-readRDS(files[9])
  dtMyT<-dtTemp[[SizeNgram]]
  rm(dtTemp)
  dtMy<-rbind(dtMy, dtMyT)
  print(9)
  
  dtMy<-dtMy[, lapply(.SD, sum), by = ngram]
  print("9a")  
  
  dtMy[order(-freq)]
}


dtNgramAlltmp<-dtOneNgrAddSum(1); saveRDS(dtNgramAlltmp, "dt1gramsAll.sav")
dtNgramAlltmp<-dtOneNgrAddSum(2); saveRDS(dtNgramAlltmp, "dt2gramsAll.sav")

##########################
###########################
###########################
setwd("~/Rt/Course10_NLP/code")
library(data.table)
files<-c("dtNgrams_8fullTwitter1to3.sav", "dtNgrams_8full_Twitter4.sav", 
         "dtNgrams_8fullBlogs1to3.sav", "dtNgrams_8fullBlogs4to7.sav",
         "dtNgrams_8fullBlogs8to10.sav", "dtNgrams_8fullNews1to5.sav",
         "dtNgrams_8fullNews6to10.sav", "dtNgrams_8fullNews11to13.sav",
         "dtNgrams_8fullNews14to15.sav")



dtTemp<-readRDS(files[2])
saveRDS(dtTemp[[3]], "dt3grams2.sav")
#...
saveRDS(dtTemp[[8]], "dt8grams2.sav")
#...
dtTemp<-readRDS(files[9])
#...
saveRDS(dtTemp[[8]], "dt8grams9.sav")
##########################################
##########################################
dtTemp<-readRDS("dt3grams1.sav")

dtAdd<-readRDS("dt3grams2.sav")
dtTemp<-rbind(dtTemp, dtAdd)
rm(dtAdd)
dtTemp<-dtTemp[, lapply(.SD, sum), by = ngram]
#.....repeat
saveRDS(dtTemp, "dt3gramsAll.sav")
### and so on for 4..8-grams.




#dtTemp<-readRDS("dt8gramsAll.sav")
#setorder(dtTemp, -freq)
#Pred8gram<-data.table(ngramInp=sub("_[^_]*$", "", dtTemp$ngram), answer=sub("^[^_]*_[^_]*_[^_]*_[^_]*_[^_]*_[^_]*_[^_]*_", "", dtTemp$ngram), freq=dtTemp$freq)
#
#saveRDS(dtNgrams, "dtNgrams_8full.sav")
#dtNgrams <- readRDS("dtNgrams_8full.sav")

#dtNgrams<-lapply(1:MaxNngram, function(x) dtNgrams[[x]]<-dtNgrams[[x]][order(-freq)])
#
#setorder(dtNgrams[[2]], -freq)
#
#dtNgramsSort<-rep(list((data.table(ngramInp=character(), answer=character(), freq=integer(), key="ngramInp"))), MaxNngram)
#
#dtNgramsSort[[1]]<-data.table(ngramInp=dtNgrams[[1]]$ngram, answer=dtNgrams[[1]]$ngram, freq=dtNgrams[[1]]$freq)
#dtNgramsSort[[2]]<-data.table(ngramInp=sub("_[^_]*$", "", dtNgrams[[2]]$ngram), answer=sub("^[^_]*_", "", dtNgrams[[2]]$ngram), freq=dtNgrams[[2]]$freq)
#dtNgramsSort[[3]]<-data.table(ngramInp=sub("_[^_]*$", "", dtNgrams[[3]]$ngram), answer=sub("^[^_]*_[^_]*_", "", dtNgrams[[3]]$ngram), freq=dtNgrams[[3]]$freq)
#dtNgramsSort[[4]]<-data.table(ngramInp=sub("_[^_]*$", "", dtNgrams[[4]]$ngram), answer=sub("^[^_]*_[^_]*_[^_]*_", "", dtNgrams[[4]]$ngram), freq=dtNgrams[[4]]$freq)
#dtNgramsSort[[5]]<-data.table(ngramInp=sub("_[^_]*$", "", dtNgrams[[5]]$ngram), answer=sub("^[^_]*_[^_]*_[^_]*_[^_]*_", "", dtNgrams[[5]]$ngram), freq=dtNgrams[[5]]$freq)
#dtNgramsSort[[6]]<-data.table(ngramInp=sub("_[^_]*$", "", dtNgrams[[6]]$ngram), answer=sub("^[^_]*_[^_]*_[^_]*_[^_]*_[^_]*_", "", dtNgrams[[6]]$ngram), freq=dtNgrams[[6]]$freq)
#dtNgramsSort[[7]]<-data.table(ngramInp=sub("_[^_]*$", "", dtNgrams[[7]]$ngram), answer=sub("^[^_]*_[^_]*_[^_]*_[^_]*_[^_]*_[^_]*_", "", dtNgrams[[7]]$ngram), freq=dtNgrams[[7]]$freq)
#dtNgramsSort[[8]]<-data.table(ngramInp=sub("_[^_]*$", "", dtNgrams[[8]]$ngram), answer=sub("^[^_]*_[^_]*_[^_]*_[^_]*_[^_]*_[^_]*_[^_]*_", "", dtNgrams[[8]]$ngram), freq=dtNgrams[[8]]$freq)
#
##saveRDS(dtNgramsSort, "dtNgrams_8sortThe.sav")
#dtNgramsSort<-readRDS("dtNgrams_8sortThe.sav")



#lines5<-readLines(con)
#myCorpus1<-corpus(lines5)
#lines5<-append(lines5, readLines(con))
#myCorpus2<-corpus(lines5)
#close(con)
#lines5<-append(lines5, readLines(con))
#myCorpus3<-corpus(lines5)
#close(con)
#
#sed -n '0~5p' oldfile > newfile
#file.pipe <- pipe("awk 'BEGIN{i=0}{i++;if (i%4==0) print $1}' < test.csv ")
#res <- read.csv(file.pipe)



#ngr<-tokens_tolower(tokens(lines5, remove_punct=TRUE, ngrams=1:MaxNngram))

# constructing ngrams of size 1,2,...,MaxNngram, removing profanities and keep it in list ngr, e.g. ngr[[3]] contains trigrams
#ngr<-lapply(1:MaxNngram, function(x) tokens_select(tokens_tolower(tokens(lines5, remove_punct=TRUE, ngrams=x)), pattern = c(ngramsProfanity, stopwords('en')), selection = 'remove'))

#myCorpus<-corpus(lines5)
#rm(lines5)
#myCorpus<-corpus_reshape(myCorpus, to = 'sentences')
#tknAll<-tokens(myCorpus, remove_punct=TRUE, remove_symbols = TRUE, remove_numbers =TRUE, 
#               remove_separators = TRUE, remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE)
#rm(myCorpus)
#tknAll<-tokens_select(tknAll, pattern = c(ngramsProfanity, stopwords('en')), selection = 'remove')

#ngr<-tokens_ngrams(tknAll, n = 2)




#sngr<-tokens_select(ngr, pattern = ngramsProfanity, selection = 'remove')

#ttt<-dfm(ngr[[1]])
#tt<-colSums(ttt)
#ttt[,1:8]
#tt[1:8]
#dt<-data.table(ngram=names(tt), freq=tt)
# (dfmNgrams[[1]])@Dimnames$features

#dfmNgrams<-lapply(1:MaxNngram, function(x) colSums(dfm(ngr[[x]])))
#rm(ngr)
# create datatable with key for further search, ngrams are sorted by frequency:
#dtNgrams<-lapply(1:MaxNngram, function(x) data.table(ngram=names(dfmNgrams[[x]]), freq=dfmNgrams[[x]], key="ngram")[order(-freq)])
#rm(dfmNgrams)
#lapply(1:MaxNngram, function(x) dtNgrams[[x]][1:10])





# realization of Stupid back-off algorythm:
# we have: (MaxNngram-1)-gram from input.
# sort count of (MaxNngram)-grams, containing (MaxNngram-1)-gram as the beginning, and any word at the end
# if found, return the best
# else, use (MaxNngram-2)-gram from input
# sort count of (MaxNngram-1)-grams, containing (MaxNngram-2)-gram as the beginning, and any word at the end
# if found, return the best
# ...
#
#  Test from Quiz 2
# returnWord("it_would_mean_the", 4)
# [1] "world"
# returnWord("bacon_a_bouquet_and_a_case_of", 7)
# [1] "beer"
#
#returnWord("you_follow_me_and_make_me_the", 7)
#[1] "manager"
# Wrong, 8-5grams  no match,
# 4 grams: "manager" "best"    "role"    "most"    "16th"    "first" 
#  1 1 1 1 1 1 (very bad)
# 3 grams: "most"           "way"            "same"           "fuck"           "opportunity" 
# 25 17 15 14 14
# 3 grams: "first"          "same"           "best"           "world"          "most" 
# 6551 5535 4451 3720 3707
# no options, doesn't work.


returnWord<-function(inputNgram, inpLen){
 if(inpLen==0) return("ERROR, NULL LENGTH REACHED !!! ")
 if(inpLen>=MaxNngram){
   inpLenN<-inpLen-1
   inputNgramN<-sub("^[^_]*_", "" , inputNgram)
   return(returnWord(inputNgramN, inpLenN))
 } 
 ## find all  (inpLen+1)-grams, starting with  inputNgram,
 ## and return the most frequent (last word)   

# Output<-dtNgrams[[inpLen+1]][ngram %like% paste0(inputNgram,"_")]
 Output<-dtNgramsSort[[inpLen+1]][ngramInp==inputNgram, answer]
 
 
# if(nrow(Output)==0) {
 if(length(Output)==0) {
   inpLenN<-inpLen-1
   inputNgramN<-sub("^[^_]*_", "" , inputNgram)
   return(returnWord(inputNgramN, inpLenN))
 }
# return(Output[1]$ngram)
 return(Output[1])
}


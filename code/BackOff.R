setwd("~/Rt/Course10_NLP/code")
library(data.table)
library(dplyr)
library(dbplyr)
my_db_file <- "PredGrams.sqlite"
my_db <- src_sqlite(my_db_file)

MaxNngram<-8
TableNames<-c("Pred1gram","Pred2gram","Pred3gram","Pred4gram","Pred5gram","Pred6gram","Pred7gram")

#dtNgramsSort<-readRDS("dtNgrams_8sortTheAll.sav")

returnWordSQL<-function(inputNgram, inpLen){
  if(inpLen==0) return("ERROR, NULL LENGTH REACHED !!! ")
  if(inpLen>=MaxNngram){
    inpLenN<-inpLen-1
    inputNgramN<-sub("^[^_]*_", "" , inputNgram)
    return(returnWordSQL(inputNgramN, inpLenN))
  } 
  ## find all  (inpLen+1)-grams, starting with  inputNgram,
  ## and return the most frequent (last word)   
  
  qSBO<-tbl(my_db, TableNames[inpLen]) %>% 
        filter(ngramInp==inputNgram) %>% 
        select(answer) %>% 
        collect(n=1)  
  
  Output<-(qSBO)[1,]$answer
  
  if(is.na(Output)) {
    inpLenN<-inpLen-1
    inputNgramN<-sub("^[^_]*_", "" , inputNgram)
    return(returnWordSQL(inputNgramN, inpLenN))
  }
  return(Output)
}




returnWordMEM<-function(inputNgram, inpLen){
  if(inpLen==0) return("ERROR, NULL LENGTH REACHED !!! ")
  if(inpLen>=MaxNngram){
    inpLenN<-inpLen-1
    inputNgramN<-sub("^[^_]*_", "" , inputNgram)
    return(returnWordMEM(inputNgramN, inpLenN))
  } 
  ## find all  (inpLen+1)-grams, starting with  inputNgram,
  ## and return the most frequent (last word)   
  
  # Output<-dtNgrams[[inpLen+1]][ngram %like% paste0(inputNgram,"_")]
  Output<-dtNgramsSort[[inpLen+1]][ngramInp==inputNgram, answer]
  
  
  # if(nrow(Output)==0) {
  if(length(Output)==0) {
    inpLenN<-inpLen-1
    inputNgramN<-sub("^[^_]*_", "" , inputNgram)
    return(returnWordMEM(inputNgramN, inpLenN))
  }
  # return(Output[1]$ngram)
  return(Output[1])
}

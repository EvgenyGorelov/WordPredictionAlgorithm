
returnWordMEM<-function(inputNgram, inpLen){
  if(inpLen==0) return("the")
  if(inpLen>MaxNngram){
    inpLenN<-inpLen-1
    inputNgramN<-sub("^[^_]*_", "" , inputNgram)
    return(returnWordMEM(inputNgramN, inpLenN))
  } 
  ## find all  (inpLen)-grams, starting with  inputNgram,
  ## and return the most frequent answer (last word)   
  Output<-dtNgramsSort[[inpLen]][ngramInp==inputNgram, answer]

  if(length(Output)==0) {
    inpLenN<-inpLen-1
    inputNgramN<-sub("^[^_]*_", "" , inputNgram)
    return(returnWordMEM(inputNgramN, inpLenN))
  }
  return(Output[1])
#  return(paste(Output[1], "              ||||| actualInp: ", inputNgram, "actualLen: ", inpLen))
}


shinyServer(  
  function(input, output) { 
    output$myText<-renderText({
  # take the piece after last punctuation, skip numbers and symbols:
       CroppedText<-str_extract(input$MyTextInp, "([a-zA-Z_\' ]+)$")
       CroppedText<-gsub("[^a-zA-Z_\' ]", "",CroppedText) 
  # crop text, remove spaces/underscores from start/end     
       CroppedText<-sub("^[ _]*([a-zA-Z0-9]+[a-zA-Z0-9 _]*[a-zA-Z0-9]+)[ _]*$","\\1",CroppedText)
       CroppedText<-tolower(CroppedText)
  # make_a_proper_ngram_syntax  :)      
       MyNgram <- gsub("[ _]+", "_", CroppedText)
       MyNgrLen <- str_count(MyNgram, "_")+1
       if (is.na(MyNgram)||MyNgram=="_") return("the")
       returnWordMEM(MyNgram, MyNgrLen)
#       paste(returnWordMEM(MyNgram, MyNgrLen), "MyNgrLen: ", MyNgrLen,"MyNgram: ",MyNgram)
    })
  })    
       

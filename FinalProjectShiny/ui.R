shinyUI(mainPanel(headerPanel("Data Science Final Project App: word prediction algorithm"),    
    textInput('MyTextInp',h3('Please enter text or n-gram below:')),
    h3('predicted word:'),
    textOutput('myText'),
    h3('Description:'),
    p('The application is based on Stupid Back-Off (SBO) algorithm: the longest matching the input (n-1)-gram
       is being selected, the last word of the n-gram is used as an answer. For details see ref. 
       (Proc. of the 2007 Joint Conf. on Empirical Methods in Natural Language Processing and Computational Natural Language Learning, pp. 858–867, Prague, 2007). 
      The ngrams with frequency 1 were omitted in order to fit the array in 1GB limits.
      The data is stored as a list of data.table objects, containing the following columns: 
      2-gram first word for input; 2-gram second word for prediction; ... 7-gram first 6-gram for input; 7-gram last word for prediction.  
      The data.table objects contains from 186K rows for 1-grams to 2.5M rows for 3-grams 
      and takes 735 MB of memory. In the absence of information for the prediction the code propose a word THE, as the most common one.
      ')
#    textOutput('myTextDebug')
  )
)

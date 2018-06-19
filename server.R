#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(NLP)
library(tm)
library(stringr)

bigrams <- readRDS("databigrams.Rdata", refhook = NULL); 
trigrams <- readRDS("datatrigrams.Rdata");
qugrams <- readRDS("dataqugrams.Rdata")




nwbigrams<-function(senSplit) {
  nw<-as.character(head((bigrams[bigrams$word1==senSplit[1],])$word2,1))
  freq<-as.character(head((bigrams[bigrams$word1==senSplit[1],])$Count,1))
  if(identical(nw,character(0))) {
    nw<-"Try_Other_Word!"
    freq<-0
  }
  nwlist<-list(nw, freq)
  return(nwlist)
}

nwtrigrams<-function(senSplit) {
  nw<-as.character(head((trigrams[trigrams$word1==senSplit[1] & trigrams$word2 == senSplit[2],])$word3,1))
  freq<-as.character(head((trigrams[trigrams$word1==senSplit[1] & trigrams$word2 == senSplit[2],])$Count,1))
  nwlist<-list(nw, freq)
  if(identical(nw,character(0))) {
    nwlist=nwFunction(senSplit[2])
  }
  return(nwlist)
}

nwqugrams<-function(senSplit) {
  nw<-as.character(head((qugrams[qugrams$word1==senSplit[1] & qugrams$word2 == senSplit[2] & qugrams$word3 == senSplit[3]
                                 ,])$word4,1))
  freq<-as.character(head((qugrams[qugrams$word1==senSplit[1] & qugrams$word2 == senSplit[2] & qugrams$word3 == senSplit[3]
                                   ,])$Count,1))
  nwlist<-list(nw, freq)
  if(identical(nw,character(0))) {
    nwlist=nwFunction(paste(senSplit[2],senSplit[3],sep=" "))
  }
  return(nwlist)
}


nwFunction<-function(frase, gramUse=0)  {
  frase_c<-removeWords(frase,stopwords('en'))
  frase_c<-stripWhitespace(removeNumbers(removePunctuation(tolower(frase_c),preserve_intra_word_dashes = TRUE)))
  senSplit<- strsplit(frase_c," ")[[1]]
  qwords<-length(senSplit)
  if(qwords==1 || gramUse==2) { 
    nwlist<-nwbigrams(tail(senSplit,1))
  }  
  else if(qwords==2 || gramUse==3) { 
    nwlist<-nwtrigrams(tail(senSplit,2))
  }
  else if(qwords>2 || gramUse==3) {
    nwlist<-nwqugrams(tail(senSplit,3))
  }
  else {
    nwlist<-list("Try_Other_Word!",0)
  }
  return(nwlist)
}

# Define server logic required 

shinyServer(function(input, output) {
  output$nw<-renderText({
    word<-nwFunction(input$inputText,0)
    word[[1]]
    
  });
})

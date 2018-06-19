getwd()
setwd("C:/Cproject")
monGrams <- readRDS("data/monGrams.RData")
biGrams <- readRDS("data/biGrams.RData")
triGrams <- readRDS("data/triGrams.RData")


library(NLP)
library(tm)


nwBigrams<-function(senSplit) {
  nw<-as.character(head((biGrams[biGrams$word1==senSplit[1],])$word2,1))
  freq<-as.character(head((biGrams[biGrams$word1==senSplit[1],])$Count,1))
  if(identical(nw,character(0))) {
    nw<-"Try_Other_Word!"
    freq<-0
  }
  nwlist<-list(nw, freq)
  return(nwlist)
}

nwTrigrams<-function(senSplit) {
  nw<-as.character(head((triGrams[triGrams$word1==senSplit[1] & triGrams$word2 == senSplit[2],])$word3,1))
  freq<-as.character(head((triGrams[triGrams$word1==senSplit[1] & triGrams$word2 == senSplit[2],])$Count,1))
  nwlist<-list(nw, freq)
  if(identical(nw,character(0))) {
    nwlist=nwFunction(senSplit[2])
  }
  return(nwlist)
}

nwQugrams<-function(senSplit) {
  nw<-as.character(head((quGrams[quGrams$word1==senSplit[1] 
                                 & quGrams$word2 == senSplit[2]
                                 & quGrams$word3 == senSplit[3]
                                 ,])$word4,1))
  freq<-as.character(head((quGrams[quGrams$word1==senSplit[1] 
                                   & quGrams$word2 == senSplit[2]
                                   & quGrams$word3 == senSplit[3]
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
    nwlist<-nwBigrams(tail(senSplit,1))
  }  

  else if(qwords==2 || gramUse==3) { 
    nwlist<-nwTrigrams(tail(senSplit,2))
  }
  else if(qwords>2 || ngrams_words==3) {
    nwlist<-nwQugrams(tail(senSplit,3))
  }
  
  else {
    nwlist<-list("Try_Other_Word!",0)
  }
  return(nwlist)
}

########################

nwsearch<-"god bless to america"
result<-nwFunction(nwsearch,0)
result[[1]]



#read input files
#no input
#returns the union of all the inputs (totlines)
getwd()

monGrams <- readRDS("data\monGrams.RData")
biGrams <- readRDS("biGrams.RData")
triGrams <- readRDS("triGrams.RData")

library(NLP)
library(tm)


average_frequency<-function(sentence)  {
  sentence_c<-removeWords(sentence,stopwords('en'))
  sentence_c<-stripWhitespace(removeNumbers(removePunctuation(tolower(sentence_c),preserve_intra_word_dashes = TRUE)))
  sentence_split<- strsplit(sentence_c," ")[[1]]
  #print(paste("sentence_split:",sentence_split))
  qwords<-length(sentence_split)
  tot_frequency<-0
  for (i in c(1,2,3)) {
    weight_i<-i/10
    #print(paste("weight_i:",weight_i))
    last_words<-tail(sentence_split,i)
    #print(paste("last_words:",last_words))
    if(i==1) {
      freq<-as.integer(head((monGrams[monGrams$word1==last_words[1],])$Count,1))
      
    }
    else  
      if(i==2) {
        freq<-as.integer(head((biGrams[biGrams$word1==last_words[1] 
                                       & biGrams$word2 == last_words[2]                               
                                       ,])$Count,1))
        
      } else if(i==3) {
        freq<-as.integer(head((triGrams[triGrams$word1==last_words[1] 
                                        & triGrams$word2 == last_words[2]
                                        & triGrams$word3 == last_words[3]
                                        ,])$Count,1))
      }
    #print(paste("freq:",freq))
    if(length(freq)==0) freq<-0
    tot_frequency<-tot_frequency+(weight_i*freq)
    #cat(sprintf("with %d words tot_frequency %.2f\n",i, tot_frequency))
  }
  print(tot_frequency)
  return(tot_frequency)
}





######

#p1
sentence<-"The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
options<- c('pretzels','cheese', 'beer', 'soda')
for(i in 1:length(options)) {
  sentence_n<-paste(sentence, options[i])
  print(sentence_n)
  a<-average_frequency(sentence_n)
  print(a)
}


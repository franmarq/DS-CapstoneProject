#read input files
#no input
#returns the union of all the inputs (totlines)
unigram <- readRDS("unigram.RData")
bigram <- readRDS("bigram_nostopwords.RData")
trigram <- readRDS("trigram_nostopwords.RData")
quagram <- readRDS("quagram_nostopwords.RData")

average_frequency<-function(sentence)  {
  sentence_c<-removeWords(sentence,stopwords('en'))
  sentence_c<-stripWhitespace(removeNumbers(removePunctuation(tolower(sentence_c),preserve_intra_word_dashes = TRUE)))
  sentence_split<- strsplit(sentence_c," ")[[1]]
  qwords<-length(sentence_split)
  tot_frequency<-0
  for (i in c(1,2,3,4)) {
    weight_i<-i/10
    last_words<-tail(sentence_split,i)
    #print(last_words)
    if(i==1) {
      freq<-as.integer(head((unigram[unigram$word1==last_words[1],])$freq,1))
    }
    else  
      if(i==2) {
        freq<-as.integer(head((bigram[bigram$word1==last_words[1] 
                                      & bigram$word2 == last_words[2]                               
                                      ,])$freq,1))
        
      } else if(i==3) {
        freq<-as.integer(head((trigram[trigram$word1==last_words[1] 
                                       & trigram$word2 == last_words[2]
                                       & trigram$word3 == last_words[3]
                                       ,])$freq,1))
      }
    else if(i==4) {
      freq<-as.integer(head((quagram[quagram$word1==last_words[1] 
                                     & quagram$word2 == last_words[2]
                                     & quagram$word3 == last_words[3]
                                     & quagram$word4 == last_words[4]
                                     ,])$freq,1))
      
    }
    if(length(freq)==0) freq<-0
    tot_frequency<-tot_frequency+(weight_i*freq)
    #cat(sprintf("with %d words tot_frequency %.2f\n",i, tot_frequency))
  }
  #print(tot_frequency)
  return(tot_frequency)
  
}


###########

sentence<-"When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd "
options<- c('give','eat','die','sleep')
for(i in 1:length(options)) {
  sentence_n<-paste(sentence, options[i])
  print(sentence_n)
  a<-average_frequency(sentence_n)
  a
}
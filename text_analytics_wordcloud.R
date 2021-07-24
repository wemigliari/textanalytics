

library(wordcloud)
library(SnowballC)
library(tm)
library(RCurl)
require(XML)

dilma_statement_pdf <- pdf_text(pdf = "https://gadebate.un.org/sites/default/files/gastatements/66/BR_en_0.pdf")

myCorpus_Dilma_pdf <- paste0(as.character(dilma_statement_pdf))
class(myCorpus_Dilma_pdf)

### Tokenizer sentences

words_Dilma <- tokenize_ngrams(myCorpus_Dilma_pdf, 
                               n=4,
                               simplify = TRUE, 
                               stopwords = c("is", "not", "one", "at","laid", "are","has","such","that","as", "which","under","the", "on", "with", "down", "other", "of", "and", "for", "to", "in", "a", "an", "by", "or", "à"))


### Tokenizer words

library(tokenizers)
words_Dilma <- tokenize_words(myCorpus_Dilma_pdf, strip_numeric = TRUE, 
                              simplify = TRUE, 
                              stopwords = c("'s", "is", "not", "one", "at","laid", "are","has","such","that","as", "which","under","the", "on", "with", "down", "other", "of", "and", "for", "to", "in", "a", "an", "by", "or", "à"))

tab_Dilma <- data.frame(matrix(unlist(words_Dilma), ncol = 1),stringsAsFactors=FALSE)


library(plyr)

frequency_Dilma <- count(tab_Dilma)

# Descending order
frequency_Dilma <- frequency_Dilma[order(frequency_Dilma$freq, decreasing = TRUE),]
names(frequency_Dilma)[1] <- "words"

percent_Dilma <- frequency_Dilma %>% group_by(words) %>%
  mutate(Percentage=paste0(round(freq/sum(freq),8)))

percent_Dilma <- data.frame(percent_Dilma)

num <- as.numeric(percent_Dilma[,3])
percent_Dilma <- cbind(percent_Dilma, num)
colnames(percent_Dilma)[4]<-"Percentage"
percent_Dilma[,3] <- NULL


#### wordcloud

dev.new(width = 1000, height = 1000, unit = "px")
wordcloud(words = percent_Dilma$words, freq = percent_Dilma$freq, 
          min.freq = 1,
          scale=c(3.5,0.25),
          max.words=500, random.order=FALSE, rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))


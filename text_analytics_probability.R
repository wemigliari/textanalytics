
library(dplyr)
library(tidytext)
library(rvest)
library(pdftools)


riks_pdf <- pdf_text(pdf = "https://www.riksdagen.se/globalassets/07.-dokument--lagar/the-instrument-of-government-2015.pdf")

myCorpus_pdf <- paste0(as.character(riks_pdf))
class(myCorpus_pdf)

library(tokenizers)
words_riks <- tokenize_words(myCorpus_pdf, strip_numeric = TRUE, 
                             simplify = TRUE, 
                             stopwords = c("is", "not", "one", "at","laid", "are","has","such","that","as", "which","under","the", "on", "with", "down", "other", "of", "and", "for", "to", "in", "a", "an", "by", "or", "à"))

tab_riks <- data.frame(matrix(unlist(words_riks), ncol = 1),stringsAsFactors=FALSE)


######


library(plyr)

frequency <- count(tab_riks)

# Descending order
frequency <- frequency[order(frequency$freq, decreasing = TRUE),]
names(frequency)[1] <- "words"

percent <- frequency %>% group_by(words) %>%
  mutate(Percentage=paste0(round(freq/sum(freq),8)))
percent <- data.frame(percent)
num <- as.numeric(percent[,3])
percent <- cbind(percent, num)
colnames(percent)[4]<-"Percentage"
percent[,3] <- NULL


percent3 <- data.frame(percent)

### Probability. What is the probability of a word set being repeated x times?

Proportion <- percent3$freq/sum(percent3$freq)
sum(Proportion)

sr_df <- cbind(percent3$words, sr_df, Proportion)
sr_df$`percent3$words`<- NULL
names(sr_df)[1]<-"Words"


### Probability on Repetitions, not on the number of events over the total number of words
pp <- function(x) {
  print(paste0(round(x*100, 7), "%"))
}
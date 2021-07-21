
library(dplyr)
library(tidytext)
library(rvest)
library(pdftools)
library(tokenizers)


riks_pdf <- pdf_text(pdf = "https://www.riksdagen.se/globalassets/07.-dokument--lagar/the-instrument-of-government-2015.pdf")

myCorpus_pdf <- paste0(as.character(riks_pdf))
class(myCorpus_pdf)

### Tokenizer sentences

words_riks2 <- tokenize_ngrams(myCorpus_pdf, 
                               n=4,
                               simplify = TRUE, 
                               stopwords = c("is", "not", "one", "at","laid", "are","has","such","that","as", "which","under","the", "on", "with", "down", "other", "of", "and", "for", "to", "in", "a", "an", "by", "or", "à"))


### Tokenizer words

library(tokenizers)
words_riks <- tokenize_words(myCorpus_pdf, strip_numeric = TRUE, 
                             simplify = TRUE, 
                             stopwords = c("'s", "is", "not", "one", "at","laid", "are","has","such","that","as", "which","under","the", "on", "with", "down", "other", "of", "and", "for", "to", "in", "a", "an", "by", "or", "à"))

tab_riks <- data.frame(matrix(unlist(words_riks), ncol = 1),stringsAsFactors=FALSE)

library(quanteda)
# remove punctuation
tab_riks <- as.character(tab_riks)
tab_riks <- quanteda::tokens(tab_riks, remove_punct = TRUE)
# remove stopwords
tab_riks <- tokens_remove(tab_riks, stopwords("english"))
class(tab_riks)

tab_riks <- data.frame(Tokens = as.character(tab_riks), 
           stringsAsFactors = FALSE)
tab_riks$Tokens <- gsub("'s|c", '', test$Tokens) ## removing single quote

tab_riks <- tab_riks[-c(1),] 
tab_riks <- data.frame(tab_riks)
colnames(tab_riks)[1] <- "Words"

duplicated(tab_riks) 
tab_riks<- tab_riks %>% distinct() #Removing duplicates if that's the case
duplicated(tab_riks)



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

### Tokens

library(quanteda)
# remove punctuation
tab_riks_plot <- as.character(tab_riks)
tab_riks_plot <- quanteda::tokens(tab_riks_plot, remove_punct = TRUE)
# remove stopwords
tab_riks_plot <- tokens_remove(tab_riks_plot, stopwords("english"))
class(tab_riks_plot)
head(tab_riks_plot)

# Lexical Dispersion Plot

library("quanteda.textplots")

text_plot <- textplot_xray(
  kwic(tab_riks_plot, pattern = "riksdag"),
  kwic(tab_riks_plot, pattern = "law"),
  kwic(tab_riks_plot, pattern = "democracy"),
  kwic(tab_riks_plot, pattern = "government"),
  kwic(tab_riks_plot, pattern = "powers"),
  kwic(tab_riks_plot, pattern = "freedom"),
  scale = "absolute"                          ### scale parameter can be "relative"
)

text_plot + aes(color = keyword) + 
  scale_color_manual(values = c("steelblue", "lightgray", "darkgreen", "lightblue", "turquoise", "orange")) +
  theme(legend.position = "none") +
  ggtitle("Lexical Dispersion. Instrument of Government. English version.") +
  labs(x = "Repetitions")


### From tokens to depurated tokens

# remove punctuation
tab_riks3 <- as.character(tab_riks)
tab_riks3 <- quanteda::tokens(tab_riks3, remove_punct = TRUE)

# remove stopwords
tab_riks3 <- tokens_remove(tab_riks3, stopwords("english"))
class(tab_riks3)
head(tab_riks3)

tab_riks3 <- data.frame(Tokens = as.character(tab_riks3), 
           stringsAsFactors = FALSE)

tab_riks3$Tokens <- gsub("'s|c", '', tab_riks3$Tokens) ## removing single quote and other characters
tab_riks3 <- tab_riks3[-c(1),]

tab_riks3 <- data.frame(tab_riks3)

colnames(tab_riks3)[1] <- "Words"

#### Removing the repetitions. Only useful for a list of words.

duplicated(tab_riks3) 
tab_riks4<- tab_riks3 %>% distinct() #Removing duplicates if that's the case
duplicated(tab_riks4)

summary(tab_riks4)


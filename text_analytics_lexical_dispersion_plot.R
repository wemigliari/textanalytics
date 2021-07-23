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


### From tokens to depurated tokens

# remove punctuation
tab_riks3 <- as.character(tab_riks)
tab_riks3 <- quanteda::tokens(tab_riks3, remove_punct = TRUE)

# remove stopwords
tab_riks3 <- tokens_remove(tab_riks3, stopwords("english"))
class(tab_riks3)
head(tab_riks3)


## Relative - Index

text_plot3 <- textplot_xray(
  kwic(tab_riks3, pattern = "riksdag"),
  kwic(tab_riks3, pattern = "law"),
  kwic(tab_riks3, pattern = "democracy"),
  kwic(tab_riks3, pattern = "government"),
  kwic(tab_riks3, pattern = "powers"),
  kwic(tab_riks3, pattern = "freedom"),
  scale = "relative"                          ### scale parameter can be "absolute"
)

text_plot3 + aes(color = keyword) + 
  scale_color_manual(values = c("steelblue", "lightgray", "darkgreen", "lightblue", "turquoise", "orange")) +
  theme(legend.position = "none") +
  ggtitle("Lexical Dispersion. Instrument of Government. English version.") +
  labs(x = "Index")

## Absolute - Total Repetitions

text_plot4 <- textplot_xray(
  kwic(tab_riks3, pattern = "riksdag"),
  kwic(tab_riks3, pattern = "law"),
  kwic(tab_riks3, pattern = "democracy"),
  kwic(tab_riks3, pattern = "government"),
  kwic(tab_riks3, pattern = "powers"),
  kwic(tab_riks3, pattern = "freedom"),
  scale = "absolute"                          ### scale parameter can be "relative"
)

text_plot4 + aes(color = keyword) + 
  scale_color_manual(values = c("orange", "lightgray", "turquoise", "lightblue", "lightgray", "steelblue")) +
  theme(legend.position = "none") +
  ggtitle("Lexical Dispersion. Instrument of Government. English version.") +
  labs(x = "Repetitions of Entries")




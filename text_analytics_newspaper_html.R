library(dplyr)
library(tidytext)
library(rvest)

newspaper <- read_html("https://www.theguardian.com/uk/environment")

headlines <- newspaper %>%
  html_nodes(".js-headline-text") %>%
  html_text() %>% 
  strsplit(split = "\n") %>%
  unlist() %>%
  .[. != ""]

class(headlines)
headlines <- data.frame(headlines, stringsAsFactors = FALSE)


# 1.1 Words

myCorpus <- paste(headlines)

library(tokenizers)

words <- tokenize_words(myCorpus)
tab <- table(words[[1]])
tab <- data_frame(word = names(tab), count = as.numeric(tab))
tab

duplicated(tab) 
tab <- tab %>% distinct() #Removing duplicates if that's the case
duplicated(tab)

head(tab, 10)
tail(tab, 10)

tab <- arrange(tab, desc(count))
tab <- data.frame(tab)
class(tab)



# 1.2 Sentences 

newspaper <- read_html("https://www.theguardian.com/uk/environment")

headlines <- newspaper %>%
  html_nodes(".js-headline-text") %>%
  html_text() %>% 
  strsplit(split = "\n") %>%
  unlist() %>%
  .[. != ""]

sample_tibble <- tibble(text = headlines)



tft_token_ngram <- tokenize_ngrams(x = myCorpus,
                                   lowercase = TRUE,
                                   n = 2L,
                                   n_min = 2L,
                                   stopwords = character(),
                                   ngram_delim = " ",
                                   simplify = FALSE)

tft_token_ngram[[1]]

quantify <- data.frame(tft_token_ngram[[1]])


duplicated(quantify) 
quantify <- quantify %>% distinct() #Removing duplicates if that's the case
duplicated(quantify)

class(quantify)





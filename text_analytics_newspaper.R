library(dplyr)
library(tidytext)
library(rvest)

newspaper <- read_html("https://www.theguardian.com/uk/commentisfree/")

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

newspaper <- read_html("https://www.theguardian.com/uk/commentisfree/")

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


# 1.3 Reading PDF files


library(pdftools)
riks_pdf <- pdf_text(pdf = "https://www.riksdagen.se/globalassets/07.-dokument--lagar/the-instrument-of-government-2015.pdf")

myCorpus_pdf <- paste0(riks_pdf)

words_riks <- tokenize_words(myCorpus_pdf, strip_numeric = TRUE, 
                             simplify = TRUE, 
                             stopwords = c("the", "of", "and", "for", "to", "in", "a", "an", "by", "or", "à"))

tab_riks <- data.frame(matrix(unlist(words_riks), ncol = 1),stringsAsFactors=FALSE)


library(plyr)

frequency <- count(tab_riks)

# Descending order
frequency <- frequency[order(frequency$freq, decreasing = TRUE),]
names(frequency)[1] <- "words"

percent <- frequency %>% group_by(words) %>%
  mutate(Percentage=paste0(round(freq/sum(freq),8)))
percent <- data.frame(percent)

percent2 <- data.frame(percent[c(1:15),])

library(ggplot2)
library(hrbrthemes)
library(RColorBrewer)

nb.cols <- 15
mycolors <- colorRampPalette(brewer.pal(8, "Blues"))(nb.cols) 

ggplot(percent2, aes(reorder(x=words, -freq, sum), y = freq, fill = words, label = words)) + 
  theme_bw() + 
  geom_bar(stat = "identity")+
  theme(legend.position = "right") +
  geom_text(size = 2.5, color="black", position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = mycolors) +
  guides(fill=guide_legend(title="Tipologia do quantum"))


### Duplication counts. Not to be removed.

head(tab_riks, 10)
tail(tab_riks, 10)

tab_riks <- arrange(tab_riks, desc(count))
tab_riks <- data.frame(tab_riks)
class(tab_riks)







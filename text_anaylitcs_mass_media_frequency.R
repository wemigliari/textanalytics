library(dplyr)
library(tidytext)
library(rvest)
library(pdftools)

##### The Guardian

newspaper <- read_html("https://www.theguardian.com/uk/environment")

headlines <- newspaper %>%
  html_nodes(".js-headline-text") %>%
  html_text() %>% 
  strsplit(split = "\n") %>%
  unlist() %>%
  .[. != ""]

class(headlines)
headlines <- data.frame(headlines, stringsAsFactors = FALSE)

headlines$headlines <- gsub(paste0('\\b',tm::stopwords("english"), '\\b', 
                                   collapse = '|'), '', headlines$headlines)


#### Words

myCorpus <- paste(headlines)

library(tokenizers)
words_guardian <- tokenize_words(myCorpus, strip_numeric = TRUE, 
                             simplify = TRUE, 
                             stopwords = c("is", "not", "one", "at","laid", "are","has","such","that","as", "which","under","the", "on", "with", "down", "other", "of", "and", "for", "to", "in", "a", "an", "by", "or", "à"))

tab_guardian<- data.frame(matrix(unlist(words_guardian), ncol = 1),stringsAsFactors=FALSE)


######

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

tab = tab[-c(8, 59),]
tab$newspaper <- "The Guardian"


tab1 <- tab[1:143,]
tab1 <- tab1[-3,]

library(dplyr)

# Adding column based on other column:
#testA <- tab1 %>%
#mutate(id = case_when(
#endsWith(word, "cop26") ~ "s01",
#endsWith(word, "climate") ~ "s02",
#endsWith(word, "uk") ~ "s03",
#endsWith(word, "carbon") ~ "s04"
#))

# Adding column based on other column:
testA <- tab1 %>%
  mutate(
    type = case_when(
      count == 34  ~ "government",
      count == 32  ~ "governance",
      count == 10 ~ "civil society",
      count == 8 ~ "culture",
      count == 6 ~ "values",
      count == 4 ~ "behaviour",
      count == 2 ~ "opinion"
    )
  )

#####
dev.off()

par(mfrow=c(1,2), mar=c(6,1,2,1), oma=c(2,2,1,1))

barplot(testA[1:10,]$count, las = 2, names.arg = testA[1:10,]$type,
        col ="gold", main ="The Guardian",
        ylab = "Frequencies", cex.axis=0.7, cex.names=0.7)

barplot(testB[1:10,]$count, las = 2, names.arg = testA[1:10,]$type,
        col ="red", main ="Dagens Nyheter",
        ylab = "Frequencies", cex.axis=0.7, cex.names=0.7)






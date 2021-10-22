library(dplyr)
library(tidytext)
library(rvest)
library(pdftools)
library(ggplot2)


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


percent2 <- data.frame(percent[c(1:15),])

percent3 <- data.frame(percent)


### Cumulative distribution function

sample.range <- percent3$freq
class(sample.range)
sr_mean <- mean(sample.range)
sr_sd <- sd(sample.range)
sr_df <- data.frame(sample.range)
names(sr_df)[1] <- "Repetition"

sr_pnorm <- pnorm(sample.range, sr_mean, sr_sd)
sr_df2 <- cbind(sr_df, "CDF" = sr_pnorm)
ggplot(sr_df2, aes(x = Repetition, y = CDF)) + geom_line() +
  labs(title="",
       y = "Cumulative Distribution Function", x = "", caption = "Source: Sveriges Riksdag",
       color = "")


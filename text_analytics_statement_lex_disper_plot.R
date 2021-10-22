library(dplyr)
library(tidytext)
library(rvest)
library(pdftools)
library(tokenizers)

##### Lexical Dispersion

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


### From tokens to depurated tokens

library(quanteda)
# remove punctuation
tab_Dilma1 <- as.character(tab_Dilma)
tab_Dilma2 <- quanteda::tokens(tab_Dilma1, remove_punct = TRUE)

# remove stopwords
tab_Dilma2 <- tokens_remove(tab_Dilma2, stopwords("english"))
class(tab_Dilma2)
head(tab_Dilma2)


## Relative - Index

library("quanteda.textplots")
library(ggplot2)

text_plot_Dilma <- textplot_xray(
  kwic(tab_Dilma2, pattern = "nations"),
  kwic(tab_Dilma2, pattern = "women"),
  kwic(tab_Dilma2, pattern = "voice"),
  kwic(tab_Dilma2, pattern = "feminine"),
  kwic(tab_Dilma2, pattern = "democracy"),
  kwic(tab_Dilma2, pattern = "government"),
  kwic(tab_Dilma2, pattern = "empowering"),
  kwic(tab_Dilma2, pattern = "freedom"),
  scale = "relative"                          ### scale parameter can be "absolute"
)

text_plot_Dilma + aes(color = keyword) + 
  scale_color_manual(values = c("steelblue", "lightgray", "darkgreen", "lightblue", "turquoise", "orange", "red", "gray")) +
  theme(legend.position = "none") +
  ggtitle("Lexical Dispersion. 2011 Dilma Rousseff's Statement, United Nations.") +
  labs(x = "Index")

## Absolute - Total Repetitions

text_plot_Dilma2 <- textplot_xray(
  kwic(tab_Dilma2, pattern = "nations"),
  kwic(tab_Dilma2, pattern = "women"),
  kwic(tab_Dilma2, pattern = "voice"),
  kwic(tab_Dilma2, pattern = "feminine"),
  kwic(tab_Dilma2, pattern = "democracy"),
  kwic(tab_Dilma2, pattern = "government"),
  kwic(tab_Dilma2, pattern = "empowering"),
  kwic(tab_Dilma2, pattern = "freedom"),
  scale = "absolute"                          ### scale parameter can be "relative"
)

text_plot_Dilma2 + aes(color = keyword) + 
  scale_color_manual(values = c("steelblue", "lightgray", "darkgreen", "lightblue", "turquoise", "orange", "red", "gray")) +
  theme(legend.position = "none") +
  ggtitle("Lexical Dispersion. 2011 Dilma Rousseff's Statement, United Nations.") +
  labs(x = "Repetitions of Entries")

##### Frequency of Words

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


percent_Dilma2 <- data.frame(percent_Dilma[c(1:30),])

###

par(mar=c(5,10,4,2)) # increase y-axis margin.
barplot(percent_Dilma2$freq, main="Top 30 Words in 2011 Dilma Rousseff's Statement, United Nations", horiz=TRUE, 
        names.arg=percent_Dilma2$words, cex.names=0.8, las=1)

###


### Normal distribution - word repetition

sample.range <- percent_Dilma2$freq

class(sample.range)
sr_mean <- mean(sample.range)

sr_sd <- sd(sample.range)
sr_dist <- dnorm(sample.range, mean = sr_mean, sd = sr_sd)

sr_df <- data.frame("Repetition" = sample.range, "Density" = sr_dist)

library(ggplot2)
ggplot(sr_df, aes(x = Repetition, y = Density)) + geom_line() +
  labs(title="",
       y = "Normal Density Function", x = "", caption = "Source: United Nations. Elaborated by W. Migliari (2021).",
       color = "")

### Cumulative distribution function

sr_pnorm <- pnorm(sample.range, sr_mean, sr_sd)
sr_df2 <- cbind(sr_df, "CDF" = sr_pnorm)
ggplot(sr_df2, aes(x = Repetition, y = CDF)) + geom_line() +
  labs(title="",
       y = "Cumulative Distribution Function", x = "", caption = "Source: United Nations. Elaborated by W. Migliari (2021).",
       color = "")

####

library(scales)

sr_df3 <- cbind(sr_df2, percent_Dilma2$words, as.numeric(percent_Dilma2$Percentage))
names(sr_df3)[4]<- "Words"
names(sr_df3)[5]<- "Percentage"

# expect a warning about rows with missing values being removed             https://www.tidytextmining.com/tidytext.html
ggplot(sr_df3, aes(x = Repetition, y = Percentage, 
                   color = abs(Percentage - Repetition))) +
  geom_jitter(alpha = 0.3, size = sr_df3$Repetition, width = 0.3, height = 0.3) +
  geom_text(aes(label = Words), check_overlap = TRUE, vjust = 1.5, size = 2.5, angle=0) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  scale_y_log10(labels = percent_format()) +
  theme(legend.position="none") +
  labs(y = "Cumulative Percentage of Repetition", x = NULL)


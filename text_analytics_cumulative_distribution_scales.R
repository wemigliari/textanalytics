
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

sample.range <- percent3$freq
class(sample.range)

sr_mean <- mean(sample.range)
sr_sd <- sd(sample.range)

sr_dist <- dnorm(sample.range, mean = sr_mean, sd = sr_sd)
plot(sr_dist)
sr_df <- data.frame("Repetition" = sample.range, "Density" = sr_dist)

sr_pnorm <- pnorm(sample.range, sr_mean, sr_sd)
sr_df2 <- cbind(sr_df, "CDF" = sr_pnorm)

#### Scales

library(scales)

sr_df3 <- cbind(sr_df2, percent3$words, as.numeric(percent3$Percentage))
names(sr_df3)[4]<- "Words"
names(sr_df3)[5]<- "Percentage"

# expect a warning about rows with missing values being removed             https://www.tidytextmining.com/tidytext.html
ggplot(sr_df3, aes(x = Repetition, y = Percentage, 
                   color = abs(Percentage - Repetition))) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = Words), check_overlap = TRUE, vjust = 1.5, size = 2.5, angle=45) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  scale_y_log10(labels = percent_format()) +
  theme(legend.position="none") +
  labs(y = "Cumulative Percentage of Repetition", x = NULL)

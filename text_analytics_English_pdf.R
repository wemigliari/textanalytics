# 1.3 Reading PDF files


library(pdftools)
riks_pdf <- pdf_text(pdf = "https://www.riksdagen.se/globalassets/07.-dokument--lagar/the-instrument-of-government-2015.pdf")

myCorpus_pdf <- paste0(riks_pdf)

words_riks <- tokenize_words(myCorpus_pdf, strip_numeric = TRUE, 
                             simplify = TRUE, 
                             stopwords = c("is", "not", "one", "at","laid", "are","has","such","that","as", "which","under","the", "on", "with", "down", "other", "of", "and", "for", "to", "in", "a", "an", "by", "or", "à"))

tab_riks <- data.frame(matrix(unlist(words_riks), ncol = 1),stringsAsFactors=FALSE)


library(plyr)

frequency <- count(tab_riks)

# Descending order
frequency <- frequency[order(frequency$freq, decreasing = TRUE),]
names(frequency)[1] <- "words"

percent <- frequency %>% group_by(words) %>%
  mutate(Percentage=paste0(round(freq/sum(freq),8)))
percent <- data.frame(percent)

riks_percent <- data.frame(percent[c(1:15),])

library(ggplot2)
library(hrbrthemes)
library(RColorBrewer)

nb.cols <- 15
mycolors <- colorRampPalette(brewer.pal(8, "Greys"))(nb.cols) 

ggplot(riks_percent, aes(reorder(x=words, -freq, sum), y = freq)) + 
  theme_bw() + 
  geom_bar(aes(fill=words),stat = "identity")+
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5), legend.position = "none")+
  labs(title="",
       y = "Repetition", x = "", caption = "Source: Sveriges Riksdag",
       color = "") +
  scale_fill_grey(limits = riks_percent$words, start = 0, end = 0.8) +
  coord_flip()


### Duplication counts. Not to be removed.

head(tab_riks, 10)
tail(tab_riks, 10)

tab_riks <- arrange(tab_riks, desc(count))
tab_riks <- data.frame(tab_riks)
class(tab_riks)







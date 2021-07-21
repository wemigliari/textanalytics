# 1. Reading PDF files


library(pdftools)
riks_pdf2 <- pdf_text(pdf = "http://rkrattsdb.gov.se/SFSdoc/11/110109.PDF")

myCorpus_pdf2 <- paste0(riks_pdf2)

words_riks2 <- tokenize_words(myCorpus_pdf2, strip_numeric = TRUE, 
                             simplify = TRUE, 
                             stopwords = c("sfs", "i", "den", "på","till", "och","lika","genom","att","med", "för","lika","na", "om", "det", "en", "god", "alla", "av", "der", "eget", "sta", "tens", "eller", "som", "ett", "de", "enligt", "vid", "inte"))

tab_riks2 <- data.frame(matrix(unlist(words_riks2), ncol = 1),stringsAsFactors=FALSE)


library(plyr)

frequency2 <- count(tab_riks2)

# Descending order
frequency2 <- frequency2[order(frequency2$freq, decreasing = TRUE),]
names(frequency2)[1] <- "words"

percentR <- frequency2 %>% group_by(words) %>%
  mutate(Percentage=paste0(round(freq/sum(freq),8)))
percentR <- data.frame(percentR)

riks_percentR <- data.frame(percentR[c(1:15),])

library(ggplot2)
library(hrbrthemes)
library(RColorBrewer)

nb.cols <- 15
mycolors <- colorRampPalette(brewer.pal(8, "Greys"))(nb.cols) 

ggplot(riks_percentR, aes(reorder(x=words, -freq, sum), y = freq)) + 
  theme_bw() + 
  geom_bar(aes(fill=words),stat = "identity")+
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5), legend.position = "none")+
  labs(title="",
       y = "Repetition", x = "", caption = "Source: Sveriges Riksdag",
       color = "") +
  scale_fill_grey(limits = riks_percentR$words, start = 0, end = 0.8) +
  coord_flip()


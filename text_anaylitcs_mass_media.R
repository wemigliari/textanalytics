library(dplyr)
library(tidytext)
library(rvest)
library(pander)
library(stringr)



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
testA <- tab1 %>%
  mutate(id = case_when(
    endsWith(word, "cop26") ~ "s01",
    endsWith(word, "climate") ~ "s02",
    endsWith(word, "uk") ~ "s03",
    endsWith(word, "carbon") ~ "s04"
  ))

# Adding column based on other column:
testA <- tab1 %>%
  mutate(
    type = case_when(
      count > 10 ~ "government",
      count > 10 ~ "governance",
      count == 8 ~ "civil society",
      count == 6 | count > 3 ~ "culture",
      count == 4 ~ "values",
      count == 2 | count < 2 ~ "behaviour"
    )
  )

##########

class(testA)

test_graphA <- graph_from_data_frame(testA$type, testA$word)

########## Dagens Nyheter

test <- lapply(paste0('https://www.dn.se/om/klimatet/?offset=0', 0:9), ### https://rstudio-pubs-static.s3.amazonaws.com/287509_dde580b0adf94bae8a7994d7ae0cb849.html
               function(url){
                 url %>% read_html() %>% 
                   html_nodes("h3") %>% 
                   html_text() %>%
                   gsub('[\r\n\t]', '', .)
                 
               })

test <- unlist(test)

class(test)
test <- data.frame(test, stringsAsFactors = FALSE)

test$test <- gsub(paste0('\\b',tm::stopwords("english"), '\\b', 
                         collapse = '|'), '', test$test)

myCorpus2 <- paste(test)

library(tokenizers)

words2 <- tokenize_words(myCorpus2)
tab2 <- table(words2[[1]])
tab2 <- data_frame(word = names(tab2), count = as.numeric(tab2))
tab2

duplicated(tab2) 
tab2 <- tab2 %>% distinct() #Removing duplicates if that's the case
duplicated(tab2)

head(tab2, 10)
tail(tab2, 10)

tab2 <- arrange(tab2, desc(count))
tab2 <- data.frame(tab2)
class(tab2)

tab2 = tab2[-c(1, 2, 3, 5, 6, 8, 10, 11, 12, 15),]
tab2$newspaper <- "Dagens Nyheter"


########### Merge Tables

join_df <- rbind(tab1, tab2)
graph <- graph_from_data_frame(join_df)


###########


library(ggraph)
set.seed(123)

a <- grid::arrow(type = "closed", length = unit(.10, "inches"))

ggraph(join_df, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "gold", size = 7) +
  geom_node_text(aes(label = name), color="darkgray") +
  theme_void()


#########

ggraph(graph, layout = 'kk') + 
  geom_edge_link(aes(colour = factor(newspaper))) + 
  geom_node_point(shape=17) +
  theme_graph(base_family = "Helvetica",
              base_size = 11,
              background = "white")


ggraph(graph, layout = 'kk', maxiter = 100) + 
  geom_edge_link(aes(colour = factor(newspaper))) + 
  geom_node_point()

layout <- create_layout(graph, layout = 'drl')
ggraph(layout) + 
  geom_edge_link(aes(colour = factor(newspaper))) + 
  geom_node_point()

ggraph(graph, layout = 'linear') + 
  geom_edge_arc(aes(colour = factor(newspaper)))

ggraph(graph, layout = 'linear', circular = TRUE) + 
  geom_edge_arc(aes(colour = factor(newspaper)))

#########


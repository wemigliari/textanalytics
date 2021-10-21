library(readxl)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(hrbrthemes)

renda_julho <- read_xlsx("/Users/wemigliari/Documents/R/tabelas/pop_rua_renda_rmbh.xlsx",
                    sheet = "Total Julho")

renda_agosto <- read_xlsx("/Users/wemigliari/Documents/R/tabelas/pop_rua_renda_rmbh.xlsx",
                          sheet = "Total Agosto")

####

renda_jul_agos <- cbind(renda_julho, renda_agosto)
renda_jul_agos[c(3:12, 14:22)] <- NULL
names(renda_jul_agos)[c(2,3)] <- c("Total Julho", "Total Agosto")

renda_jul_agos <- reshape2::melt(renda_jul_agos, id.var = "Cidades_RM_BH")

colors <- c("steelblue","orange")

ggplot(renda_jul_agos, aes(x = Cidades_RM_BH, y = value/1000, colour = variable)) + 
  geom_point(alpha=1.0, shape=1, size = 5) + 
  theme_ipsum() +  
  xlab("") +
  ylab("Pessoas em Situação de Pobreza Julho/Agosto") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  geom_label_repel(aes(label = Cidades_RM_BH),
                   box.padding   = 0.35, 
                   point.padding = 0.3,
                   size = 2.0,
                   segment.size = 0.7,
                   segment.color = 'grey50',
                   show.legend=FALSE) +
  scale_color_manual(values = c("steelblue", "red"))+
  labs(x = "",
       y = "Pessoas em Situação de Pobreza Julho/Agosto (mil)",
       color = "") 
















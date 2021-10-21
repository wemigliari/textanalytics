library(readxl)
library(dplyr)
library(ggplot2)
library(zoo)
library(hrbrthemes)

igd <- read_xlsx("/Users/wemigliari/Documents/R/tabelas/pop_rua_valores_igdm.xlsx")


ggplot(igd, aes(x=igd$Data)) +
  geom_line(aes(y = igd$`Valores (R$)`, color = "Valores (R$)"), size = 1, linetype = "F1") + 
  xlab("") +
  theme_ipsum()+
  theme(plot.caption = element_text(size = 12)) +
  theme(text = element_text(size=24), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="",
       y = "Valores dos Repasses (R$)", caption = "Fonte: CadÚnico",
       color = "") +
  labs(caption = "Fonte: CadÚnico. Elaborado por Migliari, W. (2021).",
       color = "") +
  scale_color_manual(values = c("steelblue")) 



igd2 <- read_xlsx("/Users/wemigliari/Documents/R/tabelas/pop_rua_valores_igdm.xlsx",
                 sheet = "Totais Anuais")

level_order <- factor(igd2$Data, level = c('2015-12-01', '2016-12-01', '2017-12-01', '2018-12-01', '2019-12-01', '2020-12-01', '2021-12-01'))
level_order <- factor(igd2$DATA, level = c('2015-12-31', '2016-12-31', '2017-12-31', '2018-12-31', '2019-12-31', '2020-12-31', '2021-12-31'))


ggplot(igd2, aes(x=level_order, y=igd2$`Valores (R$)`)) +
  geom_bar(stat="identity", fill=c("steelblue", "steelblue", "steelblue", "steelblue", "steelblue", "steelblue", "steelblue"))+
  labs(title=" ", x =" ", y = "") +
  geom_text(aes(label=igd2$`Valores (R$)`), vjust=1.6, color="white", size=3.5)+
  theme_ipsum()+
  theme(plot.caption = element_text(size = 14)) +
  theme(text = element_text(size=24), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title="",
       y = "Valores dos Repasses (R$)", caption = "Fonte: CadÚnico",
       color = "") +
  labs(caption = "Fonte: CadÚnico. Elaborado por Migliari, W. (2021).",
       color = "")



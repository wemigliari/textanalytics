library(readxl)
library(dplyr)
library(ggplot2)
library(hrbrthemes)


### ¢¢ http://www.sthda.com/english/wiki/ggplot2-point-shapes
pop_rua_pob_zero2 <- read_xls("/Users/wemigliari/Documents/R/tabelas/pop_rua_serie_pobreza_corr.xls",
                             sheet = "Table 2")
pop_rua_pob_zero2 <- data.frame(pop_rua_pob_zero2)


ggplot(pop_rua_pob_zero2, aes(x=Período, y=Valor, group=Status)) +
  geom_point(aes(shape=Dados, color=Dados), size = 3)+
  scale_shape_manual(values=c(2, 3, 8, 5))+
  scale_color_manual(values=c("steelblue", "steelblue", 'gray', 'gray'))+
  theme_classic() +
  labs(title="",
       y = "", x = "", caption = "Fonte: CadÚnico, Dados sobre Belo Horizonte, Minas Gerais. Elaborado por Migliari, W. (2021).") +
  theme(legend.position=c(.25, .81),
        legend.key = element_rect(colour = NA, fill = NA))


### ÷÷


grafico1 <-
  ggplot(data = pop_rua_pob_zero, aes(x = Período, y = familias_situacao_rua, 
                                      fill = Dados1, shape = Dados1)) +
  geom_point(size = 3) +
  scale_shape_manual(values = c(24, 21)) +
  theme_bw() +
  scale_fill_manual(
    values = c("steelblue", "gray")) +
  labs(title="Famílias em Situação de Rua em Belo Horizonte",
       y = "", x = "") +
  theme(plot.title = element_text(size=10))


grafico2 <-
  ggplot(data = pop_rua_pob_zero, aes(x = Período, y = familias_renda_zero, 
                                      fill = Dados2, shape = Dados2)) +
  geom_point(size = 3) +
  scale_shape_manual(values = c(22, 23)) +
  theme_bw() +
  scale_fill_manual(
    values = c("steelblue", "gray")) +
  labs(title="Famílias com Renda Zero em Belo Horizonte",
       y = "", x = "", caption = "Fonte: CadÚnico, Dados sobre Belo Horizonte, Minas Gerais. Elaborado por Migliari, W. (2021).") +
  theme(plot.title = element_text(size=10))

library(cowplot)
theme_set(theme_cowplot(font_size=10))

plot_grid(grafico1, grafico2, 
          labels = c('', ''),
          label_size = 9, nrow = 2)

#### ∫∫ Variacao e Acumulado


ggplot(pop_rua_pob_zero2, aes(x=Período, y=Acumulado, group=Status)) +
  geom_line(aes(shape=Dados, color=Dados), size = 0.5)+
  scale_color_manual(values=c("orange", "steelblue", 'gray', 'black'))+
  theme_classic() +
  labs(title="", color = "Dados Acumulados (%)",
       y = "", x = "", caption = "Fonte: CadÚnico, Dados sobre Belo Horizonte, Minas Gerais. Elaborado por Migliari, W. (2021).") +
  theme(legend.position=c(.15, .81),
        legend.key = element_rect(colour = NA, fill = NA))


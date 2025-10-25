library(ggplot2)
library(RColorBrewer)
library(scales)

# lendo o arquivo CSV dentro da pasta 'dataset'
my.data <- read.csv("dataset/data_mortalidade_Regiao.csv", check.names = FALSE)

# removendo a linha de total geral
my.data <- my.data[my.data$Região != "Total", ]

# IMG 1 - grafico com as colunas na cor cinza
p2 <- ggplot(my.data, aes(x = Região, y = Total)) +
  geom_bar(stat = "identity")
ggsave("img_grafic/img1.png", p2, width = 6, height = 3, dpi = 300)

# IMG 2 - grafico com as colunas na cor laranja e borda preta
p3 <- ggplot(my.data, aes(x = Região, y = Total)) +
  geom_bar(stat = "identity", fill = "darkorange2", color = "black")
ggsave("img_grafic/img2.png", p3, width = 6, height = 3, dpi = 300)

# IMG 3 - grafico com as colunas coloridas pela paleta de cores Set3
p4 <- ggplot(my.data, aes(x = Região, y = Total, fill = Região)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_brewer(palette = "Set3")  # aplica paleta predefinida Set3
ggsave("img_grafic/img3.png", p4, width = 6, height = 3, dpi = 300)

# IMG 4 - definindo manualmente as cores de cada coluna
p5 <- ggplot(my.data, aes(x = Região, y = Total, fill = Região)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("blue", "darkolivegreen3", "plum4", "red", "yellow3"))
ggsave("img_grafic/img4.png", p5, width = 6, height = 3, dpi = 300)

# IMG 5 - ordenando as barras de forma crescente pelo valor total
p6 <- ggplot(my.data, aes(x = reorder(Região, Total), y = Total, fill = Região)) +
  geom_bar(stat = "identity", color = "black")+
  scale_fill_manual(values = c("blue", "darkolivegreen3", "plum4", "red", "yellow3"))
ggsave("img_grafic/img5.png", p6, width = 6, height = 3, dpi = 300)

# IMG 6 - adicionando rotulos e titulo ao grafico
p7 <- ggplot(my.data, aes(x = reorder(Região, Total), y = Total, fill = Região)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("blue", "darkolivegreen3", "plum4", "red", "yellow3")) +
  labs(
    x = NULL,  # removendo o rotulo do eixo X
    y = "Total de Mortes",
    title = "Mortes no Brasil por região em 2021"
  )
ggsave("img_grafic/img6.png", p7, width = 6, height = 3, dpi = 300)

# IMG 7 - aplicando tema claro, invertendo eixos e formatando valores do eixo Y
p8 <- ggplot(my.data, aes(x = reorder(Região, Total), y = Total, fill = Região)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("mediumaquamarine", "darkolivegreen3", "plum4", "salmon", "yellow3")) +
  labs(
    x = NULL,
    y = "Total de Mortes",
    title = "Mortes no Brasil por região em 2021"
  ) +
  theme_light(base_size = 12) +  # aplicando tema claro com fonte tamanho 12
  coord_flip() +  # invertendo os eixos, antes X era vertical e Y horizontal
  theme(
    panel.grid.major.y = element_blank(),  # removendo linhas horizontais principais
    panel.grid.minor.y = element_blank()   # removendo linhas horizontais secundarias
  ) +
  scale_y_continuous(
    labels = comma_format(big.mark = ".", decimal.mark = ",")
  )  # formatando os valores do eixo Y com ponto '.' e virgula ','
ggsave("img_grafic/img7.png", p8, width = 6, height = 3, dpi = 300)

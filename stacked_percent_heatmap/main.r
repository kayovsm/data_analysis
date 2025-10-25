library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(scales)

# lendo o arquivo CSV dentro da pasta 'dataset'
my.data <- read.csv("dataset/data_mortalidade_Regiao.csv", check.names = FALSE)

# removendo a linha referente ao total geral
my.data <- my.data[my.data$Região != "Total", ]

# removendo a coluna com o total geral
my.data <- my.data[, colnames(my.data) != "Total"]

# renomeando as colunas de faixa etaria para nomes mais legiveis
names(my.data) <- c("Região", "<1", "1-4", "5-9", "10-14", "15-19", "20-29",
                    "30-39", "40-49", "50-59", "60-69", "70-79", ">80", "Ignorada")

# transformando o formato dos dados para o formato longo
# agora teremos as colunas: Região, FaixaEtaria e NroMortes
my.data_long <- pivot_longer(
  my.data,
  cols = -Região,
  names_to = "FaixaEtaria",
  values_to = "NroMortes"
)

# definindo a ordem correta das faixas etarias
my.levels <- c("<1", "1-4", "5-9", "10-14", "15-19", "20-29",
               "30-39", "40-49", "50-59", "60-69", "70-79", ">80", "Ignorada")
my.data_long$FaixaEtaria <- factor(my.data_long$FaixaEtaria, levels = my.levels)

# configuraçoes padrao de tema para graficos com barras
theme_custom <- function() {
  list(
    theme_minimal(base_size = 12),
    scale_fill_manual(values = rainbow(13)),
    theme(panel.border = element_rect(color = "gray", fill = NA, linewidth = 1))
  )
}

# configuraçoes padrao de tema para heatmaps
theme_custom_heatmap <- function() {
  list(
    theme_minimal(base_size = 12),
    theme(panel.border = element_rect(color = "gray", fill = NA, linewidth = 1))
  )
}

# IMG 1 - grafico de barras agrupadas lado a lado por faixa etaria
p1 <- ggplot(my.data_long, aes(x = Região, y = NroMortes, fill = FaixaEtaria)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = "Mortes no Brasil por região em 2021",
    x = "Região",
    y = "Total de Mortes",
    fill = "Faixa Etária"
  ) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  theme_custom()

ggsave("img_grafic/img1.png", p1, width = 7, height = 4, dpi = 300)

# IMG 2 - grafico de barras empilhadas
p2 <- ggplot(my.data_long, aes(x = Região, y = NroMortes, fill = FaixaEtaria)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  labs(
    title = "Mortes no Brasil por região em 2021",
    x = "Região",
    y = "Total de Mortes",
    fill = "Faixa Etária"
  ) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  theme_custom()

ggsave("img_grafic/img2.png", p2, width = 7, height = 4, dpi = 300)

# IMG 3 - grafico de barras empilhadas em porcentagem
p3 <- ggplot(my.data_long, aes(x = Região, y = NroMortes, fill = FaixaEtaria)) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  labs(
    title = "Mortes no Brasil por região em 2021",
    x = "Região",
    y = "Proporção",
    fill = "Faixa Etária"
  ) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01, decimal.mark = ",")) +
  theme_custom()

ggsave("img_grafic/img3.png", p3, width = 7, height = 4, dpi = 300)

# IMG 4 - barras empilhadas percentuais com regioes no eixo Y
p4 <- ggplot(my.data_long, aes(y = Região, x = NroMortes, fill = FaixaEtaria)) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  geom_vline(xintercept = seq(0.1, 0.9, by = 0.1), color = "white", linewidth = 0.5) +
  labs(
    title = "Mortes no Brasil por região em 2021",
    x = "Proporção",
    y = "Região",
    fill = "Faixa Etária"
  ) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01, decimal.mark = ",")) +
  theme_custom()

ggsave("img_grafic/img4.png", p4, width = 7, height = 4, dpi = 300)

# IMG 5 - heatmap simples com geom_tile()
p5 <- ggplot(my.data_long, aes(x = Região, y = FaixaEtaria, fill = NroMortes)) +
  geom_tile() +
  geom_vline(xintercept = seq(0.5, 4.5, by = 1), color = "white", linewidth = 0.3) +
  geom_hline(yintercept = seq(0.5, 13.5, by = 1), color = "white", linewidth = 0.3) +
  scale_fill_distiller(palette = "Spectral") +
  labs(
    title = "Mortes no Brasil por região em 2021",
    x = "Região",
    y = "Faixa Etária",
    fill = "Nº de Mortes"
  ) +
  theme_custom_heatmap()

ggsave("img_grafic/img5.png", p5, width = 7, height = 4, dpi = 300)

# IMG 6 - heatmap com bordas e rotulos numericos
p6 <- ggplot(my.data_long, aes(x = Região, y = FaixaEtaria, fill = NroMortes)) +
  geom_tile(color = "black") +
  geom_text(aes(label = NroMortes), size = 3) +
  scale_fill_distiller(palette = "Spectral") +
  labs(
    title = "Mortes no Brasil por região em 2021",
    x = "Região",
    y = "Faixa Etária",
    fill = "Nº de Mortes"
  ) +
  theme_custom_heatmap()

ggsave("img_grafic/img6.png", p6, width = 7, height = 4, dpi = 300)

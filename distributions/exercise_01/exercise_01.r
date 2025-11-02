library(ggplot2)
library(tidyverse)

# histograma de Sepal.Length por especie
p1_hist <- ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
  geom_histogram(bins = 20, color = "black", alpha = 0.7) +
  labs(
    title = "Distribuição de Comprimento da Sépala",
    x = "Comprimento da Sépala (cm)",
    y = "Frequência",
    fill = "Espécie"
  ) +
  theme_minimal(base_size = 12) +
  facet_wrap(~Species) +
  theme(panel.border = element_rect(color = "gray", fill = NA, linewidth = 1))

print(p1_hist)
ggsave("ex1_histogram.png", p1_hist, width = 7, height = 4, dpi = 300)

# boxplot de Sepal.Length por especie
p1_box <- ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot(color = "black", alpha = 0.7) +
  labs(
    title = "Boxplot de Comprimento da Sépala por Espécie",
    x = "Espécie",
    y = "Comprimento da Sépala (cm)",
    fill = "Espécie"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.border = element_rect(color = "gray", fill = NA, linewidth = 1))

print(p1_box)
ggsave("ex1_boxplot.png", p1_box, width = 7, height = 4, dpi = 300)

# violin plot de Sepal.Length por especie com pontos sobrepostos
p1_violin <- ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violin(color = "black", alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, color = "black") +
  labs(
    title = "Violin Plot de Comprimento da Sépala por Espécie",
    x = "Espécie",
    y = "Comprimento da Sépala (cm)",
    fill = "Espécie"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.border = element_rect(color = "gray", fill = NA, linewidth = 1))

print(p1_violin)
ggsave("ex1_violin.png", p1_violin, width = 7, height = 4, dpi = 300)

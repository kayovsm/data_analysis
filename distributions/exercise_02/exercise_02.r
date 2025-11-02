library(ggplot2)
library(tidyverse)

# transformar os dados para formato longo
# normalizar com scale() para melhor visualizacao
mtcars_scaled <- mtcars %>%
  select(mpg, hp, wt) %>%
  mutate(
    mpg_scaled = scale(mpg)[, 1],
    hp_scaled = scale(hp)[, 1],
    wt_scaled = scale(wt)[, 1]
  ) %>%
  pivot_longer(
    cols = ends_with("_scaled"),
    names_to = "variable",
    values_to = "value",
    names_pattern = "(.+)_scaled"
  )

# boxplot com pontos e transparencia dos dados normalizados
p2_box <- ggplot(mtcars_scaled, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(alpha = 0.6, color = "black") +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black", size = 2) +
  labs(
    title = "Exploração de MPG, HP e WT",
    x = "Variáveis",
    y = "Valores Normalizados",
    fill = "Variável"
  ) +
  theme_minimal(base_size = 12) +
  scale_x_discrete(labels = c("mpg" = "Miles per Gallon", 
                              "hp" = "Horsepower", 
                              "wt" = "Weight")) +
  theme(panel.border = element_rect(color = "gray", fill = NA, linewidth = 1))

print(p2_box)
ggsave("ex2_boxplot_scaled.png", p2_box, width = 7, height = 4, dpi = 300)

# versao alternativa: violin plot com pontos
p2_violin <- ggplot(mtcars_scaled, aes(x = variable, y = value, fill = variable)) +
  geom_violin(alpha = 0.6, color = "black") +
  geom_jitter(width = 0.15, alpha = 0.5, color = "black", size = 2) +
  labs(
    title = "Distribuição de MPG, HP e WT",
    x = "Variáveis",
    y = "Valores Normalizados",
    fill = "Variável"
  ) +
  theme_minimal(base_size = 12) +
  scale_x_discrete(labels = c("mpg" = "Miles per Gallon", 
                              "hp" = "Horsepower", 
                              "wt" = "Weight")) +
  theme(panel.border = element_rect(color = "gray", fill = NA, linewidth = 1))

print(p2_violin)
ggsave("ex2_violin_scaled.png", p2_violin, width = 7, height = 4, dpi = 300)

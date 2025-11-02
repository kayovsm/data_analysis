library(ggplot2)
library(tidyverse)

# separar dados por suplemento para criar distribuicao em espelho
tooth_oj <- ToothGrowth %>% filter(supp == "OJ") %>% mutate(supp_group = "OJ")
tooth_vc <- ToothGrowth %>% filter(supp == "VC") %>% mutate(supp_group = "VC")

# histograma em espelho (uma distribuicao para cima, outra para baixo)
p3_mirror <- ggplot() +
  geom_histogram(
    data = tooth_oj,
    aes(x = len, fill = supp_group, y = after_stat(density)),
    bins = 15,
    color = "black",
    alpha = 0.7
  ) +
  geom_histogram(
    data = tooth_vc,
    aes(x = len, fill = supp_group, y = -after_stat(density)),
    bins = 15,
    color = "black",
    alpha = 0.7
  ) +
  labs(
    title = "Comparação de Comprimento de Dentes por Tipo de Suplemento",
    x = "Comprimento do Dente",
    y = "Densidade",
    fill = "Suplemento"
  ) +
  theme_minimal(base_size = 12) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  theme(panel.border = element_rect(color = "gray", fill = NA, linewidth = 1))

print(p3_mirror)
ggsave("ex3_mirror_histogram.png", p3_mirror, width = 7, height = 4, dpi = 300)

# versao alternativa com density plots em facet
p3_density <- ggplot(ToothGrowth, aes(x = len, fill = supp)) +
  geom_density(alpha = 0.6, color = "black") +
  facet_wrap(~supp, ncol = 1, scales = "free") +
  labs(
    title = "Comparação de Comprimento de Dentes por Tipo de Suplemento",
    x = "Comprimento do Dente",
    y = "Densidade",
    fill = "Suplemento"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.border = element_rect(color = "gray", fill = NA, linewidth = 1))

print(p3_density)
ggsave("ex3_density_faceted.png", p3_density, width = 7, height = 4, dpi = 300)

# boxplot lado a lado para comparacao dos suplementos
p3_box <- ggplot(ToothGrowth, aes(x = supp, y = len, fill = supp)) +
  geom_boxplot(color = "black", alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, color = "black") +
  labs(
    title = "Boxplot de Comprimento de Dentes por Suplemento",
    x = "Tipo de Suplemento",
    y = "Comprimento do Dente",
    fill = "Suplemento"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.border = element_rect(color = "gray", fill = NA, linewidth = 1))

print(p3_box)
ggsave("ex3_boxplot.png", p3_box, width = 7, height = 4, dpi = 300)

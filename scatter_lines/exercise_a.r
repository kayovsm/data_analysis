library(ggplot2)

# criar pasta de saida para figuras
dir_out <- "img"
if (!dir.exists(dir_out)) {
  dir.create(dir_out, recursive = TRUE)
}

# definir semente para reprodutibilidade
set.seed(123)

# criar conjunto de dados
n <- 100
x <- 1:n
mu <- 0
sigma <- 0.2
y <- log(x) + rnorm(n, mean = mu, sd = sigma)

df_scatter <- data.frame(x = x, y = y)

plot_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_blank(),
    plot.caption = element_blank(),
    legend.position = "bottom",
    axis.title = element_text(size = 12, face = "plain"),
    axis.text = element_text(size = 12, face = "plain"),
    panel.background = element_rect(fill = "white", colour = "grey80"),
    panel.border = element_rect(fill = NA, colour = "grey80")
  )

# scatter plot basico
p_scatter <- ggplot(df_scatter, aes(x = x, y = y)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  labs(
    title = "Scatter de log(x) com ruído normal",
    x = "Amostra",
    y = "log(x) com ruído"
  ) +
  plot_theme

# adicionar tendencia linear
p_linear <- p_scatter +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "Trend linear estimada",
    x = "Amostra",
    y = "Estimativa ajuste linear"
  )

# adicionar tendencia loess
p_loess <- p_scatter +
  geom_smooth(method = "loess", se = TRUE, color = "darkgreen") +
  labs(
    title = "Tendência LOESS padrão",
    x = "Amostra",
    y = "Estimativa suavizada (LOESS)"
  )

# loess com span = 0.2
p_loess_span <- p_scatter +
  geom_smooth(method = "loess", se = TRUE, span = 0.2, color = "purple") +
  labs(
    title = "LOESS com span = 0.2",
    x = "Amostra",
    y = "Estimativa suavizada (LOESS, span = 0.2)"
  )

# scatter basico
ggsave(file.path(dir_out, "scatter_basico.png"),
  plot = p_scatter, width = 6, height = 4, dpi = 300)

# scatter com tendencia linear
ggsave(file.path(dir_out, "scatter_linear.png"),
  plot = p_linear, width = 6, height = 4, dpi = 300)

# scatter com loess
ggsave(file.path(dir_out, "scatter_loess.png"),
  plot = p_loess, width = 6, height = 4, dpi = 300)

# scatter com loess span 0.2
ggsave(file.path(dir_out, "scatter_loess_span.png"),
  plot = p_loess_span, width = 6, height = 4, dpi = 300)
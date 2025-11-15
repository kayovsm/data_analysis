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
df_line <- data.frame(
  x = x,
  value = sin(x / 5) + rnorm(n, 0, 0.1)
)

plot_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_blank(),
    plot.caption = element_blank(),
    axis.title = element_text(size = 12, face = "plain"),
    axis.text = element_text(size = 12, face = "plain"),
    panel.background = element_rect(fill = "white", colour = "grey80"),
    panel.border = element_rect(fill = NA, colour = "grey80")
  )

# grafico de linha
p_line <- ggplot(df_line, aes(x = x, y = value)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", alpha = 0.6) +
  labs(
    title = "Senoide com ruído gaussiano",
    x = "Amostra",
    y = "Amplitude com ruído"
  ) +
  plot_theme

# salvar grafico
ggsave(file.path(dir_out, "line_plot.png"),
  plot = p_line, width = 6, height = 4, dpi = 300)
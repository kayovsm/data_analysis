library(ggplot2)

# criar pasta de saida para figuras
dir_out <- "img"
if (!dir.exists(dir_out)) {
  dir.create(dir_out, recursive = TRUE)
}

# definir semente para reprodutibilidade
set.seed(123)

# criar conjunto de dados
data_inicial <- as.Date("2025-01-01")
n <- 100
datas <- data_inicial + 0:(n - 1)

df_ts <- data.frame(
  data = datas,
  valor = cumsum(rnorm(n, mean = 0.1, sd = 0.5))
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

# grafico de serie temporal
p_time <- ggplot(df_ts, aes(x = data, y = valor)) +
  geom_line(color = "darkorange", linewidth = 1) +
  labs(
    title = "Série temporal acumulada",
    x = "Data",
    y = "Soma cumulativa de incrementos"
  ) +
  plot_theme

# salvar grafico
ggsave(file.path(dir_out, "serie_temporal.png"),
  plot = p_time, width = 6, height = 4, dpi = 300)
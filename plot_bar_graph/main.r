
.libPaths("~/R/library")

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# ler dados
basketball_raw <- read_csv("basketball/Basketball.csv", show_col_types = FALSE)

head(basketball_raw)
glimpse(basketball_raw)

# pivota jogadores male e female para linhas
basketball_tidy <- basketball_raw %>%
	pivot_longer(cols = c(male.player, female.player),
							 names_to = "gender_col",
							 values_to = "player") %>%
	mutate(gender = case_when(
		gender_col == "male.player" ~ "male",
		gender_col == "female.player" ~ "female",
		TRUE ~ as.character(gender_col)
	)) %>%
	select(player, gender, year, stat, amount)

glimpse(basketball_tidy)

# imprimi resultado do pivot_longer()
cat('=== Resultado: pivot_longer() (basketball_tidy) ===\n')
print(basketball_tidy)

# cria coluna pair e separar em male e female
pair_df <- basketball_raw %>%
  mutate(pair = paste(male.player, female.player, sep = ":")) %>%
  select(pair, year, stat, amount)

pair_separated <- pair_df %>%
  tidyr::separate(col = pair, into = c("male", "female"), sep = ":", remove = FALSE)

cat('=== Resultado: separate_wider_delim() (pair_separated) ===\n')
print(pair_separated)

# separa stat em colunas points e assists
wider_df <- basketball_raw %>%
  tidyr::pivot_wider(names_from = stat, values_from = amount)

cat('=== Resultado: pivot_wider() ===\n')
print(wider_df)

# remove a coluna points
if ("points" %in% colnames(wider_df)) {
  wider_df_no_points <- wider_df %>% dplyr::select(-points)
} else {
  wider_df_no_points <- wider_df
}
cat('=== Resultado: pivot_wider() sem points ===\n')
print(wider_df_no_points)

# filtra apenas a assists
assists <- basketball_tidy %>% filter(tolower(stat) == "assists")

# calcula a media de assists por gender e year
assists_summary <- assists %>%
	group_by(gender, year) %>%
	summarise(mean_assists = mean(amount, na.rm = TRUE), .groups = "drop")

print(assists_summary)

# grafico de barras agrupadas
max_y <- ceiling(max(assists_summary$mean_assists, na.rm = TRUE) / 2.5) * 2.5

p <- ggplot(assists_summary, aes(x = gender, y = mean_assists, fill = factor(year))) +
		geom_col(position = position_dodge(width = 0.8), width = 0.8) +
	scale_fill_manual(name = "Year", values = c("1" = "#FF6F61", "2" = "#06BCC1")) +
		scale_y_continuous(
			breaks = seq(0, max_y, by = 2.5),
			limits = c(0, max_y),
			labels = function(x) sprintf("%.1f -", x)
		) +
		labs(x = "Gender", y = "Assists") +
		theme_minimal(base_size = 14) +
		theme(
					panel.grid.major.x = element_blank(),
					panel.grid.major.y = element_line(color = "white"),
					panel.minor.y = element_line(color = "white"),
					panel.background = element_rect(fill = "#efefef", color = NA),
					plot.background = element_rect(fill = "white", color = NA),
					plot.margin = margin(t = 20, r = 40, b = 20, l = 40),
					panel.spacing = unit(1, "lines"),
					legend.position = "right"
		)

p <- p + geom_vline(xintercept = c(1, 2), colour = "white", linewidth = 1.2)

library(grid)
g <- ggplotGrob(p)
png("basketball_assists.png", width = 8 * 150, height = 6 * 150, res = 150)
grid.newpage()
grid.draw(g)
dev.off()

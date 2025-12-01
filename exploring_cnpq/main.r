suppressPackageStartupMessages({
	library(tidyverse)
	library(sf)
	library(treemapify)
	library(scales)
	library(geobr)
})

get_script_dir <- function() {
	args <- commandArgs(trailingOnly = FALSE)
	file_arg <- grep("^--file=", args, value = TRUE)
	if (length(file_arg)) {
		normalizePath(dirname(sub("^--file=", "", file_arg)[1]))
	} else {
		getwd()
	}
}

script_dir <- get_script_dir()
data_path <- file.path(script_dir, "dataset", "Universal2023_prelim.csv")
plot_dir <- file.path(script_dir, "plots")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

raw_data <- read_csv(data_path, show_col_types = FALSE)

data_clean <- raw_data %>%
	mutate(across(c(Custeio.Aprov, Capital.Aprov, Bolsas.Aprov, Total.Aprovado), as.double))

save_plot <- function(plot, name) {
	ggsave(file.path(plot_dir, name), plot = plot, width = 10, height = 8, dpi = 300)
}

# GRAFICO 1: distribuiçao empilhada por regiao
# agrupa valores por regiao e tipo de despesa (custeio, capital, bolsas)
region_long <- data_clean %>%
	group_by(Região) %>%
	summarise(across(c(Custeio.Aprov, Capital.Aprov, Bolsas.Aprov), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
	pivot_longer(cols = Custeio.Aprov:Bolsas.Aprov, names_to = "tipo", values_to = "valor") %>%
	mutate(
		tipo = recode(tipo,
			Custeio.Aprov = "Custeio",
			Capital.Aprov = "Capital",
			Bolsas.Aprov = "Bolsas"
		),
		tipo = factor(tipo, levels = c("Custeio", "Capital", "Bolsas"))
	)

## Mapear abreviações de Região para o nome completo (ex.: NO -> Norte)
region_long <- region_long %>%
    mutate(Região = as.character(Região)) %>%
    mutate(Região = dplyr::recode(Região,
                                  NO = "Norte",
                                  NE = "Nordeste",
                                  SE = "Sudeste",
                                  SU = "Sul",
                                  CO = "Centro-Oeste",
                                  .default = Região)) %>%
    mutate(Região = factor(Região, levels = c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul")))

# monta e salva o grafico de distribuiçao por regiao
plot_region <- ggplot(region_long, aes(x = Região, y = valor, fill = tipo)) +
	geom_col(position = "stack") +
	scale_fill_brewer(palette = "Set2") +
	scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
	labs(
		title = "Distribuição dos tipos de despesa por região",
		y = "Total aprovado (R$)",
		fill = "Tipo",
		caption = "Fonte: Universal CNPq 2023"
	) +
	theme_minimal() +
	# adicionar borda cinza ao redor do plot
	theme(
		plot.background = element_rect(fill = "white", color = "#bdbdbd", linewidth = 0.6),
		panel.border = element_rect(color = "#bdbdbd", fill = NA, linewidth = 0.6)
	)

# salvar GRAFICO 1
save_plot(plot_region, "regiao_despesas_empilhadas.png")

# GRAFICO 2: mapa de total aprovado por UF

# agrupa valores por UF
uf_stats <- data_clean %>%
	group_by(UF) %>%
	summarise(
		projetos = n(),
		total = sum(Total.Aprovado, na.rm = TRUE),
		media = mean(Total.Aprovado, na.rm = TRUE),
		.groups = "drop"
	) %>%
	mutate(UF = toupper(UF))

states <- geobr::read_state(year = 2020)
map_data <- states %>%
	mutate(abbrev_state = toupper(abbrev_state)) %>%
	left_join(uf_stats, by = c("abbrev_state" = "UF"))

# calcular pontos para rotulos
centroids <- sf::st_point_on_surface(map_data)
cent_coords <- sf::st_coordinates(centroids)
map_labels <- map_data %>%
	sf::st_drop_geometry() %>%
	mutate(X = cent_coords[, 1], Y = cent_coords[, 2])

# criar bins discretos para total aprovado
breaks_bins <- c(0,
				 1000000,
				 5000000,
				 10000000,
				 20000000,
				 30000000,
				 40000000,
				 Inf)
labels_bins <- c(
	"0 - 1.000.000",
	"1.000.000 - 5.000.000",
	"5.000.000 - 10.000.000",
	"10.000.000 - 20.000.000",
	"20.000.000 - 30.000.000",
	"30.000.000 - 40.000.000",
	"> 40.000.000"
)

# atribuir bin aos dados do mapa
map_data <- map_data %>%
	mutate(total_num = as.numeric(total)) %>%
	mutate(total_bin = cut(total_num,
						   breaks = breaks_bins,
						   include.lowest = TRUE,
						   right = FALSE,
						   labels = labels_bins))

# definir paleta de cores
pal <- rev(viridisLite::plasma(length(labels_bins)))
names(pal) <- labels_bins

# monta o mapa por UF usando bins discretos para faixas de total aprovado
plot_map <- ggplot(map_data) +
	geom_sf(aes(fill = total_bin), color = "white", size = 0.15) +
	geom_text(data = map_labels, aes(x = X, y = Y, label = abbrev_state),
			  size = 3, color = "black", fontface = "bold") +
	scale_fill_manual(
		values = pal,
		na.value = "gray95",
		drop = FALSE
	) +
	labs(
		title = "Recursos aprovados por UF",
		fill = "Total aprovado (R$)",
		caption = "Fonte: Universal CNPq 2023"
	) +
	theme_minimal() +
	theme(
		panel.grid = element_blank(),
		panel.background = element_rect(fill = "#f5f5f5", color = NA),
		plot.background = element_rect(fill = "white", color = NA),
		panel.border = element_rect(color = "#d0d0d0", fill = NA, linewidth = 0.5),
		axis.title = element_blank(),
		axis.text = element_blank(),
		axis.ticks = element_blank(),
		plot.caption = element_text(hjust = 0.95, size = 8)
	)
# salvar GRAFICO 2
save_plot(plot_map, "mapa_uf_total.png")

# GRAFICO 3: mapa mostrando a instituição com mais projetos por UF

# instituiçao com mais projetos por UF
uf_top_inst <- data_clean %>%
	filter(!is.na(UF) & !is.na(Instituição) & Instituição != "") %>%
	mutate(UF = toupper(UF)) %>%
	group_by(UF, Instituição) %>%
	summarise(projetos = n(), .groups = "drop") %>%
	group_by(UF) %>%
	slice_max(order_by = projetos, n = 1, with_ties = FALSE) %>%
	ungroup()

states_top_inst <- geobr::read_state(year = 2020) %>%
	mutate(abbrev_state = toupper(abbrev_state)) %>%
	left_join(uf_top_inst, by = c("abbrev_state" = "UF"))

# pontos para rotulos
cent_top <- sf::st_point_on_surface(states_top_inst)
cent_coords_top <- sf::st_coordinates(cent_top)
labels_top <- states_top_inst %>% sf::st_drop_geometry() %>%
	mutate(X = cent_coords_top[,1], Y = cent_coords_top[,2])

max_proj_val <- max(c(0, states_top_inst$projetos), na.rm = TRUE)

# monta o mapa onde cada UF é colorida pelo nº de projetos da instituiçao lider
plot_map_top_inst <- ggplot(states_top_inst) +
	geom_sf(aes(fill = projetos), color = "white", size = 0.15) +
	geom_text(data = labels_top, aes(x = X, y = Y, label = ifelse(is.na(Instituição), "-", stringr::str_trunc(Instituição, 20))),
						size = 3, color = "black") +
	scale_fill_viridis_c(
		option = "plasma",
		direction = -1,
		begin = 0.15,
		end = 0.95,
		limits = c(0, max_proj_val),
		breaks = scales::pretty_breaks(n = 5),
		na.value = "gray95",
		labels = label_number(big.mark = ".", decimal.mark = ",")
	) +
	labs(
		title = "Instituição com mais projetos aprovados por UF",
		fill = "Nº de projetos",
		caption = "Fonte: Universal CNPq 2023"
	) +
	theme_minimal() +
	theme(
		panel.grid = element_blank(),
		panel.background = element_rect(fill = "#f5f5f5", color = NA),
		plot.background = element_rect(fill = "white", color = NA),
		panel.border = element_rect(color = "#d0d0d0", fill = NA, linewidth = 0.5),
		axis.title = element_blank(),
		axis.text = element_blank(),
		axis.ticks = element_blank(),
		plot.caption = element_text(hjust = 0.95, size = 8)
	)
# salvar GRAFICO 3
save_plot(plot_map_top_inst, "mapa_top_instituicao_uf.png")

# GRAFICO 4: top 10 instituicoes com maior proporçao de projetos nao aprovados
sugest_summary <- raw_data %>%
	filter(!is.na(Instituição) & Instituição != "") %>%
	mutate(nao_aprovado = `Aprovação` == "Não Aprovada") %>%
	group_by(Instituição) %>%
	summarise(
		total = n(),
		nao_aprovados = sum(nao_aprovado, na.rm = TRUE),
		proporcao_nao_aprovados = ifelse(total > 0, nao_aprovados / total, NA_real_),
		.groups = "drop"
	) %>%
	arrange(desc(proporcao_nao_aprovados), desc(nao_aprovados))

# preparar top10 com filtro minimo de 20 projetos
top10_nr <- sugest_summary %>% filter(total >= 20) %>% slice_head(n = 10) %>% mutate(Instituição = forcats::fct_reorder(Instituição, proporcao_nao_aprovados))

# exibir em colunas com rotulos em porcentagem
top10_nr <- top10_nr %>% mutate(Instituição = forcats::fct_reorder(Instituição, proporcao_nao_aprovados, .desc = TRUE))

plot_nr <- ggplot(top10_nr, aes(x = Instituição, y = proporcao_nao_aprovados)) +
	geom_col(fill = "#c0392b") +
	geom_text(aes(label = paste0(formatC(proporcao_nao_aprovados * 100, format = "f", digits = 1, big.mark = ".", decimal.mark = ","), "%")),
			  vjust = -0.5, size = 3) +
	scale_y_continuous(labels = function(x) paste0(formatC(x*100, format = "f", digits = 0, big.mark = ".", decimal.mark = ","), "%"), limits = c(0, 1.0), expand = c(0,0)) +
	labs(title = "Top 10 instituições com taxa de projetos não aprovados",
		 x = "Instituição",
		 y = "Proporção de não aprovados",
		 caption = "Fonte: Universal CNPq 2023") +
	theme_minimal() +
	theme(
		plot.caption = element_text(hjust = 0.95),
		axis.text.x = element_text(angle = 45, hjust = 1),
		plot.background = element_rect(fill = "white", color = "#bdbdbd", linewidth = 0.6),
		panel.border = element_rect(color = "#bdbdbd", fill = NA, linewidth = 0.6)
	)

# salvar GRAFICO 4
save_plot(plot_nr, "top10_instituicoes_nao_aprovadas.png")


suppressPackageStartupMessages({
	library(sf)
	library(geobr)
	library(dplyr)
	library(ggplot2)
	library(viridis)
	library(readr)
})

# caminho dos dados
args <- commandArgs(trailingOnly = FALSE)
script_path <- dirname(sub("--file=", "", args[grep("--file=", args)]))
if (length(script_path) == 0 || script_path == "") script_path <- getwd()

# buscando dados no arquivo "campeoes_estado.csv"
data_path <- file.path(script_path, "dataset", "campeoes_estado.csv")
campeoes <- read.csv(data_path, stringsAsFactors = FALSE, encoding = "UTF-8")

campeoes_state <- campeoes %>%
	mutate(Estado = toupper(Estado),
				 Titulos = as.numeric(Titulos)) %>%
	group_by(Estado) %>%
	summarise(Titulos = sum(Titulos, na.rm = TRUE)) %>%
	ungroup()

# ler geometria dos estados (geobr)
states <- read_state(year = 2019, showProgress = FALSE)

# codigo da UF dos estados 
states$abbrev_state <- toupper(states$abbrev_state)
	
# juntar dados agregados com a geometria dos estados
states <- left_join(states, campeoes_state, by = c("abbrev_state" = "Estado"))
states$Titulos[is.na(states$Titulos)] <- 0

# gerar mapa
p <- ggplot() +
		geom_sf(data = states, aes(fill = Titulos), color = "black", size = 0.15) +
		geom_sf_text(data = states, aes(label = abbrev_state), size = 4, check_overlap = TRUE) +
		# escala automatica baseada nos dados
		scale_fill_viridis_c(
			option = "plasma",
			direction = -1,
			name = "Nro de Títulos",
			begin = 0.15,
			end = 0.95,
			breaks = scales::pretty_breaks(n = 5)
		) +
		labs(title = "Campeonatos Brasileiros por Estado",
				 subtitle = "Soma dos títulos por clubes do estado",
				 caption = "Fonte: arquivo 'campeoes_estado.csv'") +
		theme_minimal() +
		theme(
			panel.grid = element_blank(),
				# fundo e borda cinza apenas no painel do mapa
				panel.background = element_rect(fill = "#f5f5f5", color = NA),
				plot.background = element_rect(fill = "white", color = NA),
				panel.border = element_rect(color = "#d0d0d0", fill = NA, linewidth = 0.5),
			# titulos
			axis.title = element_blank(),
			axis.text = element_blank(),
			axis.ticks = element_blank()
		)

# salvar mapa
out_file <- "maps/campeoes_estado.png"
ggsave(out_file, p, width = 10, height = 8, dpi = 300)
message("Mapa salvo em: ", normalizePath(out_file))


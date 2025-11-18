library(ggplot2)
library(dplyr)
library(readr)
library(glue)
library(lubridate)
library(viridis)
library(zoo)
library(tidyr)

# dataset_path <- "covid_graph/dataset/owid-covid-data (1).csv"
output_root <- "covid_graph/img"
ensure_dir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE)
ensure_dir(output_root)

focus_countries <- c("Argentina", "Colombia", "Chile", "Spain", "Italy", "Netherlands")
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2023-01-31")

data <- read_csv(dataset_path, show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  filter(location %in% focus_countries, date >= start_date, date <= end_date) %>%
  mutate(continent = case_when(
    location %in% c("Argentina", "Colombia", "Chile") ~ "América",
    TRUE ~ "Europa"
  ))

visible_start <- start_date
max_date <- max(data$date, na.rm = TRUE)
date_breaks_seq <- seq(visible_start, max_date, by = "2 months")
date_breaks_seq_3m <- seq(visible_start, max_date, by = "3 months")

# Função para normalizar dados para uma escala de 0 a 1
scale_to_01 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

summarise_metric <- function(column) {
  data %>%
    filter(!is.na(.data[[column]])) %>%
    group_by(continent, date) %>%
    summarise(value = mean(.data[[column]], na.rm = TRUE), .groups = "drop")
}

# ============================================================================
# GRÁFICO 1: TENDÊNCIA DE NOVOS CASOS
# ============================================================================

cases_df <- summarise_metric("new_cases_smoothed_per_million") %>%
  filter(date >= visible_start) %>%
  group_by(continent) %>%
  arrange(date) %>%
  mutate(trend = rollmean(value, 14, fill = NA, align = "right")) %>%
  ungroup()

cases_plot <- cases_df %>%
  ggplot() +
  geom_line(aes(date, value, color = continent), linewidth = 1.0, alpha = 0.9) +
  scale_color_viridis_d(name = "Continente", option = "D") +
  scale_x_date(limits = c(visible_start, max_date), breaks = date_breaks_seq, expand = c(0, 0), date_labels = "%b %Y") +
  labs(
    title = "Tendência de Novos Casos por Continente",
    caption = "Fonte: Our World in Data (OWID)",
    x = NULL,
    y = "Casos/milhão (média)",
    color = "Continente"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5)
  )

# ============================================================================
# GRÁFICO 2: COMPARATIVO DE MORTES SEMANAIS
# ============================================================================

week_cut <- floor_date(visible_start, "week")
deaths_weekly <- summarise_metric("new_deaths_smoothed_per_million") %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(continent, week) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  filter(week >= week_cut) %>%
  group_by(continent) %>%
  arrange(week) %>%
  mutate(trend = rollmean(value, 5, fill = NA, align = "center")) %>%
  ungroup()

deaths_plot <- deaths_weekly %>%
  ggplot(aes(week, value, fill = continent)) +
  geom_col(position = position_dodge(width = 7), width = 6, alpha = 0.85) +
  scale_fill_viridis_d(name = "Continente", option = "D") +
  scale_color_viridis_d(name = "Continente", option = "D") +
  scale_y_continuous(limits = c(0, 25), expand = c(0, 0)) +
  scale_x_date(limits = c(visible_start, max_date), breaks = date_breaks_seq, expand = c(0, 0), date_labels = "%b %Y") +
  labs(
    title = "Comparativo de Mortes Semanais",
    caption = "Fonte: Our World in Data (OWID)",
    x = "Semana",
    y = "Mortes/milhão (semanal)",
    fill = "Continente",
    color = "Continente"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5)
  )

# ============================================================================
# GRÁFICO 3: AVANÇO DA VACINAÇÃO COMPLETA
# ============================================================================

month_cut <- visible_start
vacc_monthly <- data %>%
  filter(!is.na(people_fully_vaccinated_per_hundred)) %>%
  filter(date >= month_cut) %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(continent, month) %>%
  summarise(value = mean(people_fully_vaccinated_per_hundred, na.rm = TRUE), .groups = "drop")

vacc_plot <- vacc_monthly %>%
  ggplot(aes(month, value, color = continent)) +
  geom_step(direction = "hv", linewidth = 1.0) +
  geom_point(data = vacc_monthly %>% group_by(continent) %>% slice_tail(n = 1) %>% ungroup(),
    aes(month, value), size = 3
  ) +
  scale_color_viridis_d(option = "D") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  scale_x_date(limits = c(visible_start, max_date), breaks = date_breaks_seq, expand = c(0, 0), date_labels = "%b %Y") +
  labs(
    title = "Avanço da Vacinação Completa",
    x = NULL,
    y = "% Vacinados (total)",
    color = "Continente"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5)
  )

# ============================================================================
# GRÁFICO 4: NÍVEL DE RESTRIÇÕES GOVERNAMENTAIS
# ============================================================================

stringency_df <- summarise_metric("stringency_index") %>%
  filter(date >= visible_start)

stringency_plot <- stringency_df %>%
  ggplot(aes(date, value, color = continent)) +
  geom_line(linewidth = 1.0) +
  geom_point(data = stringency_df %>% group_by(continent) %>% slice_tail(n = 1) %>% ungroup(),
             aes(date, value, color = continent), size = 2) +
  scale_color_viridis_d(name = "Continente", option = "D") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  scale_x_date(limits = c(visible_start, max_date), breaks = date_breaks_seq, expand = c(0, 0), date_labels = "%b %Y") +
  labs(
    title = "Nível de Restrições Governamentais",
    caption = "Fonte: Our World in Data (OWID)",
    x = NULL,
    y = "Índice de Restrição",
    color = "Continente"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5)
  )

# ============================================================================
# GRÁFICO 5: NOVOS CASOS vs RESTRIÇÕES (NORMALIZADO)
# ============================================================================

cases_df_comp <- data %>%
  group_by(continent, date) %>%
  summarise(cases_value = mean(new_cases_smoothed_per_million, na.rm = TRUE), .groups = "drop")

stringency_df_comp <- data %>%
  group_by(continent, date) %>%
  summarise(stringency_value = mean(stringency_index, na.rm = TRUE), .groups = "drop")

cases_vs_stringency <- cases_df_comp %>%
  left_join(stringency_df_comp, by = c("continent", "date")) %>%
  filter(!is.na(cases_value) & !is.na(stringency_value)) %>%
  group_by(continent) %>%
  mutate(
    cases_scaled = (cases_value - min(cases_value, na.rm = TRUE)) / (max(cases_value, na.rm = TRUE) - min(cases_value, na.rm = TRUE)) * 100,
    stringency_scaled = stringency_value
  ) %>%
  ungroup()

p_cases_vs_restrictions <- cases_vs_stringency %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = cases_scaled, color = "Novos Casos"), linewidth = 1.2, alpha = 0.8) +
  geom_line(aes(y = stringency_scaled, color = "Índice de Restrição"), linewidth = 1.2, alpha = 0.8) +
  facet_wrap(~continent, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("Novos Casos" = "#E69F00", "Índice de Restrição" = "#009E73")) +
  scale_x_date(limits = c(as.Date("2020-01-01"), max_date), breaks = date_breaks_seq_3m, date_labels = "%b %Y") +
  scale_y_continuous(
    name = "Escala Normalizada (0-100)",
    limits = c(0, 100)
  ) +
  labs(
    title = "Novos Casos vs. Restrições Governamentais",
    subtitle = "Ambos normalizados em escala 0-100 para comparação direta de tendências.",
    caption = "Fonte: Our World in Data (OWID)",
    x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5)
  )

# ============================================================================
# GRÁFICO 6: MORTES vs VACINAÇÃO (NORMALIZADO)
# ============================================================================

deaths_df_comp <- data %>%
  group_by(continent, date) %>%
  summarise(deaths_value = mean(new_deaths_smoothed_per_million, na.rm = TRUE), .groups = "drop") %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(continent, month) %>%
  summarise(deaths_value = mean(deaths_value, na.rm = TRUE), .groups = "drop")

vacc_df_comp <- data %>%
  group_by(continent, date) %>%
  summarise(vacc_value = mean(people_fully_vaccinated_per_hundred, na.rm = TRUE), .groups = "drop") %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(continent, month) %>%
  summarise(vacc_value = mean(vacc_value, na.rm = TRUE), .groups = "drop") %>%
  mutate(vacc_value = vacc_value * 10000)  # Converter percentual para escala por milhão

deaths_vs_vacc <- deaths_df_comp %>%
  left_join(vacc_df_comp, by = c("continent", "month")) %>%
  ungroup()

p_deaths_vs_vaccination <- deaths_vs_vacc %>%
  ggplot(aes(x = month)) +
  geom_col(aes(y = vacc_value, fill = "Vacinação Completa"), position = position_dodge(width = 20), width = 18, alpha = 0.85) +
  geom_line(aes(y = deaths_value * 100000, color = "Mortes"), linewidth = 1.2, alpha = 0.8) +
  facet_wrap(~continent, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("Vacinação Completa" = "#009E73")) +
  scale_color_manual(values = c("Mortes" = "#E69F00")) +
  scale_x_date(limits = c(as.Date("2020-01-01"), max_date), breaks = date_breaks_seq, date_labels = "%b %Y") +
  scale_y_continuous(
    name = "Pessoas Vacinadas por Milhão",
    sec.axis = sec_axis(~. / 100000, name = "Mortes por Milhão")
  ) +
  labs(
    title = "Mortes vs. Vacinação",
    subtitle = "Mortes por milhão (linha) e pessoas vacinadas por milhão (colunas) com eixos independentes.",
    caption = "Fonte: Our World in Data (OWID)",
    x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5)
  )

# ============================================================================
# GERAR PDF UNIFICADO COM TODOS OS GRÁFICOS
# ============================================================================

all_plots <- list(
  cases_plot,
  deaths_plot,
  vacc_plot,
  stringency_plot,
  p_cases_vs_restrictions,
  p_deaths_vs_vaccination
)

output_pdf <- "covid_graph/report_all_plots.pdf"
pdf(output_pdf, width = 11, height = 8.5)
for (plot in all_plots) {
  print(plot)
}
dev.off()

message("Relatório completo gerado: ", output_pdf)

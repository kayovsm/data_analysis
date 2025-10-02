.libPaths("~/R/library")

library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# buscando dados no arquivo "student_grade.csv"
student_data <- read_csv("dataset/student_grade.csv")

glimpse(student_data)

# organizar dados em formato tidy
tidy_data <- student_data %>%
  pivot_longer(cols = starts_with("Q"), names_to = "question", values_to = "grade") %>%
  mutate(question = str_remove(question, "Q"))

# removendo duplicatas
tidy_data <- tidy_data %>% distinct()

# removendo NA
tidy_data <- tidy_data %>% drop_na(grade)

# removendo colunas com informações repetidas
tidy_data <- tidy_data %>% select(-Year, -Class)

# inspecionando dados organizados
glimpse(tidy_data)

# media e dp para questões 1 e 2
summary_stats <- tidy_data %>%
  filter(question %in% c("1", "2")) %>%
  group_by(question) %>%
  summarise(mean_grade = mean(grade), sd_grade = sd(grade))

print(summary_stats)
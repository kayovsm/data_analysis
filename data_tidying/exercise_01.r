.libPaths("~/R/library")

library(readr)
library(dplyr)

# buscando dados no arquivo "small_file.txt"
my_data <- read_delim("dataset/small_file.txt", delim = "\t")

head(my_data)
glimpse(my_data)

# filtrando linhas da categoria D
my_data %>% filter(Category == "D")

# ordenando por comprimento a categoria D
my_data %>% filter(Category == "D") %>% arrange(Length)

# media do comprimento da categoria D
mean_d <- my_data %>% filter(Category == "D") %>% summarise(mean_length = mean(Length))
print(mean_d)

# media do comprimento da categoria A
mean_a <- my_data %>% filter(Category == "A") %>% summarise(mean_length = mean(Length))
print(mean_a)
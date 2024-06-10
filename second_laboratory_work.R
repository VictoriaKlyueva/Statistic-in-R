library(ggplot2)

# Загрузка датасета
data <- read.csv("data.csv")
data <- data[sample(nrow(data), 200), ]
# Перевод фичи в категориальный тип данных
data$Month <- as.factor(data$Month)

head(data)

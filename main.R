library(ggplot2)

# Загрузка датасета
data <- read.csv("data.csv")
data <- data[sample(nrow(data), 200), ]
# Перевод фичи в категориальный тип данных
data$Month <- as.factor(data$Month)

# Просмотр основных данных из датасета
head(data)
summary(data)
dim(data)

# Построение графиков для визуализации
visualize_distribution = function(feature, name) {
  # Визуадизация распределения
  ggplot(data, aes(x = feature)) +
    # Гистограмма
    geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "skyblue", color = "black") +
    # Полигон частот
    geom_density(alpha = 0.4, fill = "lightgreen") + ggtitle(paste("Распределение", name, sep=" ")) +
    
    # Добавление подписей осей
    xlab(name) + ylab("Density")
}

# Построение графиков для визуализации категориальной переменной
visualize_factor_distribution = function(feature, name) {
  ggplot(data, aes(x = feature)) + 
    # Гистограмма
    geom_bar(fill = "skyblue", color = "black") + 
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
    # Добавление подписей осей 
    xlab(name) + ylab("Count") + 
    ggtitle(paste("Распределение", name, sep=" ")) 
}
    
# Вывод метрик
show_metrics = function(feature, name) {
  # Среднее
  mean <- sum(feature) / length(feature)
  mean_right <- mean(feature)
  
  # Медиана
  sorted <- sort(feature)
  if (length(sorted) %% 2 == 1) { 
    median <- sorted[(length(sorted) + 1) %/% 2] 
  } else { 
    median <- (sorted[length(sorted) %/% 2] + sorted[(length(sorted) %/% 2) + 1]) / 2 
  }
  median_right <-  median(feature)
  
  # Мода
  mode <- as.numeric(names(sort(-table(feature))[1]))
  mode_right <- as.numeric(names(table(feature))[which.max(table(feature))])
  
  # Дисперсия
  variance <- sum((feature - mean)^2) / (length(feature) - 1)
  variance_right <- var(feature)
  
  # Среднее отклонение
  std <- sqrt(variance)
  std_right <- sd(feature)
  
  cat(paste("Метрики для", name, sep=" "), "\n")
  cat("Среднее:", mean, "Верное среднее:", mean_right, "\n") 
  cat("Медиана:", median, "Верная медиана:", median_right, "\n")
  cat("Мода:", mode, "Верная мода:", mode_right, "\n") 
  cat("Дисперсия:", variance, "Верная дисперсия:", variance_right, "\n") 
  cat("Cтандартное отклонение:", std, "Верное стандратное отклонение:", std_right, "\n")
}

# Работа с Month

# Выведем уникальные значения для поиска выбросов
library(dplyr)
my_summary_data <- data %>%
  group_by(Month) %>%
  summarise(Count = n())  
my_summary_data

# Выбросов нет

# Визуадизация распределения
visualize_factor_distribution(data$Month, "Month")


# Работа с Close

# Убеждаемся в отсутсвии выбросов с помощью ящика с усами
boxplot(data$Close, ylab = "Close")

# Визуадизация распределения
visualize_distribution(data$Close, "Close")

show_metrics(data$Close, "Close")


# Работа с Number of trades

# Убеждаемся в наличии выбросов с помощью ящика с усами
boxplot(data$Number.of.trades, ylab = "Number of trades")

# Удаление выбросов методом межквартильного диапазона
Q1 <- quantile(data$Number.of.trades, .25)
Q3 <- quantile(data$Number.of.trades, .75)
IQR <- IQR(data$Number.of.trades)
data <- subset(data, data$Number.of.trades > (Q1 - 1.5 * IQR) & data$Number.of.trades < (Q3 + 1.5 * IQR))

# Снова проверяем ящик с усами
boxplot(data$Number.of.trades, ylab = "Number of trades")

# Визуализация распределения
visualize_distribution(data$Number.of.trades, "Number of trades")

# Вывод метрик
show_metrics(data$Number.of.trades, "Number of trades")


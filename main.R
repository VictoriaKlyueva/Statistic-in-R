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
  ggplot(data, aes(x = factor(feature))) + 
    geom_bar(fill = "skyblue", color = "black") + 
    # Добавление подписей осей 
    xlab(name) + ylab("Count") + 
    ggtitle(paste("Распределение", name, sep=" ")) 
}
    
# Вывод метрик
show_metrics = function(feature) {
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
  
  cat("Среднее:", mean, "\n") 
  cat("Медиана:", median, "\n")
  cat("Мода:", mode, "\n") 
  cat("Дисперсия:", variance, "\n") 
  cat("Cтандартное отклонение:", std, "\n")
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

# Вывод метрик
show_metrics(data$Close)


# Работа с Close

# Убеждаемся в отсутсвии выбросов с помощью ящика с усами
boxplot(data$Close, ylab = "Close")

# Визуадизация распределения
visualize_distribution(data$Close, "Close")

show_metrics(data$Close)


# Работа с Number of trades

# Убеждаемся в наличии выбросов с помощью ящика с усами
boxplot(data$Number.of.trades, ylab = "Number of trades")

# Удаление выбросов методом межквартильного диапазона
Q1 <- quantile(feature, .25)
Q3 <- quantile(feature, .75)
IQR <- IQR(feature)
data <- subset(data, feature > (Q1 - 1.5 * IQR) & feature < (Q3 + 1.5 * IQR))

# Снова проверяем ящик с усами
boxplot(data$Number.of.trades, ylab = "Number of trades")

# Визуализация распределения
visualize_distribution(data$Number.of.trades, "Number of trades")

# Вывод метрик
show_metrics(data$Number.of.trades)
library(ggplot2)

# Загрузка датасета
data <- read.csv("data.csv")
data <- data[sample(nrow(data), 200), ]

# Просмотр основных данных из датасета
head(data)
summary(data)
dim(data)

### Работа с Close

# Убеждаемся в отсутсвии выбросов с помощью ящика с усами
boxplot(data$Close, ylab = "Close")

# Визуадизация распределения
ggplot(data, aes(x = data$Close)) +
  # Гистограмма
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "skyblue", color = "black") +
  # Полигон частот
  geom_density(alpha = 0.4, fill = "lightgreen") + ggtitle("Распределение Close") +
  
  # Добавление подписей осей
  xlab("Close price") +
  ylab("Density")

# Среднее
mean_close <- sum(data$Close) / length(data$Close)
mean_right_close <- mean(data$Close)

# Медиана
sorted_close <- sort(data$Close)
if (length(sorted_close) %% 2 == 1) { 
  median_close <- sorted_close[(length(sorted_close) + 1) %/% 2] 
} else { 
    median_close <- (sorted_close[length(sorted_close) %/% 2] + sorted_close[(length(sorted_close) %/% 2) + 1]) / 2 
}
median_right_close <-  median(data$Close)

# Мода
mode_close <- as.numeric(names(sort(-table(data$Close))[1]))
mode_right_close <- as.numeric(names(table(data$Close))[which.max(table(data$Close))])

# Дисперсия
variance_close <- sum((data$Close - mean_close)^2) / (length(data$Close) - 1)
variance_right_close <- var(data$Close)

# Среднее отклонение
std_close <- sqrt(variance_close)
std_right_close <- sd(data$Close)

cat("Среднее:", mean_close, "\n") 
cat("Медиана:", median_close, "\n")
cat("Мода:", mode_close, "\n") 
cat("Дисперсия:", variance_close, "\n") 
cat("Cтандартное отклонение:", std_close, "\n")

### Работа с Number of trades

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
ggplot(data, aes(x = Number.of.trades)) +
  # Гистограмма
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "skyblue", color = "black") +
  # Полигон частот
  geom_density(alpha = 0.4, fill = "lightgreen") + ggtitle("Распределение Number of trades") +
  
  # Добавление подписей осей
  xlab("Number of trades") +
  ylab("Density")

# Среднее
mean_trades <- sum(data$Number.of.trades) / length(data$Number.of.trades)
mean_right_trades <- mean(data$Number.of.trades)

# Медиана
sorted_trades <- sort(data$Number.of.trades)
if (length(sorted_trades) %% 2 == 1) { 
  median_trades <- sorted_trades[(length(sorted_trades) + 1) %/% 2] 
} else { 
  median_trades <- (sorted_trades[length(sorted_trades) %/% 2] + sorted_trades[(length(sorted_trades) %/% 2) + 1]) / 2 
}
median_right_trades <-  median(data$Number.of.trades)

# Мода
mode_trades <- as.numeric(names(sort(-table(data$Number.of.trades))[1]))
mode_right_trades <- as.numeric(names(table(data$Number.of.trades))[which.max(table(data$Number.of.trades))])

# Дисперсия
variance_trades <- sum((data$Number.of.trades - mean_trades)^2) / (length(data$Number.of.trades) - 1)
variance_right_trades <- var(data$Number.of.trades)

# Среднее отклонение
std_trades <- sqrt(variance_trades)
std_right_trades <- sd(data$Number.of.trades)

cat("Среднее:", mean_trades, "\n") 
cat("Медиана:", mean_trades, "\n")
cat("Мода:", mode_trades, "\n") 
cat("Дисперсия:", variance_trades, "\n") 
cat("Cтандартное отклонение:", std_trades, "\n")


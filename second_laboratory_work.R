# О том, как проверять гипотезы
# https://habr.com/ru/articles/558836/

# Загрузка датасета
data <- read.csv("data.csv")
data <- data[sample(nrow(data), 200), ]
# Перевод фичи в категориальный тип данных
data$Month <- as.factor(data$Month)

head(data)

# Гипотеза: Среднее значение Close равняется 50 000
hypothetical_mean = 50000

# 1. Уровень значимости
alpha = 0.05

# 2. О доверительном интервале
# https://www.codecamp.ru/blog/confidence-interval-mean/

# Стандартное отклонение
std = sd(data$Close)
# Размер выборки
n = length(data$Close)

# Критическое значение t-распределения для alpha = 0.05
# по таблице критических значений t-критерия Стьюдента
# https://statpsy.ru/t-student/t-test-tablica/
t_critical = qt(0.975, df = n-1)

# Границы доверительного интервала
lower_bound = mean(data$Close) - t_critical * std / sqrt(n)
upper_bound = mean(data$Close) + t_critical * std / sqrt(n)

# 3. Вывод
cat("Границы доверительного интервала: [", lower_bound, ":", upper_bound, "]", sep="")
cat(
    if (lower_bound <= hypothetical_mean && hypothetical_mean <= upper_bound) 
      "С 95% процентной вероятностью среднее значение генеральной совокупности находится в интервале [lower_bound:upper_bound], гипотеза верна" 
    else 
      "Гипотеза неверна"
) 

# 4. Нахождение P-value вероятности:
# Нахождение t-статистики
t_stat = (mean(data$Close) - hypothetical_mean) / SE

# P-value
p_value = 2 * pt(abs(t_stat), df = n-1, lower = FALSE)
# pt - кумулятивная функция плотности (cdf) распределения Стьюдента
# (считается только по таблице или через встроенные функции)
cat(if (p_value >= alpha) "Гипотеза не может быть отклонена" else "Можно отклонить гипотезу") 


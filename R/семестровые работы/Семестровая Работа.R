df <- read.csv("https://kms.kpfu.ru/sites/default/files/unmanaged/forstud/Курсовая%20работа%20по%20ТВМС/813/7/r1z1.csv")

# Быстрый расчет основных параметров описательной статистики.
summary(df)

# Арифметическая средняя:
mean(df$X)

# Медиана
median(df$X)

# Дисперсия(смещённая оценка)
n <- length(df$X)
z <- (1/n * sum( (df$X - mean(df$X))^2 ))
z
# Дисперсия(не смещённая оценка), аналог R-функция var
y <- (1/(n-1) * sum( (df$X - mean(df$X))^2 ))
y
# Стандартное отклонение:
sd(df$X)

# Минимальное значение:
min(df$X)

# Максимальное значение:
max(df$X)

# Число элементов в векторе (Выборка)
length(df$X)

# Стандартная ошибка средней
SEdf <-  sd(df$X)/sqrt(length(df$X))
SEdf

# Квантиль
quantile(df$X)

# Размах выборки
b <- (quantile(df$X, 1.0) - quantile(df$X, 0.0))
b

# Интерквартильный размах
IQR(df$X)

# ассиметрия
library(psych)
skew(df$X)

# Построение гистограммы вариант1
k <- (1 + 3.2 * log(length(df$X)))
hist(df$X, breaks = k, freq = FALSE, col = "lightblue",
     xlab = "X variable",                            # Переменная X
     ylab = "Probability Density",                   # Плотность вероятности
     main = "Histogram combined with density curve") # Гистограмма, совмещенная с кривой плотности

lines(density(df$X), col = "red", lwd = 2)
abline(v= density(df$X)$x[which.max(density(df$X)$y)], col="green")
grid()
main <-  "X"
location <-  "topright"
labels <-  c("lighthist", "curve", "moda")
colors <-  c("blue", "red", "green")
legend(location, labels, title = main, fill=colors)

# Построение гистограммы вариант2 (реализация встроенной функции hist)
k <- (1.72 * (length(df$X))^(1/3))
hist(df$X, breaks = k, freq = FALSE, col = "lightblue",
     xlab = "X variable",                            # Переменная X
     ylab = "Probability Density",                   # Плотность вероятности
     main = "Histogram combined with density curve") # Гистограмма, совмещенная с кривой плотности

lines(density(df$X), col = "red", lwd = 2)
abline(v= density(df$X)$x[which.max(density(df$X)$y)], col="green")
grid()
main  <-  "X"
location <-  "topright"
labels <-  c("lighthist", "curve", "moda")
colors <-  c("blue", "red", "green")
legend(location, labels, title = main, fill=colors)

# Мода
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
v <- df$X
result <- getmode(v)
print(result)

# Эмпирическая функция распределения
# Сравние (ЭФР) с функцией распределения  соответствующая нормальному распределению
data <- df$X
n <- length(data)
plot(sort(data), (1:n)/n, type="S",
     col="seagreen", main="Empirical distribution function",
     xlab="x",
     ylab="")

# mn <- mean(data)
# sig <- sd(data)
#x <- seq(110, 140, by=0.25)
#lines(x, pnorm(x, mean=mn, sd=sig), type="l", col="orange", lwd=1)





# Вариант 5 (Z9: Проверить гипотезу однородности критерием Вилкоксона)
# α = 0.025
# K: I-ая гр. БолМеньше

data1 <- read.csv('https://kms.kpfu.ru/sites/default/files/unmanaged/forstud/Курсовая%20работа%20по%20ТВМС/813/7/r2z1.csv')

X <- data1$X
Y <- data1$Y

# Объединить все данные в единый ряд, пометив данные, принадлежащие разным выборкам.
XY <-  c(X, Y)
XY_TYPE <-  rep(c("X", "Y"), each = 19)
DATASET <- data.frame(XY_TYPE, XY, stringsAsFactors = TRUE)

# Проранжировать значения, приписывая меньшему значению меньший ранг. Всего рангов получится (n1 + n2).
DATASET$rank <- rank(XY)

# Подсчитать сумму рангов отдельно для каждой выборки.
DATASET <- na.omit(DATASET)
sumXY <- tapply(DATASET$rank, DATASET$XY_TYPE, FUN=sum)

# Определить большую из двух ранговых сумм.
sumXY[[1]]  # число X
sumXY[[2]]  # число Y
maxXY <- sumXY[[which.max(sumXY)]]

# Определить значение U по формуле:
# U = n1*n2 + nx·(nx + 1)/2 – Tx,
# где n1 – объем выборки №1; n2 – объем выборки №2;
# Tx – большая из двух ранговых сумм;
# nx – объем максимальной выборки: nx= max(n1, n2).
n1 <- length(na.omit(X))
n2 <- length(Y)
nx <- length(Y)
Tx <- sumXY[[2]]
U <- n1*n2 + ((nx*(nx+1))/2 ) - Tx

z <-  (U - (n1*n2/2)) / sqrt(n1*n2*(n1+n2+1)/12)
# смотрим таблицу http://www.z-table.com
# умножаем на два так как нас интересуют оба хвоста
p <-  0.2451 * 2




# мат ожиданием
MUw <- (n1*(n1+n2+1))/2 + 0.5
# дисперсией
SIGMAw <- (n1*n2*(n1+n2+1))/12
?pwilcox()
pp <- pwilcox(XY, 19, 18)
(sum(na.omit(pp)))
# p > 𝛼 - хорошее согласие с нулевой гипотезой
# Принимается нулевая гипотеза, альтернативная отвергается.
# pwilcox — функция распредления Вилкоксона
# dwilcox — функция плотности Вилкоксона
# qwilcox — квантиль распредления Вилкоксона



# ==============================================================
# смотрим таблицу http://edu.mari.ru/rnoo/DocLib7/В%20помощь%20учителю/Таблицы%20Манна-Уитни%20и%20Вилкоксона.pdf
# и выясняем что Скрит = 106
# Для вычисления функции распределения Вилкоксона можно воспользоваться асимптотической формулой.
# Известно, что статистика W асимптотически нормальна с
# мат ожиданием
MUw <- (n1*(n1+n2+1))/2 + 0.5
# дисперсией
SIGMAw <- (n1*n2*(n1+n2+1))/12


# Φ — стандартная нормальная функция распределения.
phi <- (U - MUw)/sqrt(SIGMAw)
wilcox.test(X, Y, paired = F, alternative = "two.sided")






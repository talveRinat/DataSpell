

# задание 1
data1 <- read.csv('https://kms.kpfu.ru/sites/default/files/unmanaged/forstud/Курсовая%20работа%20по%20ТВМС/813/7/r2z1.csv')

# Задание r2z1:
# Вариант 5 (Z9: Проверить гипотезу однородности критерием Вилкоксона)
# α = 0.025
# K: I-ая гр. БолМеньше
data1_without_NA <- na.omit(data1)
after <-  data1_without_NA$Y
before <-  data1_without_NA$X
# вектор знаков
sgn <-  sign(after-before)
# абсолютная величина изменения
abs <-  abs(after - before)
# создадим таблицу
d <-  data.frame(after,before,sgn,abs)
# ранжируем по абсолютной величине изменения
d$rank <-  rank(replace(abs,abs==0,NA), na='keep')
# знаковый ранг
d$multi <-  d$sgn * d$rank
# Подсчитать сумму рангов
mass <- d$multi
# для положительных (ΣТ+)
(W_p <- sum(mass[mass>0], na.rm = T))
# и отрицательных (ΣТ–) разностей
(W_n <- abs(sum(mass[mass<0], na.rm = T)))
# Меньшая из двух сумм рангов (без учета знака) и
# будет являться эмпирическим значением парного критерия Вилкоксона (Т).
T <- W_n
# рассчитаем z-значение для парного критерия Вилкоксона и уровень статистической значимости

# n – количество наблюдений, не имеющих нулевых изменений.
n <- length(mass)
# среднее значение парного критерия Вилкоксона
tt <-(n*(n+1))/4
# стандартная ошибка парного критерия Вилкоксона
SEt <-sqrt((n*(n+1)*(2*n+1))/24)
# z - значение равно
z <- (T - tt)/SEt
# смотрим таблицу областей под нормальной кривой
# получаем число p =  0.2236
# что бы получить p-value надо умножить число на два
p_v <- 0.2236 * 2




wilcox.test(data1$X,data1$Y, paired = F, alternative = "greate")


wilcox.test(d$before,d$after, paired = F, alternative = "two.sided", exact = T)



# задание 2
read.csv('https://kms.kpfu.ru/sites/default/files/unmanaged/forstud/Курсовая%20работа%20по%20ТВМС/813/7/r2z2.csv')
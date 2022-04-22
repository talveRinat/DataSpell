# Title     : Практические задания по анализу данных в R.
# Objective : https://stepik.org/lesson/26186/step/1?unit=8128
# Created by: talverinat
# Created on: 22.02.2021

# Пример 1

# Напишите функцию smart_test, которая получает на вход dataframe с двумя номинативными переменными с произвольным числом градаций.
# Функция должна проверять гипотезу о независимости этих двух переменных при помощи критерия хи - квадрат или точного критерия Фишера.
# Если хотя бы в одной ячейке таблицы сопряженности двух переменных меньше 5 наблюдений,
# функция должна рассчитывать точный критерий Фишера и возвращать вектор из одного элемента: получившегося p - уровня значимости.
# Если наблюдений достаточно для расчета хи-квадрат (во всех ячейках больше либо равно 5 наблюдений),
# тогда функция должна применять критерий хи-квадрат и возвращать вектор из трех элементов: значение хи-квадрат, число степеней свободы,  p-уровня значимости.

A <- table(mtcars[,c("am", "vs")])
all(A > 5)
B <- table(mtcars[1:20,c("am", "vs")])
all(B > 5)

# или можно через (any x < 5)
smart_test1 <- function(x) {
  x <-  table(x)
  if(all(x > 5)){
    print(c(chisq.test(x)$statistic, chisq.test(x)$parameter, chisq.test(x)$p.value))
  }
  else{
    print (fisher.test(x)$p.value)
  }
}

smart_test2 <- function(test_data) {
  test_table <- table(test_data)
  if (min(test_table) < 5){
    fit  <- fisher.test(test_table)
    result  <- fit$p.value
  }
  else {
    fit  <- chisq.test(test_table)
    result  <- c(fit$statistic, fit$parameter, fit$p.value)
  }

  return(result)
}

smart_test1(B)
smart_test1(A)


# Пример 2

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data.csv", stringsAsFactors = F)
str(test_data)

# Почувствуй себя биоинформатиком!  Вся наследственная информация в живых организмах хранится внутри молекулы ДНК.
# Эта молекула состоит из последовательности четырех "букв" — A, T, G и C.
# Напишите функцию most_significant, которая получает на вход dataframe с произвольным количеством переменных,
# где каждая переменная это нуклеотидная последовательность.
# Cкачайте тестовый набор данных (смотри ниже), чтобы познакомиться с их структурой, на которых будет тестироваться ваша функция.
# Рассмотрим пример, всего-лишь с несколькими наблюдениями, чтобы прояснить суть задачи:
# V1 V2 V3
# 1  A  A  C
# 2  G  G  A
# 3  C  C  C
# 4  T  T  A
# 5  G  T  T
# 6  T  A  G

# В этом примере три последовательности  V1 , V2, V3.
# Для каждой переменной мы можем проверить нулевую гипотезу о том, что все нуклеотиды (A, T, G, C)
# встречаются равновероятно внутри этой последовательности. Однако, возможно,
# что в некоторых последовательностях распределение частоты встречаемости каждого нуклеотида отличается от равномерного.
# Функция должна возвращать вектор с названием переменной (или переменных),
# в которой был получен минимальный p - уровень значимости при проверке гипотезы о равномерном распределении нуклеотидов
# при помощи критерия хи - квадрат.

most_significant1 <- function(test_data){
  variable_p_values <- sapply(test_data, function(x) chisq.test(table(x))$p.value)
  name_of_minimal <- colnames(test_data[which(variable_p_values == min(variable_p_values))])
  return(name_of_minimal)
}


most_significant2  <- function(test_data){
	chisq_tests <- sapply(test_data, function(col) chisq.test(table(col))$p.value)
	min_p  <- which(chisq_tests == min(chisq_tests))
	return(colnames(test_data)[min_p])
}

most_significant1(test_data)

# Как работает функция:
#  1. Сначала мы при помощи `table(x)` считаем таблицу соопряженности.
#  2. Далее при помощи `sapply` применяем `chisq.test()` ко всем столбцам исходной выборки.
#  3. Присваиваем полученный именнованный вектор в переменную `variable_p_values`
#  4. При помощи `min(variable_p_values)` ищем минимальное значение p-уровня значимости
#  5. Функцией `which()` получаем именованный вектор номеров, p-value  в которых был минимальным
#  6. При помощи индексирования `[]` берём из исходной выборки переменные
#  7. Через `colnames` получаем именна переменных

chisq.test(table(test_data$V1))$p.value



# Пример 3

# В лекциях я говорил, что иногда возникает необходимость перекодировать количественную переменную в номинативную.
# Однако зачастую мы можем создавать новую номинативную переменную, комбинируя значения нескольких количественных переменных.
# Рассмотрим такой пример.
# Воспользуемся встроенными в R данными Iris.
# ?iris
# Создайте новую переменную important_cases - фактор с двумя градациями ("No" и "Yes").
# Переменная должна принимать значение Yes, если для данного цветка значения хотя бы трех количественных переменных выше среднего.
# В противном случае переменная important_cases  будет принимать значение No.

#Например, рассмотрим первую строчку данных iris:

#    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
# 1       5.1         3.5          1.4         0.2     setosa

# В данном случае только значение  Sepal.Width 3.5 больше, чем среднее значение mean(Sepal.Width) = 3.057.
# Соответственно для первого цветка значение переменной important_cases будет "No".

# Теперь рассмотрим 62 строчку данных

#     Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
# 62      5.9           3          4.2         1.5        versicolor

# В данном случае и значение Sepal.Length 5.9 больше чем среднее по выборке, mean(Sepal.Length)  = 5.84.
# Также значение Petal.Length и Petal.Width для этого цветка больше чем соответствующие средние значения:
# mean(Petal.Length) = 3.76,   mean(Petal.Width ) = 1.1.
# Следовательно, для этого цветка значение переменной important_cases будет "Yes".

# Таким образом, если хотя бы три переменные превышают среднее значение по выборке,
# тогда  значение переменной important_cases будет "Yes".

# Что должно получиться:
# > str(iris$important_cases)
# Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
# > table(iris$important_cases)
# No Yes
# 81  69

# Формат ответа: в поле для ответа напишите скрипт, который создает новую переменную - фактор в данных iris.
# Код для проверки задания считает переменную  important_cases из данных Iris и сравнит ее с верным ответом.

### Решение

# Для того чтобы корректно сравнить матрицу построчно с вектором нужно произвести транспонирование.
# Чтобы полученная матрица была той же размерности что и исходная, нужно сделать ещё одно транспонирование:

vector <- c(5,9)
matrix_test <- matrix(c(4,10,4,13,2,8),3,2)
t(t(matrix_test) < vector)

# Разберём задачу по шагам:

# Получаем вектор средних количественных значений
mean_iris <- sapply(iris[1:4], mean)

# Сравниваем матрицу построчно с вектором средних
compare_matrix <- t(t(iris[1:4]) > mean_iris)

# Получаем вектор с количеством измерений выше средних
count_high_mean <- rowSums(compare_matrix)

# Задаём условие --- если 3 или больше измерений больше среднего присвой "Yes", иначе "No"
important <- ifelse(count_high_mean >= 3, "Yes", "No")

# Делаем этот вектор фактором
as.factor(important)

# Присваиваем в новую переменную в iris
iris$important_cases <- as.factor(important)


# сводное решение "в одну строку"
iris$important_cases <- as.factor(ifelse(rowSums(t(t(iris[1:4]) > sapply(iris[1:4], mean))) >= 3, "Yes", "No"))

str(iris$important_cases)
table(iris$important_cases)

# другой вариант
importance_calc <- function(v1, v2, threshold=3) {
  ifelse(sum(v1 > v2) >= threshold, 'Yes', 'No')
}
iris$important_cases <- factor(apply(iris[1:4], 1, importance_calc, v2 = colMeans(iris[, 1:4])))


# Пример 4

# Обобщим предыдущую задачу!
# Напишем функцию get_important_cases, которая принимает на вход dataframe с произвольным числом количественных переменных
# (гарантируется хотя бы две переменные). Функция должна возвращать dataframe с новой переменной - фактором important_cases.
# Переменная  important_cases принимает значение Yes, если для данного наблюдения больше половины количественных переменных имеют значения больше среднего.
# В противном случае переменная important_cases принимает значение No.
# Переменная  important_cases - фактор с двумя уровнями 0 - "No", 1  - "Yes".
# То есть даже если в каком-то из тестов все наблюдения получили значения "No", фактор должен иметь две градации.

# Пример работы функции.

# > test_data <- data.frame(V1 = c(16, 21, 18),
#                           V2 = c(17, 7, 16),
#                           V3 = c(25, 23, 27),
#                           V4 = c(20, 22, 18),
#                           V5 = c(16, 17, 19))

# > get_important_cases(test_data)
#   V1 V2 V3 V4 V5 important_cases
# 1 16 17 25 20 16              No
# 2 21  7 23 22 17              No
# 3 18 16 27 18 19             Yes


#### Решение
# Всё просто. По сравнению с предыдущим примером, мы добавляем общее условие `length(df)/2` т.е. больше половины.
# А так же обобщаем уровень факторов.
test_data <- data.frame(V1 = c(16, 21, 18),
                        V2 = c(17, 7, 16),
                        V3 = c(25, 23, 27),
                        V4 = c(20, 22, 18),
                        V5 = c(16, 17, 19))

get_important_cases1 <- function(df){
df$important_cases <- ifelse(rowSums(t(t(df) > sapply(df, mean))) > length(df)/2, 1, 0)
df$important_cases <- factor(df$important_cases, levels = c(1, 0), labels = c("Yes", "No"))
return(df)
}

# альтернативное решение
get_important_cases2 <- function(d) {
	m <-  colMeans(d)
	compare_to_means <- apply(d, 1, function(x) as.numeric(x > m))
	is_important <- apply(compare_to_means, 2, sum) > ncol(d)/2
	is_important <- factor(is_important, levels = c(FALSE, TRUE), labels = c('No', 'Yes'))
	d$important_cases <- is_important
	return(d)
}
get_important_cases1(test_data)
get_important_cases2(test_data)


# Пример 5

# В R мы без труда можем рассчитать среднее и медиану вектора,
# а вот встроенной функции для расчета моды — наиболее часто встречаемого значения — в R нет!
# А мода так бы пригодилась нам при анализе номинативных данных! При этом функция mode в R существует,
# но выполняет абсолютно другую задачу
# (если хотите узнать, какую именно, ознакомьтесь со справкой: наберите в консоли ?mode).

# Напишите функцию stat_mode, которая получает на вход вектор из чисел произвольной длины и
# возвращает числовой вектор с наиболее часто встречаемым значением. Если наиболее часто встречаемых значений несколько,
# функция должна возвращать несколько значений моды  в виде числового вектора.

# > v <- c(1, 2, 3, 3, 3, 4, 5)
# > stat_mode(v)
# [1] 3

# > v <- c(1, 1, 1, 2, 3, 3, 3)
# > stat_mode(v)
# [1] 1 3

### Решение
v <- c(1, 2, 3, 3, 3, 4, 5)
v1 <- c(1, 1, 1, 2, 3, 3, 3)
v2 <- c(5,9,20,8,11,7,2,7,7,18,17,15,2,11)


stat_mode1 <- function(v){
  result <- names(which(table(v) == max(table(v))))
  return(as.integer(result))
}

stat_mode2 <- function(v){
	mode_positions <- which(table(v) == max(table(v)))
	as.numeric(names(table(v))[mode_positions])
}

stat_mode1(v2)

# Пример 6
# Доктор Пилюлькин решил вооружиться статистикой, чтобы сравнить эффективность трех лекарств!
# Давайте поможем ему и напишем функцию max_resid, которая получает на вход dataframe с двумя переменными:
# типом лекарства и результатом его применения.
#
# Drugs - фактор с тремя градациями: drug_1, drug_2, drug_3.
#
# Result - фактор с двумя градациями: positive, negative.
#
#  Функция должна находить ячейку таблицы сопряженности с максимальным  значением стандартизированного остатка и
#  возвращать вектор из двух элементов: название строчки и столбца этой ячейки.
#
# Для расчета стандартизированных остатков вы можете воспользоваться уже знакомой вам функцией chisq.test().
#  Изучите справку по этой функции, чтобы найти, где хранятся стандартизированные остатки.

# Пример работы функции на одном из вариантов:

# > test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_drugs.csv")
# > str(test_data)
# 'data.frame':  395 obs. of  2 variables:
#  $ Drugs : Factor w/ 3 levels "drug_1","drug_2",..: 3 1 1 2 1 1 3 1 2 3 ...
#  $ Result: Factor w/ 2 levels "negative","positive": 2 1 1 2 1 2 2 2 1 1 ...
# > max_resid(test_data)
# [1] "drug_1"   "positive"
# именно в этой ячейке было максимальное значение стандартизированного остатка, равное 2.07


### Решение
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_drugs.csv")

max_resid1 <- function(x){
  table_resid <- chisq.test(table(x))$stdres
  result <- c(rownames(which(table_resid == max(table_resid), arr.ind = TRUE)),
    rownames(which(t(table_resid) == max(table_resid), arr.ind = TRUE)))
  return(result)
}

max_resid2 <- function(test_data){
	d <- table(test_data)
	chi <- chisq.test(d)
	ind <- which(chi$stdres==max(chi$stdres), arr.ind = T)
	return(c(row.names(d)[ind[1]],colnames(d)[ind[2]]))
}

max_resid1(test_data)



# Пример 7
# Ну и напоследок построим гистограмму частот при помощи ggplot2!
# Чтобы получить доступ к данным:
# > install.packages("ggplot2") # если у вас не установлен пакет
# > library("ggplot2")
# теперь данные diamonds доступны для работы
# > str(diamonds)
# Основной способ визуализировать распределение частот номинативной переменной - гистограмма частот (барплот).
# Используя библиотеку ggplot2 и встроенные данные diamonds, постройте график распределения частот переменной color,
# на котором за цвет заполнения столбиков отвечает переменная cut.
# Сохраните код графика в переменную obj. В итоге должен получиться вот такой график.

obj <- ggplot(diamonds, aes(x=color, fill=cut)) +
	geom_bar(position='dodge')




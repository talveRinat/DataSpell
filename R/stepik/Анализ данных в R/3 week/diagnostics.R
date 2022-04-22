# Урок 4
# regression diagnostics
# 

library(ggplot2)

data(swiss)
str(swiss)



# relationships between all variables
pairs(swiss)

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point()


# Outliers

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point() + 
  geom_smooth(method = 'lm')


# Normality of variables distributions

ggplot(swiss, aes(x = Examination)) + 
  geom_histogram()

ggplot(swiss, aes(x = Education)) + 
  geom_histogram()


# linearity 

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point() + 
  geom_smooth()

lm1 <- lm(Education ~ Examination, swiss)
summary(lm1)


swiss$Examination_squared <- (swiss$Examination)^2

lm2 <- lm(Education ~ Examination + Examination_squared, swiss)
summary(lm2)


anova(lm2, lm1)


swiss$lm1_fitted <- lm1$fitted
swiss$lm2_fitted <- lm2$fitted
swiss$lm1_resid <- lm1$resid
swiss$lm2_resid <- lm2$resid
swiss$obs_number <- seq_len(nrow(swiss))

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point(size = 3) + 
  geom_line(aes(x = Examination, y = lm1_fitted), col = 'red', lwd=1) +
  geom_line(aes(x = Examination, y = lm2_fitted), col = 'blue', lwd=1)


ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid)) + 
  geom_point(size = 3) + geom_hline(y=0, col = 'red', lwd = 1)

ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid)) + 
  geom_point(size = 3) + geom_hline(y=0, col = 'red', lwd = 1)


# independence of errors

ggplot(swiss, aes(x = obs_number, y = lm1_resid)) + 
  geom_point(size = 3) + geom_smooth()

ggplot(swiss, aes(x = obs_number, y = lm2_resid)) + 
  geom_point(size = 3) + geom_smooth()


# Homoscedasticity

ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid)) + 
  geom_point(size = 3)

ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid)) + 
  geom_point(size = 3)


# Errors Normally distributed

ggplot(swiss, aes(x = lm1_resid)) + 
  geom_histogram(binwidth = 4, fill = 'white', col = 'black')

qqnorm(lm1$residuals)
qqline(lm1$residuals)

shapiro.test(lm1$residuals)


ggplot(swiss, aes(x = lm2_resid)) + 
  geom_histogram(binwidth = 4, fill = 'white', col = 'black')

qqnorm(lm2$residuals)
qqline(lm2$residuals)

shapiro.test(lm2$residuals)


#### Пример 1
# Какое преобразование позволяет сделать его распределение нормальным (согласно shapiro.test)?
my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, 0.069, 0.261, 0.241, 0.166, 0.283, 0.041, 0.369, 0.167, 0.001, 0.053, 0.262, 0.033, 0.457, 0.166, 0.344, 0.139, 0.162, 0.152, 0.107, 0.255, 0.037, 0.005, 0.042, 0.220, 0.283, 0.050, 0.194, 0.018, 0.291, 0.037, 0.085, 0.004, 0.265, 0.218, 0.071, 0.213, 0.232, 0.024, 0.049, 0.431, 0.061, 0.523)

# берет те где p > 0.05
shapiro.test(my_vector)
shapiro.test(log(my_vector))
shapiro.test(sqrt(my_vector))
shapiro.test((1/my_vector))


#### Пример 2
# Функция scale() позволяет совершить стандартизацию вектора, то есть делает его среднее значение равным нулю,
# а стандартное отклонение - единице (Z-преобразование).

# Стандартизованный коэффициент регрессии (β) можно получить, если предикторы и зависимая переменная стандартизованы.

# Напишите функцию, которая на вход получает dataframe с двумя количественными переменными,
# а возвращает стандартизованные коэффициенты для регрессионной модели,
# в которой первая переменная датафрейма выступает в качестве зависимой, а вторая в качестве независимой.

# Примеры работы функции.
# beta.coef(mtcars[,c(1,3)])
# -7.036582e-17 -8.475514e-01

# beta.coef(swiss[,c(1,4)])
# 3.603749e-16 -6.637889e-01

beta.coef <- function (df){
  # нормируем или масшабируем данные в количественных столбцах.
  # Параметр Center = True означает , что из данных в столбце вычитается среднее.
  # Параметр scale = true означает, что данные из которых уже вычли среднее ,
  # далее делятся на среднекв отклонение по этому столбцу
  df <- scale (df, center= TRUE, scale=TRUE)

  # переводим полученную отмасштабированную матртцу в датафрейм,
  # так как функция Lm требует на вход именно Датафрейм а не матрицу
  df <- as.data.frame(df)

  # в переменную fit помещаем нашу линейную регресионную прямую и выводим коэффициенты
  fit <- lm(df[,1]~df[,2])$coefficients

 return(fit)
}

beta.coef <- function(x){
	x <- scale(x)
	return(lm(x[, 1] ~ x[, 2])$coefficients)
}

beta.coef(mtcars[,c(1,3)])

# То, что вы только что сделали, можно сделать с помощью функции lm.beta из библиотеки QuantPsyc! :)
library(QuantPsyc)
lm.beta(lm(mpg ~ disp, mtcars))


# Пример 3
# Напишите функцию normality.test, которая получает на вход dataframe с количественными переменными,
# проверяет распределения каждой переменной на нормальность с помощью функции shapiro.test.
# Функция должна возвращать вектор с значениями p - value, полученного в результате проверки на нормальность каждой переменной.
# Названия элементов вектора должны совпадать с названиями переменных.

# Пример работы функции:
# normality.test(mtcars[,1:6])
#          mpg          cyl         disp           hp         drat           wt
# 1.228814e-01 6.058338e-06 2.080657e-02 4.880824e-02 1.100608e-01 9.265499e-02

# normality.test(iris[,-5])
# Sepal.Length  Sepal.Width Petal.Length  Petal.Width
# 1.018116e-02 1.011543e-01 7.412263e-10 1.680465e-08


# Опять же, обратите внимание функция должна работать корректно с различным количеством переменных и в независимости от их названий.
# Подсказка. Как задать имена элементов вектора:
my_vector <- c(1, 2, 3, 4)
names(my_vector) <- c("A", "B", "C", "D")
my_vector

normality.test  <- function(x){
  ans <- vector(length = length(x))
  for(i in seq_along(x)){
    ans[i] <- shapiro.test(x[[i]])$p.value
}
  names(ans) <- names(x)
  ans
}

normality.test2  <- function(x){
	return(sapply(x, FUN =  shapiro.test)['p.value',])
}

normality.test(mtcars[,1:6])
normality.test(iris[,-5])


### Пример 1
# Функция gvlma() из библиотеки gvlma позволяет получить оценку выполнения основных допущений линейной регрессии.
# В качестве аргумента она принимает объект, в который сохранена модель. Можно задать формулу модели прямо в функции gvlma.
# Чтобы увидеть основные статистики, нужно выполнить команду summary для объекта, созданного с помощью функции gvlma.
library(gvlma)
# Например,
x <- gvlma(fit)

# или

x <- gvlma(Y ~ X, data = mydata)

summary(x)


# Загрузите себе прикреплённый к этому степу датасет и постройте регрессию, предсказывающую DV по IV.
# Установите библиотеку gvlma и проверьте, удовлетворяется ли в этой модели требование гомоскедастичности.
# Введите в поле ответа p-значение для теста гетероскедастичности.

# Данные:
my_data <- read.csv("https://stepic.org/media/attachments/lesson/12088/homosc.csv")
my_data
res <- gvlma(DV ~ IV, data = my_data)
summary(res)["Heteroscedasticity",]['p-value']

# 1) Global Stat Являются ли отношения между вашими предсказателями X и Y  линейными?.
# Отклонение нулевой (p <.05) указывает на нелинейную связь между одним или несколькими вашими X и Y

# 2) Skewness <- Является ли ваше распределение искаженным положительно или отрицательно, что требует преобразования,
# чтобы соответствовать предположению о нормальности? Отклонение нулевого значения (p <.05) указывает на то,
# что вы, вероятно, должны преобразовать свои данные.

# 3) Kurtosis <-  проверка на выбросы (остроконечность) что требует трансформации,
# чтобы соответствовать предположению о нормальности? Отклонение нулевого значения (p <.05) указывает на то,
# что вы, вероятно, должны преобразовать свои данные.

# 4) Link function <- Является ли ваша зависимая переменная по-настоящему непрерывной или категоричной?
# Отклонение нулевого (p <.05) означает, что вы должны использовать альтернативную форму обобщенной линейной модели
# (например, логистическая или биномиальная регрессия).

# 5) Heteroscedasticity <- Является ли вариация остатков вашей модели постоянной в диапазоне X (предположение о гомосексуализме)?
# Отклонение нулевого значения (p <.05) указывает на то, что ваши остатки являются гетероседикальными и, следовательно,
# непостоянными в диапазоне X. Ваша модель лучше/хуже при прогнозировании для определенных диапазонов ваших шкал X.


#### Пример 2
# Напишите функцию resid.norm, которая тестирует распределение остатков от модели на нормальность при помощи функции shapiro.test
# и создает гистограмму при помощи функции ggplot() с красной заливкой "red", если распределение остатков значимо отличается от нормального (p < 0.05),
# и с зелёной заливкой "green" - если распределение остатков значимо не отличается от нормального.

# На вход функция получает регрессионную модель. Функция возвращает переменную, в которой сохранен график ggplot.

# В поле для ответа не нужно создавать никаких дополнительных объектов, только напишите функцию  resid.norm.

# Для создания гистограммы при помощи функции ggplot требуется dataframe, где хранится переменная.
# Обратите внимание на такие функции как:
# data.frame()
# as.data.frame()

resid.norm1  <- function(fit){
  if(shapiro.test(fit$residuals)$p.value < 0.05){
    ggplot(fit, aes(x = fit$residuals)) +
      geom_histogram( fill = 'red')
  }
  else{
    ggplot(fit, aes(x = fit$residuals)) +
      geom_histogram( fill = 'green')
  }
}

resid.norm2 <- function(fit) {
	resid.norm.pv <- shapiro.test(fit$residuals)$p.value
	plt <- ggplot(data.frame(fit$model), aes(x = fit$residuals)) +
		geom_histogram(fill = ifelse(resid.norm.pv < 0.05, 'red', 'green'))
	return(plt)
}

fit <- lm(mpg ~ disp, mtcars)
my_plot <- resid.norm(fit)
my_plot

fit2 <- lm(mpg ~ wt, mtcars)
my_plot2 <- resid.norm(fit2)
my_plot2


#### Пример 3

# Ещё одной проблемой регрессионных моделей может стать мультиколлинеарность - ситуация,
# когда предикторы очень сильно коррелируют между собой. Иногда корреляция между двумя предикторами может достигать 1,
# например, когда два предиктора - это одна и та же переменная, измеренная в разных шкалах
# (x1 - рост в метрах, x2 - рост в сантиметрах)

# Проверить данные на мультиколлинеарность можно по графику pairs()
# и посчитав корреляцию между всеми предикторами c помощью функции cor.

# Напишите функцию high.corr, которая принимает на вход датасет с произвольным числом количественных переменных и
# возвращает вектор с именами двух переменных с максимальным абсолютным значением коэффициента корреляции.

# Примеры работы функции:
# high.corr(swiss)
# [1] "Examination" "Education"

# high.corr(iris[,-5])
# [1] "Petal.Length" "Petal.Width"

# > x1 <- rnorm(30) # создадим случайную выборку
# > x2 <- rnorm(30) # создадим случайную выборку
# > x3  <- x1 + 5 # теперь коэффициент корреляции x1 и x3 равен единице
# > my_df <- data.frame(var1 = x1, var2 = x2, var3 = x3)
# > high.corr(my_df)
# [1] "var1" "var3"

# Вам могут понадобиться следующие функции: which, dimnames, colnames, rownames, diag, abs. Посмотрите справку по ним.

# Подсказки: Далеко не всегда 1 == 1 есть ТRUE!
# https://stackoverflow.com/questions/17606906/find-row-and-column-index-of-maximum-value-in-a-matrix

high.corr <- function(x){
	cr <- cor(x)
	diag(cr) <- 0
	return(rownames(which(abs(cr)==max(abs(cr)),arr.ind=T)))
}

high.corr2 <- function(x){
  dt <- cor(x)
  diag(dt) <- 0
  dt <- round(dt, 3)
  dt <- abs(dt)
  res <- which(dt == max(dt), arr.ind = TRUE)
  return(row.names(res))
}

high.corr2(swiss)
high.corr2(iris[,-5])

test_data <- as.data.frame(list(V1 = c(-1.8, 0.5, -0.3, -0.2, -0.8, 1.4, 1.8, -1.5, -1.6, -0.2),
                                V2 = c(0.9, -0.8, 0.8, -0.3, 0.1, 2.1, -0.2, 0.3, 0.2, -0.6),
                                V3 = c(0.3, 0.4, 0.2, -0.3, -1.3, -0.5, -2.1, 1.8, -0.6, 0),
                                V4 = c(-0.4, 0.6, -0.6, -0.9, 0.6, -0.5, 0.1, -0.7, -0.6, 0.4),
                                V5 = c(0, -0.1, 1.3, -1.7, -1.3, -0.6, -0.1, -0.7, 1.5, 1.3),
                                V6 = c(1.8, -0.5, 0.3, 0.2, 0.8, -1.4, -1.8, 1.5, 1.6, 0.2)))

high.corr2(test_data)
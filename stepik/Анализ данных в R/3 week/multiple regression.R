# Урок 3
# multiple linear regression

?swiss
swiss <- data.frame(swiss)
str(swiss)

hist(swiss$Fertility, col = 'red')
# numeric predictors

fit <- lm(Fertility ~ Examination + Catholic, data = swiss)
summary(fit)


fit2 <- lm(Fertility ~ Examination*Catholic, data = swiss)
summary(fit2)


confint(fit2)


# categorical predictors

hist(swiss$Catholic, col = 'red')

swiss$religious <- ifelse(swiss$Catholic > 60, 'Lots', 'Few')
swiss$religious <- as.factor(swiss$religious)

fit3 <- lm(Fertility ~ Examination + religious, data = swiss)
summary(fit3)

fit4 <- lm(Fertility ~ religious*Examination, data = swiss)
summary(fit4)

# plots

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point() 

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point() + 
  geom_smooth()

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point() 

ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point()  + 
  geom_smooth()

ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point()  + 
  geom_smooth(method = 'lm')


#

fit5 <- lm(Fertility ~ religious*Infant.Mortality*Examination, data = swiss)
summary(fit5)


# model comparison

rm(swiss)
swiss <- data.frame(swiss)

fit_full <- lm(Fertility ~ ., data = swiss)
summary(fit_full)

fit_reduced1 <- lm(Fertility ~ Infant.Mortality + Examination + Catholic + Education, data = swiss)
summary(fit_reduced1)

anova(fit_full, fit_reduced1)

fit_reduced2 <- lm(Fertility ~ Infant.Mortality + Education + Catholic + Agriculture, data = swiss)
summary(fit_reduced2)

anova(fit_full, fit_reduced2)


# model selection

optimal_fit <-  step(fit_full, direction = 'backward')
summary(optimal_fit)



#### Пример 1

# Напишите функцию fill_na, которая принимает на вход данные с тремя переменными:
# x_1  -  числовой вектор
# x_2 - числовой вектор
# y - числовой вектор с пропущенными значениями.
#
# Теперь — самое интересное. На первом этапе, используя только наблюдения, в которых нет пропущенных значений,
# мы построим регрессионную модель (без взаимодействий),
# где  y — зависимая переменная, x_1 и x_2 — независимые переменные.
# Затем, используя построенную модель, мы заполним пропущенные значения предсказаниями модели.
#
# Функция должна возвращать dataframe c новой переменной  y_full.
# Сохраните в нее переменную y, в которой пропущенные значения заполнены предсказанными значениями построенной модели.


## Решение
test_data <- read.csv("https://stepic.org/media/attachments/course/129/fill_na_test.csv")
str(test_data)

fill_na1 <- function(my_df){
  model <- lm(my_df[,3]~my_df[,1] + my_df[,2], my_df)
  my_df$y_full <- ifelse(is.na(my_df$y), predict(model, my_df), my_df$y)
  return(my_df)
}

fill_na2 <- function(my_df){
	fit <- lm(y ~ x_1 + x_2, my_df)
	my_df$y_full <-  ifelse(is.na(my_df$y), predict(fit, my_df), my_df$y)
	return(my_df)
}

fill_na3 <- function(my_df){
  data.frame(my_df, y_full = ifelse(is.na(my_df$y), predict(lm(y ~ x_1 + x_2, my_df), my_df), my_df$y))
}

fill_na3(test_data)


#### Пример 2
# В переменной df сохранен subset данных mtcars только с переменными "wt", "mpg", "disp", "drat", "hp".
# Воспользуйтесь множественным регрессионным анализом, чтобы предсказать вес машины (переменная "wt").
# Выберите такую комбинацию независимых переменных (из "mpg", "disp", "drat", "hp"),
# чтобы значение R^2 adjusted было наибольшим. Взаимодействия факторов учитывать не надо.
#
# Выполните все операции по сравнению моделей на вашем компьютере.
# В поле для ответа сохраните в переменную  model регрессионную модель с оптимальной комбинацией предикторов!

# Решение 1
df <- mtcars[,c("wt", "mpg", "disp", "drat", "hp")]
cols <- c("mpg", "disp", "drat", "hp")
max_adj_r <- 0
els <- ""
for(n in seq_along(cols)){
  for(xs in combn(c("mpg", "disp", "drat", "hp"), n, simplify = F)){
    frml <-  paste(xs, collapse = ' + ')
    lin_mod <-  lm(eval(paste0("wt ~ ", frml)), df)
    sum <-  summary(lin_mod)
    print(paste(frml, sum$adj.r.squared))
    if(sum$adj.r.squared > max_adj_r){
      max_adj_r <- sum$adj.r.squared
      els <- frml
    }
  }
}
print("")
print(paste("Max adj. R^2 is", max_adj_r, "for elements", els))

# Решение 2
summary(lm(wt ~ mpg + disp + drat + hp, data = df))$adj.r.squared # убираем по одному значению и сравниваем результат

# Решение 3
df1 <- mtcars[,c("wt", "mpg", "disp", "drat", "hp")]
fit_full <- lm(wt ~ ., data = df1)
optimal_fit <-  step(fit_full, direction = 'backward')
opt_summary <- summary(optimal_fit)
attr(as.formula(opt_summary), "term.labels")

# Ответ
model <- lm(wt ~mpg+disp+hp,df)


#### Пример 3
# Воспользуйтесь встроенным датасетом attitude, чтобы предсказать рейтинг (rating) по переменным complaints и critical.
# Каково t-значение для взаимодействия двух факторов?
# Разделителем целой и дробной части в ответе должна быть запятая!

# Решение
fit <- lm(rating ~ complaints * critical, data = attitude)
summary(fit)

# Короткое решение
summary(lm(rating ~ complaints*critical, data = attitude))$coefficients["complaints:critical", "t value"]

### Пример 4
# В этом примере будем работать с хорошо вам известным встроенным датасетом mtcars.
# Переменная am говорит о том, какая коробка передач используется в машине: 0 - автоматическая, 1 - ручная.

#Сделаем эту переменную факторной.
mtcars$am <- factor(mtcars$am, labels = c('Automatic', 'Manual'))

# Теперь постройте линейную модель, в которой в качестве зависимой переменной выступает расход топлива (mpg),
# а в качестве независимых - вес машины (wt) и коробка передач (модифицированная am), а также их взаимодействие.
# Выведите summary этой модели.

# Что отражает значение intercept в данной модели?
fit6 <- lm(mpg ~ wt*am, data = mtcars)
summary(fit6)

# Ответ:
# Расход топлива у машин с автоматической коробкой передач и нулевым весом

# Т.К Вес - непрерывная переменная, поэтому Intercept подразумевает нулевой вес.
# Для категориальной переменной при построении берется Automatic (она раньше по алфавиту, чем Manual).


### Пример 5
# В этой задаче снова нужно использовать модель из предыдущей задачи и её summary.
# Какие утверждения мы можем сделать на основе данной модели?
# Обратите внимание на то, что чем выше значение mpg (miles per gallon),
# тем ниже будет расход топлива (на одном галлоне бензина машина сможет проехать большее).

ggplot(mtcars, aes(x = wt, y = mpg, col = am)) +
  geom_point()  +
  geom_smooth(method = 'lm')

# Ответ:
# 1. У машин с ручной коробкой передач расход топлива ниже
# 2. В машинах с ручной коробкой передач вес сильнее влияет на расход топлива




#### Пример 1
# Сейчас мы поработаем со встроенным датасетом attitude. Рассмотрим две модели

model_full <- lm(rating ~ ., data = attitude)
model_null <- lm(rating ~ 1, data = attitude)

# model_full - модель, которая предсказывает значение переменной рейтинг (rating)
# в зависимости от всех остальных переменных в данном датасете.

# model_null - модель, в которой нет ни одного предиктора, а есть только intercept.
# Значение intercept - это просто среднее значение зависимой переменной.
# Соответственно, модель предоставляет нам информацию только о том, отличается ли это среднее от нуля.

# Как говорилось в лекции, функция step позволяет нам подобрать модель с оптимальным количеством предикторов.
# С помощью аргумента scope мы можем задать пространство моделей с разным числом предикторов,
# в котором будет происходить поиск оптимального набора предикторов.
# Самый простой путь - задать границы возможных моделей с помощью нулевой и полной моделей.

scope <-  list(lower = model_null, upper = model_full)

# Аргумент direction позволяет задать направление поиска.
# Первый аргумент (object) задаёт начальную модель, с которой начинается поиск.
# Обратите внимание на то, что при разных значениях аргумента direction нужно использовать разные начальные модели.

# Функция step возвращает оптимальную модель.

# Итак, задача! C помощью функции step найдите оптимальную модель для предсказания rating в датасете attitude.
# Model_full и model_null уже созданы. Сохраните команду с функцией step в переменную ideal_model.

ideal_model1 <- step(model_full, direction = 'backward')
ideal_model2 <- step(model_null, direction = "forward")
ideal_model3 <- step(model_full, scope = list(lower = model_null, upper = model_full),
                     direction = c("both", "backward", "forward"))

#### Пример 2
anova(model_full, ideal_model1)


#### Пример 3
# Напоследок потренируемся в эффективном написании формул.
# В этой задаче будем работать со встроенным датасетом LifeCycleSavings.
# Попытаемся предсказать значение sr на основе всех остальных переменных в этом датасете.
# Вспомните способы сокращения формул и напишите команду,
# которая создаёт линейную регрессию с главными эффектами и всеми возможными взаимодействиями второго уровня.
# Сохраните модель в переменную model.

# смотри файлик anova и 71 строчку
# можно и без скобок возвести в квадрат
model <- lm(sr ~ (.)^2, data = LifeCycleSavings)
model
# Урок 2
df  <- mtcars
df_numeric  <- df[,c(1,3:7)]

fit  <- lm(mpg ~ hp, df)
summary(fit)
str(fit)
fit$coefficients
ggplot(df, aes(hp, mpg))+
  geom_point(size = 5)+
  geom_smooth(method = "lm")+
  facet_grid(.~cyl)

ggplot(df, aes(hp, mpg))+
  geom_smooth(method = "lm", se = F)+
  facet_grid(.~cyl)

fitted_values_mpg  <- data.frame(mpg = df$mpg, fitted = fit$fitted.values )

new_hp <- data.frame(hp = c(100, 150, 129, 300))
new_hp$mpg  <- predict(fit, new_hp)

predict(fit, new_hp)


##################################

my_df  <- mtcars
my_df$cyl  <- factor(my_df$cyl, labels = c("four", "six", "eight"))
fit  <- lm(mpg ~ cyl, my_df)

#### Пример 1
# Скачайте набор данных - dataframe с двумя количественными переменными
# (вспомните при необходимости, как задавать разделитель и другие параметры функции read.table),
# постройте линейную регрессию, где - первая переменная - зависимая, вторая - независимая.
# В ответ укажите значения регрессионных коэффициентов сначала intercept затем  slope.

# Решение
df <- read.csv("/Users/talverinat/Documents/R-project/stepik/Анализ данных в R/3 week/dataset_11508_12.txt", sep="", header = F, dec = '.')
fit <- lm(df[[1]] ~ df[[2]], df)
fit$coefficients
# Альтернативное решение
df <- read.table("dataset_11508_12.txt", sep = ' ')
m <- lm(df[, 1] ~ df[, 2], df)
m$coefficients


#### Пример 2
# Воспользуемся уже знакомыми данными diamonds из библиотеки ggplot2.
# Только для бриллиантов класса Ideal (переменная cut) c числом карат равным 0.46 (переменная carat)
# постройте линейную регрессию, где в качестве зависимой переменной выступает price,
# в качестве предиктора - переменная  depth. Сохраните коэффициенты регрессии в переменную fit_coef.
# Памятка:
#> fit <- lm(mpg ~ disp + wt, mtcars)
#> fit$coefficients # коэффициенты модели

## Решение
df <- subset(diamonds, cut == "Ideal" & carat == 0.46)
fit_coef <- lm(price ~ depth, df)$coefficients

## Альтернативные решения
fit_coef <- lm(price ~ depth, diamonds, (diamonds$cut =='Ideal') & (diamonds$carat == 0.46))$coef

df <- diamonds[diamonds$cut == "Ideal" & diamonds$carat == 0.46, ]
fit <- lm(price ~ depth, df)
fit_coef <- fit$coefficients


#### Пример 3
# Напишите функцию regr.calc, которая на вход получает dataframe c двумя переменными.
# Если две переменные значимо коррелируют (p - уровень значимости для коэффициента корреляции Пирсона меньше 0.05),
# то функция строит регрессионную модель, где первая переменная - зависимая, вторая - независимая.
# Затем создает в dataframe новую переменную с назанием fit,
# где сохраняет предсказанные моделью значения зависимой переменной.
# В результате функция должна возвращать исходный dataframe с добавленной новой переменной fit.
# Если две переменные значимо не коррелируют, то функция возвращает строчку "There is no sense in prediction"


## Решение
regr.calc <- function(x) {
  a <- cor.test(x[[1]], x[[2]], method = "pearson")$p.value
  if(a < 0.05){
    x$fit <- lm(x[[1]] ~ x[[2]], x)$fitted.values
    return(x)
  }
  else{
    return("There is no sense in prediction")
  }
}
## Альтернативное
regr.calc <- function(sample_data) {
	cor_result <-  cor.test(~sample_data[[1]] + sample_data[[2]])
	if (cor_result$p.value < 0.05) {
		fit_model  <- lm(sample_data[[1]] ~ sample_data[[2]])
		sample_data$fit  <- fit_model$fitted.values
	return(sample_data)
	} else {
		return('There is no sense in prediction')
    }
}
my_df <-  iris[,1:2] # на вход подаем данные iris только с переменными Sepal.Length и Sepal.Width
regr.calc(iris[,1:2]) # переменные значимо не коррелируют
# [1] "There is no sense in prediction"

my_df <-  iris[,c(1,4)] # на вход подаем данные iris только с переменными Sepal.Length и Petal.Width
regr.calc(my_df) # переменные значимо коррелируют


#### Пример 4
# Постройте scatterplot по данным iris, сохранив его в переменную my_plot :
# Ось X - переменная Sepal.Width
# Ось Y -  переменная Petal.Width
# Цвет точек - переменная Species
# Также добавьте линейное сглаживание для каждой группы наблюдений по переменной Species.

my_plot <- ggplot(iris, aes(Sepal.Width, Petal.Width, col = Species, xlab = "Sepal.Width", ylab = "Petal.Width")) +
  geom_point() +
  geom_smooth(method = "lm")
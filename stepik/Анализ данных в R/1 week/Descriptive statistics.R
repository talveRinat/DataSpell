# урок 4

#Step 2: Data preprocessing

?mtcars

df  <- mtcars

str(df)

df$vs  <- factor(df$vs  , labels = c("V", "S"))
df$am  <- factor(df$am  , labels = c("Auto", "Manual"))


#Step 3: Descriptive statistics

median(df$mpg)
mean(df$disp)
sd(df$hp)
range(df$cyl)

mean_disp  <- mean(df$disp)

mean(df$mpg[df$cyl == 6])

mean(df$mpg[df$cyl == 6 & df$vs == "V"])

sd(df$hp[df$cyl != 3 & df$am == "Auto"])

# Пример
result <- mean(mtcars$qsec[mtcars$cyl != 3 & mtcars$mpg > 20])


#Step 5: Aggregation

?aggregate

mean_hp_vs  <- aggregate(x = df$hp, by = list(df$vs), FUN = mean)

colnames(mean_hp_vs)  <- c("VS", "Mean HP")

# разбиение ввиде формулы, сначала указываем переменную которую хотим разбить,
# далее указываем группирующию переменную, далее указываем данные откдуа берем
# и в конце указываем функцию которую хотим применить
aggregate(hp ~ vs, df, mean)

# хотим разбить с учетом двух переменных
aggregate(hp ~ vs + am, df, mean)
# все тоже самое
aggregate(x = df$hp, by = list (df$vs, df$am), FUN = mean)

# укажем все наблюдения кроме 8 и 9 колонки
aggregate(x = df[,-c(8,9)], by = list(df$am), FUN = median)
# df[, c(1, 3)] берем все строки и только 1 и 3 колонку
aggregate(df[,c(1,3)], by = list(df$am, df$vs), FUN = sd)

aggregate(cbind(mpg, disp) ~ am + vs, df, sd)
cbind(df$mpg, df$disp)

my_stats  <- aggregate(cbind(mpg, disp) ~ am + vs, df, sd)

# Пример
escriptions_stat <- aggregate(cbind(hp, disp) ~ am, mtcars, sd)


#Step 8, 9: Library "psych"

library(psych)

?describe

describe(x = df)

descr  <- describe(x = df[,-c(8,9)])

descr2  <- describeBy(x = df[,-c(8,9)], group = df$vs)

descr2$V
descr2$S

descr2  <- describeBy(x = df[,-c(8,9)], group = df$vs, mat = T, digits = 1)

descr3  <- describeBy(x = df[,-c(8,9)], group = df$vs, mat = T, digits = 1, fast = T)

describeBy(df$qsec, group = list(df$vs, df$am), digits = 1, 
           fast = T)


#Step 10: NA values

sum(is.na(df))

df$mpg[1:10]  <- NA

mean(df$mpg, na.rm = T)

aggregate(mpg ~am, df, sd)

describe(na.rm = )

# Задание
?airquality

# 1-ое
df <- subset(airquality, Month %in% c(7,8,9))
result <- aggregate(Ozone ~ Month, df, length)
# короткий вариант
result <- aggregate(Ozone ~ Month, airquality, subset = Month %in% c(7,8,9), length)

# 2-ое
# используем прошлый df  и точность 3 значка после запятой
describeBy(x = df, group = df$Month, mat = T, digits = 3)

# конкретные и точное значение
describeBy(airquality, airquality$Month == 8)[['TRUE']]['Wind','skew']

# 3-ье
?iris
# мое
describe(iris, fast = T)
# правильное
describe(iris)['sd']

# 4-ое
data(iris)
# мое
a <-  describeBy(iris, iris$Species == 'virginica')[['TRUE']]['mean']
sort(a$mean)

# правильное
sort(aggregate(. ~ iris$Species,iris[,1:4],subset=iris$Species %in% 'virginica', median), decreasing = T)
# или
 describeBy(iris, group = iris$Species)$'virginica'['median']

# 5-ое
?replace

# Дано
my_vector <- rnorm(30)
my_vector[sample(1:30, 10)] <- NA

# мое решение
fixed_vector <- my_vector
fixed_vector[is.na(my_vector)] <- mean(my_vector, na.rm = T)

# Правильное
fixed_vector <- replace(my_vector, is.na(my_vector), mean(my_vector, na.rm = T))
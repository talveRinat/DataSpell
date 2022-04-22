# урок 2

# Reading data

?read.table
?read.csv


mydata <- read.csv('/stepik/evals.csv')


# Summaries

head(mydata, 3)
tail(mydata)

View(mydata)

str(mydata)

a <- names(mydata)

summary(mydata)




# Variables

b <- mydata$score

mean(mydata$score)

summary(mydata$score)

mydata$score * 2

mydata$ten_point_scale <- mydata$score * 2



summary(mydata$ten_point_scale)

mydata$new_varible <- 0
mydata$number <- 1 : nrow(mydata)
summary(mydata$number)

nrow(mydata)
ncol(mydata)





# Subsetting

mydata$score[1:10]

mydata[1,1]
mydata[c(2,193,225),1]
mydata[101:200,1]

mydata[5,]
mydata[,1] == mydata$score

mydata[,2:5]
head(mydata[,2:5])

##


# Subsetting with condition

mydata$gender
mydata$gender == 'female'
head(mydata[mydata$gender == 'female',1:3])

head(subset(mydata, gender == 'female'))
head(subset(mydata, score > 3.5))



# rbind, cbind

mydata2 <- subset(mydata, gender == 'female')
mydata3 <- subset(mydata, gender == 'male')
mydata4 <- rbind(mydata2, mydata3)

mydata5 <- mydata[,1:10]
mydata6 <- mydata[,11:24]
mydata7 <- cbind(mydata6, mydata5)

data(mtcars)
help(mtcars)

my_data <- mtcars

# Задачи:
# Номер 1
# В этой задче поработаем со встроенными данными mtcars.
# В датафрэйме mtcars создайте новую колонку (переменную) под названием even_gear,
# в которой будут единицы, если значение переменной (gear) четное, и нули если количество нечетное.
# Мое решение
mtcars$even_gear <- as.integer(as.logical(mtcars$gear %% 2 != 1))

# Решение степика
mtcars$even_gear <- (mtcars$gear+1) %% 2
# или
mtcars$even_gear <- 1-mtcars$gear%%2

# Номер 2
# Продолжим нашу работу с данными mtcars.
# Теперь ваша задача создать переменную - вектор mpg_4 и
# сохранить в нее значения расхода топлива (mpg) для машин с четырьмя цилиндрами (cyl).

# Мое решение
mpg_4 <- subset(mtcars$mpg, mtcars$cyl == 4)
mpg_4

# Решение степика
mpg_4 <- mtcars$mpg[mtcars$cyl == 4]
# или
mpg_4 <- mtcars[ mtcars$cyl == 4, "mpg" ]

# Номер 3
# А теперь научимся отбирать только некоторые строчки из исходных данных.
# Ваша задача создать новый dataframe под названием mini_mtcars,
# в котором будут сохранены только третья, седьмая, десятая, двенадцатая и последняя строчка датафрейма mtcars.
mini_mtcars <- mtcars[c(3, 7, 10, 12, nrow(mtcars)), ]
nrow(mtcars)
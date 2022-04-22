# Title     : 2 урок
# темы      : многомерные массивы, apply, фактор,
# Created by: talverinat
# Created on: 17.02.2021

plot(Titanic)
str(Titanic)
sum(Titanic[,2,,]) #женщин
sum(Titanic[,1,,]) #мужчин

apply(Titanic, 2, sum) #по полу
apply(Titanic, 1, sum) # по классу
apply(Titanic, c(1, 4), sum) # выживаемость по классу


# в R нет массивов, все вектор

dim(1:20) #размерность вектора

# создадим матрицу
x <- 1:20
dim(x) <- c(4,5)
x[2, 3] # матрица
x[6] # вектор
x[,3] # столбец
str(x)

is.vector(x)
is.matrix(x)
is.array(x)

#вернем х в вектор
dim(x) <- NULL
is.vector(x)


cbind(2:6, 3:7) #укладывается в столбцы
rbind(2:6, 3:7) # в строки
rbind(a=2:6, n=3:7)

cbind(a=2:6, n=3:7)[8]


ocen <- sample(2:5, 20, rep=T)

ocen_word <- c("неуд", "удовл", "хор", "отл")
ocen_word[ocen-1]
#----------------------

# подраздел
# фактор - хранит категории

ocen_f <- as.factor(ocen_word)
ocen_f

levels(ocen_f)
str(ocen_f)

as.numeric(ocen_f)
as.character(ocen_f)

is.numeric(ocen_f)
is.character(ocen_f)
#---------------------

str(iris)
plot(iris)
plot(iris[, -5]) #убераем 5 элемент
plot(iris[, -5], col=as.numeric(iris[, 5])) # номер сорта преобзовался в число, а число в цвет
plot(iris[, -5], col=iris[, 5])

plot(1:10, pch=1:10) #pch - тип значка
plot(1:10, pch=LETTERS[1:10])

plot(iris[,1:2], pch=as.character(iris[,5]))
plot(iris[,1:2], pch=as.numeric(iris[,5]))

summary(iris)
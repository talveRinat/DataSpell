# Title     : Графика
# Objective : фактор, cut(), таблица, samplr(), plot()
# Created by: talverinat
# Created on: 03.03.2021

factor(c(3, 2, 4, 10))
ft <- factor(c('gh','ty','vb','gh'))
is.character(ft)
str(ft)

x <- runif(100)
a <- cut(x, 5)
str(a)

bally <- sample(20:100, 30, rep=T) # выборка из дискретного набора
plot(bally)

# правильнео решение
cut(bally, breaks=c(0, 56, 71, 86, 100), labels=c('неуд','удов','хор', 'отл'), right = F, include.lowest = T)

# тестирование
cut(c(20,56, 71, 87, 100), breaks=c(0, 56, 71, 86, 100), labels=c('неуд','удов','хор', 'отл'), include.lowest = T, right=F)


# нарисовать множество точек и раскрасить их от центра
n <- 200
x <- rnorm(n)
y <- rnorm(n)

plot(x, y, col = (sqrt(x^2 + y^2)*2+1), pch = 16)
plot(x, y, col = rainbow(10), pch = 16)


####
length(iris)
str(iris)

iris[30:35, ]

plot(iris[,1:2])

head(iris[ , 1])

str(iris[1])
str(iris[,1])

# так правильно
sum(iris[,1])
sum(iris[[1]])
sum(iris$Sepal.Length)


# выбрать где первый параметр меньше пяти в iris
iris[iris[,1] < 5, ]

plot(iris[,1:2], col=iris$Species)
plot(iris[1:4], col=iris[, 5])
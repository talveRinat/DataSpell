# Title     : 24.02.2021
# Objective : TODO
# Created by: talverinat
# Created on: 24.02.2021

apply(Titanic, 2 , sum)

# размер выборки
n <- 20

# количество выборок
nvyb <- 100

vv<-matrix(nrow = 100, ncol = 20)
vv[1:100,]<-runif(20)
# создать выборки из равноменых распределения
# https://ru.wikibooks.org/wiki/%D0%AF%D0%B7%D1%8B%D0%BA_%D0%BF%D1%80%D0%BE%D0%B3%D1%80%D0%B0%D0%BC%D0%BC%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F_R/%D0%93%D0%B5%D0%BD%D0%B5%D1%80%D0%B0%D1%86%D0%B8%D1%8F_%D1%81%D0%BB%D1%83%D1%87%D0%B0%D0%B9%D0%BD%D1%8B%D1%85_%D1%87%D0%B8%D1%81%D0%B5%D0%BB
# https://aakinshin.net/ru/posts/r-functions/

x <- runif(n * nvyb)
dim(x) <- c(n, nvyb)
dim(x)
mean_x <- apply(x,2 ,FUN=mean) # для каждого столбца
str(mean_x)
plot(mean_x)
summary(mean_x)
qt(c(0.025,0.975), n-1)  / sqrt(n*12) + 0.5

y <- rnorm(n*nvyb)
dim(y) <- c(n, nvyb)
mean_y <- apply(y,2 ,FUN=mean) # для каждого столбца
str(mean_y)
plot(mean_y)
summary(mean_y)

abline(h=(qt(c(0.025,0.975), n-1)  / sqrt(n) ) , col='red')


df <- read.csv2("staff.csv")
df1 <- read.table("staff.csv", sep=';', head = T, dec=',')
str(df)
summary(df)

write.csv(mean_x, "filename.csv")

dev.off()

sink()
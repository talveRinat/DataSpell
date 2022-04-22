# Title     : Проверка гипотез
# Objective : TODO
# Created by: talverinat
# Created on: 24.03.2021

n <- 20
x <- rnorm(n)

aa <- sample(LETTERS[1:3], 5, rep=T)
f_aa <- as.factor(aa)


# гипотеза о значениях
# одновыборочная гипотеза или двух выборочная

t.test(x)
t.test(x, alternative = 'greater')
t.test(x, alternative = 'less')


y <- rnorm(n)

t.test(x, y)

proverka <- function (x, y){
  pv <- t.test(x, y)$p.value
  paste("Среднее", ifelse(pv > 0.05, 'не', ''), 'отличается')
}

proverka(x, y)

y <- runif(n)
proverka(x, y)


res <- c("Среднее отличается", "Среднее не отличается")

pv <- 0.1
pv > 0.05
res[1+(pv>0.05)]


proverka2 <- function (x, y, res=c("Среднее отличается", "Среднее не отличается"), alfa = 0.05){
  pv <- t.test(x, y)$p.value
  res[1+(pv>alfa)]
}

proverka2(x, y)


proverka3 <- function (x, y, test, res=c("Среднее отличается", "Среднее не отличается"), alfa = 0.05){
  pv <- test(x, y)$p.value
  res[1+(pv>alfa)]
}

proverka3(x, y, t.test)

proverka4 <- function (x, y, test, res=c("Отвергаем Н0", "Принимаем Н0"), alfa = 0.05){
  pv <- test(x, y)$p.value
  res[1+(pv>alfa)]
}

proverka4(x, y, var.test, c('Дисперсия отличается', ' Дисперсия не отличается '))

apropos('.test')

y <- rnorm(n)
t.test(x, y)$p.value
t.test(x, y, paired = TRUE)$p.value

t.test(sort(x), sort(y))$p.value
t.test(sort(x), sort(y), paired = TRUE)$p.value


par(mfrow=c(1, 2))
plot(x, y)
plot(sort(x), sort(y))


# однофакторный дисперсионный анализ
aa3 <- sample(LETTERS[1:3], n, rep=T)
aa2 <- sample(LETTERS[1:2], n, rep=T)

t.test(x~aa2)
t.test(x~aa3)

oneway.test(x~aa3)
oneway.test(x~aa2)

wilcox.test(x~aa3)
wilcox.test(x~aa2)


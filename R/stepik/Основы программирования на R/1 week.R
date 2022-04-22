# Title     : TODO
# Objective : TODO
# Created by: talverinat
# Created on: 21.10.2020

log(sin(atan(1))^2, base=2)

divide <- function(x, y) {
return (x/y)
}
divide(2, 4)


# пункт 1
?help
?help.search
?"<-"
?ls
?"function"
?rm
?rnorm
?sort

# An obligatory one
print("Hello world!")
"Hello world!"

# Lost in translation
c(1, 2, 3) # ?!
c(3, 2, 1)

# Important caveat
0.1 + 0.1 == 0.2
0.1 + 0.05 == 0.15
# R FAQ 7.31
# google "R why are these numbers not equal"
# http://stackoverflow.com/questions/9508518/why-are-these-numbers-not-equal
?all.equal
all.equal(0.1 + 0.05, 0.15)

# Non-trivial sequence
u <- seq(0, 1, 1/3)
v <- (0:7)/7
c(u, v)
help.search("sort")
w <- sort(c(u, v))
w <- unique(w)
w

# fizz-buzz, imperative style
y <- vector(mode = "character", length = 100)
y <- character(100)
for (i in 1:100) {
  if (i %% 15 == 0) {
    y[i] <- "fizz buzz"
  } else if (i %% 3 == 0) {
    y[i] <- "fizz"
  } else if (i %% 5 == 0) {
    y[i] <- "buzz"
  } else {
    y[i] <- i
  }
}
y

# fizz-buzz, vector-oriented style
x <- 1:100
z <- 1:100
x %% 5
x %% 5 == 0
z[x %% 5 == 0]
z[x %% 5 == 0] <- "buzz"
z[x %% 3 == 0] <- "fizz"
z[x %% 15 == 0] <- "fizz buzz"
z
all(y == z)

# Geometric progression
x <- 2 ^ (0:10)
x
log2(x)

# Some randomness
set.seed(42)
x <- sample(1:100, 50)

# Neigbors with greatest diff
x[-1]
x[-length(x)]
x[-1] - x[-length(x)]
k <- which.max(abs(x[-1] - x[-length(x)]))
x[c(k, k + 1)]

# Multiple min/max
x <- sample(1:100, 50, replace = TRUE)
min(x)
which.min(x)
which(x == min(x))



get_fractions <- function(m, n) {
  a <- (0:m)/m
  b <- (0:n)/n
  w <- sort(c(a,b), decreasing = TRUE)
  w <- unique(w)
  return(w)
}

get_fractions(3, 7)

as.logical(-1:1)
as.numeric(as.logical(-1:1))

# пункт 2
?c
?":"
?seq
?rep
?print
?all.equal
?typeof
?is.logical
?as.logical
?length
?names

set.seed(1337)
x <- runif(1e6, min = -1, max = 1)

system.time({
sum(x>-0.2 & x<0.3)
})

system.time({
length(which(x>-0.2 & x<0.3))
})

# кубик рубик
dice_roll <- function(n) {
  x <- runif(n)
ifelse( x > 5/6, 1,
        ifelse(x > 4/6, 2,
               ifelse(x > 3/6, 3,
                      ifelse(x > 2/6, 4,
                             ifelse(x > 1/6, 5, 6)))))

}

dice_roll <- function(n) {
  as.integer(runif(n, 1, 7))
}

dice_roll <- function(n) {
    sample.int(6, n, replace = TRUE)
}

dice_roll <- function(n) {
  x <- runif(n, min = 1, max = 7)
  x <- as.integer(x)
  return(print(x))
}
dice_roll <- function(n){
  round(runif(n, 0.5, 6.5))
}
dice_roll(4)


library(grid)
grid.newpage()
grid.lines()

sessionInfo()

library(randtoolbox)
??randtoolbox
randtoolbox::shortintro
library(stats)
help(package = "stats")

x <- 1:14
x[-(seq(7, length(x), by = 7))]
x[-(1:floor(length(x)/7) * 7)]
x[c(rep(T, 6), F)]
x[1:length(x) %% 7 != 0]
x[1:length(x) %% 7 > 0]

# Кто не понял, попытаюсь объяснить. За основу возьму вектор x равный 11:20 (11 12 13 14 15 16 17 18 19 20)
# 1)x[1:length(x) %% 7 != 0]
# 1. 1:length(x)сначала создаётся вектор от 1 до length(x) (величина длины вектора). В нашем случае длина вектора равна 10, то есть получился вектор 1:10 (1 2 3 4 5 6 7 8 9 10).
# 2. Далее проверяем каждый элемент получившегося вектора на условие %% 7 != 0  , которое расшифровывается так — остаток от деления на 7, не равен 0.
# Проверяя вектор 1:10 на условие %% 7 != 0 получаем
# ( 1,   2,     3,   4,     5 ,  6 ,    7 ,    8,   9,    10)
#(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE)
# Следовательно вернётся вектор без элемента с индексом 7.
# (11 12 13 14 15 16 17 18 19 20) —> (11 12 13 14 15 16 18 19 20)

# 2)x[c(rep(T, 6), F)] (T —TRUE, F — FALSE)
# 1. rep(T, 6)создаёт вектор в котором 6 раз повторяется T (T T T T T T)
# 2. c(rep(T, 6), F) создаёт вектор, склеивая вектор (T T T T T T) и  F, получается (T T T T T T F)
# 3. Так как наш исходный вектор (11 12 13 14 15 16 17 18 19 20) длиннее чем вектор (T T T T T T F), для вектора(T T T T T T F) применяются правила переписывания. Получается вектор (T T T T T T F T T T).
#    1   2   3   4   5    6   7    8   9  10
# (  T   T   T   T   T   T   F    T   T   T )
# Следовательно вернётся вектор без элемента с индексом 7
# (11 12 13 14 15 16 17 18 19 20) —> (11 12 13 14 15 16 18 19 20)

# 3)x[-(1:floor(length(x)/7) * 7)]
# 1. floor(length(x)/7) функция floor возвращает самое большое целое число не превышающее аргумент. В нашем случае аргумент length(x)/7 и он равен 1.43, следовательно floor(1.43) - > 1
# 2. 1:floor(length(x)/7)создаётся вектор 1:1 (1) т.к. floor(1.43) - > 1
# 3. 1:floor(length(x)/7) * 7 каждый элемент вектора умножается на 7. В нашем случае вектор единичный (1) умножаем на 7 получается (7)
# 4. -(1:floor(length(x)/7) * 7) минус делает все элементы вектора (7) отрицательными индексами, а значит исключает их.
#   1  2  3  4  5  6  7  8  9  10
# (11 12 13 14 15 16 17 18 19 20)
# Следовательно вернётся вектор без элемента с индексом 7.
# (11 12 13 14 15 16 17 18 19 20) —> (11 12 13 14 15 16 18 19 20)

# 4) x[-(seq(7, length(x), by = 7))]
# 1. seq(7, length(x), by = 7) создаётся числовая последовательность которая начинается с 7 и заканчивается 10 (length(x)) с шагом 7 (by = 7). В нашем случае получается единичный вектор (7).
# 2. -(seq(7, length(x), by = 7)) минус делает все элементы вектора (7) отрицательными индексами, а значит исключает их.
#   1  2  3  4  5  6  7  8  9  10
# (11 12 13 14 15 16 17 18 19 20)
# Следовательно вернётся вектор без элемента с индексом 7.
# (11 12 13 14 15 16 17 18 19 20) —> (11 12 13 14 15 16 18 19 20)

# 5) x[1:length(x) %% 7 > 0]
# 1. 1:length(x)сначала создаётся вектор от 1 до length(x) (величина длины вектора). В нашем случае длина вектора равна 10, то есть получился вектор 1:10 (1 2 3 4 5 6 7 8 9 10).
# 2. Далее проверяем каждый элемент получившегося вектора на условие %% 7 > 0  , которое расшифровывается так — остаток от деления на 7, больше 0.
# Проверяя вектор 1:10 на условие %% 7 > 0 получаем
# (  1,    2,    3,    4,    5,    6,    7,    8,    9,    10)
# (TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE)
# Следовательно вернётся вектор без элемента с индексом 7.
# (11 12 13 14 15 16 17 18 19 20) —> (11 12 13 14 15 16 18 19 20)



check <- function(x) {
  print(max(x))
  print(which.max(x))
  print(which(x))
}

check(5)
check(1:10)
check(22:19)
check(c("A", "BBB", "Z"))
check(c("1", "99", "HI"))
check(c(TRUE, FALSE, FALSE, TRUE))

v1 <- 1:4
v2 <- 1:6
v3 <- v1 + v2
v3


d <- LETTERS
d
d[c(1, 23, 5, 19, 15, 13, 5)]

x <- c(0, 0, 3, 4, 4, 8)
k <- x[-1] - x[-length(x)]
a <- all(k >= 0)

y <- c(3:0, 1)
l <- y[-1] - y[-length(y)]
b <- all(l<0)

is_monotone <- function(x) {
  k <- x[-1] - x[-length(x)]
  return( (all(k>=0) | all(k<=0)) )
}

is_monotone <- function(x) {
    all(x == sort(x) | x == -sort(-x))
}

is_monotone <- function(x) {
    all(diff(x) >= 0) || all(diff(x) <= 0)
}
z <- c(10:1,3)

is_monotone(x)
is_monotone(y)
is_monotone(z)

x <- seq(10, 100, by = 10)
# Положительные индексы
# "Элементы с порядковыми номерами":
x[1]
x[3:4]
x[c(8, 7, 3, 6:8, x[1])]


# Отрицательные индексы

# "Все элементы, кроме указанных":

x[-5]
x[-(2:6)]
x[c(-3, -5, -length(x), -5)]


# Логические индексы
# "Элементы, соответствующие значению TRUE":
x[rep(c(TRUE, FALSE), 5)]
x[c(TRUE, FALSE)]
x[x > 77 & x < 99]


# Индексация по имени
a[c("two", "one", "forty two")]
a[c("forty two", "forty three", "forty four")]




combin_count <- function(n, k, with_repretitions = FALSE) {
if(with_repretitions == TRUE){
  factorial(n+k-1)/(factorial(k)*factorial(n-1))
}else{
  factorial(n)/(factorial(k)*factorial(n-k))
}
}


combin_count <- function(n, k, with_repretitions = FALSE) {
    ifelse(with_repretitions,choose(n+k-1, k),choose(n, k))
}


combin_count <- function(n, k, with_repretitions = FALSE) {
 return (choose(ifelse(with_repretitions, n+k-1, n), k))
}



combin_count <- function(n, k, with_repretitions = FALSE) {
  if (with_repretitions) {
    return( prod((k+1):(n+k-1))/prod(1:(n-1)) )
  } else {
    return( prod(((k+1):n))/prod(1:(n-k)) )
  }
}

combin_count(10, 5, FALSE)
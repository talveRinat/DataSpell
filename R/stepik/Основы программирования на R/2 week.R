# Title     : TODO
# Objective : TODO
# Created by: talverinat
# Created on: 22.10.2020


m <- 3
n <- 4
mat <- matrix(1:12, m, n)
mat[m, n]
mat[n, m]
mat[m, ]
mat[m, , drop = FALSE]
mat[, n, drop = F]
mat[, n, drop = TRUE]
mat > 5
mat[mat > 5]


v <- c(5, 2, 7, 7, 7, 2, 0, 0)
n <- 1

find_closest <- function(v, n) {
  e <- abs(v - n)
  return(which(e == min(e)))
}

find_closest <- function(v, n) {
  which(abs(v-n) == min(abs(v - n)))
}
find_closest(v, n)


bind_diag <- function(m1, m2, fill) {
  m3 <- matrix(fill,
               nrow = nrow(m1) + nrow(m2),
               ncol = ncol(m1) + ncol(m2))
  m3[1:nrow(m1), 1:ncol(m1)] <- m1
  m3[nrow(m1) + 1:nrow(m2), ncol(m1) + 1:ncol(m2)] <- m2
  m3
}

m1 <- matrix(1:12, nrow = 3)
m2 <- matrix(10:15, ncol = 3)
bind_diag(m1, m2, fill = NA)
bind_diag(m2, m1, fill = 0)

# самый медленный
zigguratCreator <- function(n){
  size <- 2*n-1
  out <- matrix(nr=size, nc=size)
  for(i in 1:size){
    for (j in 1:size){
      out[i,j] <-min(n-abs(i-n),n-abs(j-n))
    }
  }
  return(out)
}
# побыстрее но тоже медленно
ziggurat_Creator <- function(n){
  out <- mat.or.vec(nr = (n - 1)*2 + 1, nc = (n - 1)*2 + 1) + 1
  for(i in seq(n - 1)){
    out[seq(i + 1, nrow(out) - i), seq(i + 1, ncol(out) - i)] <- i + 1
  }
  return(out)
}

# быстро
build_ziggurat <- function(n) {
  d <- n * 2 - 1
  outer(1:d, 1:d, function(x,y) {
   x <- n - abs(n - x)
   y <- n - abs(n - y)
   pmin(x,y)
  })
}
# еще быстрее
build_ziggurat_mod <- function(n) {
  x <- n - abs(n - seq_len(2*n - 1))
  outer(x, x, pmin)
}

# самое быстрое построение
build_ziggurat_1 <- function(n) {
    m <- matrix(c(1:n,(n-1):1), 2*n-1, 2*n-1)
pmin(m,t(m))
}


system.time(build_ziggurat_mod(5000))
system.time(build_ziggurat(5000))
system.time(build_ziggurat_1(5000))
system.time(zigguratCreator(5000))
system.time(ziggurat_Creator(5000))

a <- 1
v <- 1:2
m <- matrix(1:9,nr=3,nc=3)
diag(m)
diag(v)
diag(a)
?diag()



numbers <- c(5, 2, 7, 7, 7, 2, 0, 0)
# мое решение
count_elements_1 <- function(numbers) {
  len <- length(unique(numbers))
  out <- matrix(nr=2, nc = len)
  out[nr = 1, 1:len] <- sort(unique(numbers))
  out[nr = 2, 1:len] <- as.numeric(table(sort(numbers)))
  out
}

v <- as.numeric(table(numbers))
v

# изящное решение
count_elements_2 <- function(numbers) sapply(sort(unique(numbers)), function (n) c(n, sum(numbers == n)))

# c rbind
count_elements_3 <- function(numbers) {
rbind(sort(unique(numbers)), sapply(sort(unique(numbers)), function(i) length(numbers[numbers==i])))
}

# с table
count_elements_4 <- function(numbers) {
  t <- table(numbers)
  rbind(as.numeric(names(t)), t)
}

# rle
count_elements_5 <- function(numbers) {
  l <- rle(sort(numbers))
  rbind(l$values,l$lengths)
}



set.seed(1789)
bastille <- list(
  "La Chapelle Tower" = rbinom(5, 10, 1/2),
  "Tresor Tower" = rbinom(8, 12, 1/4),
  "Comte Tower" = rbinom(14, 3, 1/5) + 1,
  "Baziniere Tower" = rbinom(8, 4, 4/5),
  "Bertaudiere Tower" = rbinom(4, 8, 2/3),
  "Liberte Tower" = rbinom(1, 100, 0.1),
  "Puits Tower" = rbinom(5, 5, 0.7),
  "Coin Tower" = rbinom(3, 16, 0.4)
)

b <- sapply(bastille, sum)
paste(c(names(which.min(b)), min(b), sum(b)), collapse = ", ")

bastille[names(which.min(sapply(bastille, sum)))]
sum(sapply(bastille, sum))

mat <- matrix(1:3, nr=3, nc=3)
as.data.frame(mat)

df <- data.frame(x = 1:4, y = LETTERS[1:4], z = c(T, F))
df
as.matrix(df)

str(attitude)

which.max(rowSums(attitude[order(-attitude$learning),][1:5,][c("complaints","raises","advance")]))
names(which.max(rowSums(attitude[order(attitude$learning,decreasing = T),][1:5,] [c("complaints","raises","advance")])))

#встроенный массив
a<-attitude
#сортируем (- по убыванию) по столбцы learning и выбираем первые 5 строк
b<-a[head(order(-a$learning),5),]
# можно без head
b<-a[order(-a$learning)[1:5],]
#считаем суммы других показателей
b$sm<-b$complaints+b$raises+b$advance
#сортируем и смотрим имя строки для первого
rownames(b[head(order(-b$sm),1),])



attitude[attitude$rating < 50, -"rating"]
subset(sel = -rating, sub = rating < 50, attitude)
subset(attitude, rating < 50, -rating)
attitude[attitude$rating < 50, names(attitude) != "rating"]
attitude[rating < 50, names(attitude) != "rating"]


str(quakes)
summary(quakes)
min(quakes$mag)
max(quakes$mag)
head(tail(quakes$stations, 2), 1)
median(quakes$depth)
sum(quakes$depth)/nrow(quakes)
tail(head(quakes$stations, 3), 1)


# мое решение
avian <- read.csv('https://raw.githubusercontent.com/tonytonov/Rcourse/master/R%20programming/avianHabitat.csv')
avian2 <- read.csv2(sep = ";", skip = 5, header = T, comment.char = "%", quote = "", na.strings = "Don't remember", file = 'https://raw.githubusercontent.com/tonytonov/Rcourse/master/R%20programming/avianHabitat2.csv')
names(avian2)
summary(avian2)
avian$Observer
avian2$Observer <- NA
new <- rbind(avian, avian2)
coverage_variables <- names(new)[-(1:4)][c(T, F)]
new$total_coverage <- rowSums(new[,coverage_variables])
summary(new$total_coverage)

# лучшее решение
avian <- read.csv("https://raw.githubusercontent.com/tonytonov/Rcourse/master/R%20programming/avianHabitat.csv")
coverage_variables <- names(avian)[-(1:4)][c(T, F)]
avian2 <- read.csv("https://raw.githubusercontent.com/tonytonov/Rcourse/master/R%20programming/avianHabitat2.csv",
                   sep = ";", na.strings = "Don't remember", skip = 5, comment.char = "%")
avian2$Observer <- "CL" # для красоты
avian3 <- merge(avian, avian2, all = T)
avian3$total_coverage <-  rowSums(avian3[coverage_variables])
summary(avian3)[4, "total_coverage"]




#library(dplyr)
#sort(sapply(df %>% select(ends_with("Ht")), max), decreasing = T)

height_variables <- names(avian)[-(1:5)][c(T, F)]
sort(sapply(avian[height_variables], max), decreasing = T)

# ?data.frame
# ?str
# ?rownames, ?colnames, ?dimnames, ?nrow, ?ncol, ?dim
# ?subset, ?rbind, ?cbind, ?merge
# ?read.table (?read.csv, ?read.delim)
# ?write.table (?write.csv, ?write.delim)
# ?complete.cases, ?na.omit
# ?write.table (?write.csv)


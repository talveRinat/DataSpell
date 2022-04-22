?replicate
?mapply
?outer
?do.call
Vectorize(existsFunction, "f")(paste0(letters, "apply"))


# Задача 1
cat_temper <- c("задиристый", "игривый", "спокойный", "ленивый")
cat_color <- c("белый", "серый", "чёрный", "рыжий")
cat_age <- c("кот", "котёнок")
cat_trait <- c("с умными глазами", "с острыми когтями", "с длинными усами")

# решение 1
cat_catalogue <- outer(outer(cat_temper,cat_color,paste),
                       outer(cat_age,cat_trait, paste),paste)
cat_catalogue <- sort(cat_catalogue)
cat_catalogue[42]

#решение 2
df <- expand.grid(cat_temper, cat_color, cat_age, cat_trait)
r <- sort(apply(df, 1, paste, collapse = " "))
r[42]




simulate_walk <- function(r = 6, x = 0, y = 0, n_max = 100, p = 0.01) {
  current_calc <- function(x, y) return(sqrt(x^2 + y^2))
  for (i in 1:n_max) {
    is_absorbed <- rbinom(1, 1, p)
    if (is_absorbed) return (1)
    x <- x + rnorm(1)
    y <- y + rnorm(1)
    if (current_calc(x, y) > r) return (2)
  }
  return (3)
}

# Simulate results
num_of_calls <- 100000
result <- replicate(num_of_calls, simulate_walk(), simplify = T)
chance_br <- length(which(result == 2))/num_of_calls

simulate <- function(r = 6, n=100, p=0.01){
  curr_x <- 0
  curr_y <- 0
  for (i in 1:n){
    if (rbinom(1,1,p)) return (0)
    curr_x <- curr_x +rnorm(1)
    curr_y <- curr_y +rnorm(1)
    if (((curr_x^2 + curr_y^2)^0.5) > r) return(1)
  }
  return(0)
}

res <- replicate(100000,simulate(),simplify = F)
print(do.call(sum,res)/100000)


funs <- c("print","summary","plot")
meths <- lapply(funs, methods)
grepl("matrix", meths)
grepl("function", meths)
grepl("default", meths)

g <- c('print', 'summary', 'plot')
f <- c('matrix', 'function', 'default')
df <- as.data.frame(lapply(f, function(y) sapply(g, function(x) sum(grepl(paste(x, y, sep = '.'), methods(x))))))
names(df) <- f
t(df)


f <- function(y) {
  y <- x + y
  y
}

g <- function(x) {
  y <- f(x)
  f <- function(x) {
    y - x
  }
  y - f(x)
}

x <- 10
y <- 1
f(x); f(y)
g(x); g(y)
x; y


m1 <- function(x, y) {
  m <- matrix(0, length(x), length(y))
  for (i in 1:length(x))
    for (j in 1:length(y)) {
      m[i, j] = x[i] * y[j]
    }
  m
}

m2 <- function(x, y) {
  vapply(y, function(i) i * x, numeric(length(x)))
}

m3 <- function(x, y) x %o% y

x <- rnorm(100)
y <- runif(1000)
all.equal(m1(x, y), m2(x, y))
all.equal(m2(x, y), m3(x, y))

library(microbenchmark)
microbenchmark(m1(x, y), m2(x, y), m3(x, y))
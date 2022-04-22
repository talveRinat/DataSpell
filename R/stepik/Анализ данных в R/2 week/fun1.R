# Урок 4


my_calc  <- function(x, y){
  s  <- x + y
  return(s)
}

my_calc(x = 10, y = 15)
result  <- my_calc(10, 15)

my_calc_2  <- function(x, y){
  s  <- x + y
  d  <- x - y
  return(c(s, d))
}

my_calc_2(10, 15)


my_calc_3  <- function(x, y, z){
  s  <- x + y + z
  d  <- x - y - z
  return(c(s, d))
}

my_calc_3  <- function(x, y, z = 10){
  s  <- x + y + z
  d  <- x - y - z
  return(c(s, d))
}



############

distr1  <- rnorm(100)
distr1[1:30]  <- NA
distr1[is.na(distr1)]  <- mean(distr1, na.rm = T)

my_na_rm  <- function(x){
  if (is.numeric(x)){
    stat_test  <- shapiro.test(x)
    if (stat_test$p.value > 0.05){
      x[is.na(x)]  <- mean(x, na.rm = T)
      print("NA values were replaced with mean")
    } else{
      x[is.na(x)]  <- median(x, na.rm = T)
      print("NA values were replaced with median")
    }
    return(x)
  } else{
    print("X is not numeric")
  }
}

d1  <- rnorm(2000)
d2  <- runif(2000)

d1[1:10]  <- NA
d2[1:10]  <- NA

d1  <- my_na_rm(d1)
head(d1)

d2  <- my_na_rm(d2)
head(d2)


######################

read_data  <- function(){
  df  <- data.frame()
  number  <<- 0
  for (i in dir(pattern = "*.csv")){
    temp_df  <- read.csv(i, stringsAsFactors = F)
    df  <- rbind(temp_df, df)
    number <<- number + 1
    }
  print(paste(as.character(number), "files were combined"))
  return(df)
}

# Advanced method without for loop

read_data_advanced <- function(){
    df <- do.call(rbind, lapply(dir(pattern = "*.csv"), 
                                read.csv, stringsAsFactors = F))
    return(df)
}

df  <- data.frame(x = factor(1:5))
df1  <- data.frame(x = factor(7:8))
str(df)
str(df1)

df3  <- rbind(df, df1)
str(df3)
table(df3$x)

# пример 1
# Напишите функцию, которая выводит номера позиций пропущенных наблюдений в векторе.
# На вход функция получает числовой вектор с пропущенными значениями.
#  Функция возвращает новый вектор с номерами позиций пропущенных значений.
# Подсказка: чтобы проверить является ли наблюдение NA, воспользуйтесь функцией is.na(),
#  кстати, функция векторизирована, и аргументом может служить вектор произвольной длинны.
#   Запись x == NA ни к чему осмысленному не приведет. Т.к. если x это NA, то команда x == NA также вернет NA, а не TRUE!

# my_vector <- c(1, 2, 3, NA, NA)
# NA.position(my_vector)
# [1] 4 5

NA.position1 <- function(x){
  which(is.na(x))
}

NA.position2 <- function(x){
 which(x%in% NA)
}

my_vector <- c(1, 2, 3, NA, NA)
NA.position1(my_vector)
NA.position2(my_vector)


# Пример 2
# Напишите функцию NA.counter для подсчета пропущенных значений в векторе.
# На вход функция  NA.counter должна принимать один аргумент - числовой вектор.
#  Функция должна возвращать количество пропущенных значений.

# my_vector <- c(1, 2, 3, NA, NA)
# NA.counter(my_vector)
# [1] 2

NA.counter1 <- function(x){
  length(which(is.na(x)))
}

NA.counter2 <- function(x){
return(sum(is.na(x)))
}

NA.counter1(my_vector)
NA.counter2(my_vector)

# Пример 3
# Напишите функцию filtered.sum, которая на вход получает вектор с пропущенными,
# положительными и отрицательными значениями и возвращает сумму положительных элементов вектора.

# filtered.sum(c(1, -2, 3, NA, NA))
# [1] 4

filtered.sum1 <- function(x){
  s <-  0
  x[is.na(x)] <- 0
  for(i in x){
    if(i >= 0){
      s <-  s + i
    }
  }
  return (s)
}


filtered.sum2 <- function(x)sum(x[x>0 & !(is.na(x))])

filtered.sum3 <- function(x){
    sum(ifelse(x > 0 & !is.na(x), x, 0))
}

filtered.sum4 <- function(x){
  return(sum(x[x > 0], na.rm = T))
}

my_vec <-  c(2, NA, NA, 0, NA, 1, -4, -1, 5, -3, 1, -1, 5, 1, -3, -1, -1)
filtered.sum1(my_vec)
filtered.sum2(my_vec)
filtered.sum3(my_vec)
filtered.sum4(my_vec)



# Пример 4
# Напишите функцию outliers.rm, которая находит и удаляет выбросы.
# Для обнаружения выбросов воспользуемся самым простым способом, с которым вы не раз встречались, используя график Box plot.
# Выбросами будем считать те наблюдения, которые отклоняются от 1 или 3 квартиля больше чем на 1,5 *  IQR, где  IQR  - межквартильный размах.
# На вход функция получает числовой вектор x. Функция должна возвращать модифицированный вектор x с удаленными выбросами.

IQR(x) # рассчитывает межквартильный размах вектора x

quantile(x, probs = c(0.25, 0.75)) # рассчитывает первый и третий квартиль вектора x


IQR(my_vec, na.rm = T)
a <- quantile(my_vec, probs = c(0.25, 0.75), na.rm = T)

a[1]
a[2]

outliers.rm1 <- function (x) {
  num <- IQR(x, na.rm = T)
  q <- quantile(x, probs = c(0.25, 0.75), na.rm = T)
  ind1 <- which(x < q[1] - 1.5*num)
  ind2 <- which(x > q[2] + 1.5*num)
  x <- x[-c(ind1, ind2)]
  return (x)
}

outliers.rm2 <- function(x) {
  q <- quantile(x, 0.25) + quantile(x, 0.75)
  return(x[abs(x - q/2) <= 2*IQR(x)])
}

outliers.rm1(my_vec)


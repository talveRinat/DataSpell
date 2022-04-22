# Title     : обработка пропущенный данных
# Objective : TODO
# Created by: talverinat
# Created on: 17.03.2021

x <- c(1:5, 1/0, sqrt(-1))
x[10]
x[10] <- 0
x[x == NA]
x == NA
x == Inf
is.na(x)

is.vector(x)
is.matrix(x)
dim(x) <- 10 # добавили размерность
is.matrix(x)

is.array(x)

is.nan(x)

nana <- c(NA, NaN, 0/0, 1/0)
is.na(nana)
is.nan(nana)
is.infinite(nana)

str(nana)

sum(is.na(x))

y <- x[is.na(x) == F]
y <- x[!is.na(x)]


numPeople <-  10
sex <- sample(c("male","female"),numPeople,replace=T)
age <-  sample(14:102, numPeople, replace=T)
income <-  sample(20:150, numPeople, replace=T)
minor <-  age < 18

population <-  data.frame(sex, age, income, minor, stringsAsFactors = T)
str(population)
summary(population)

population -> pop1
pop1[2,3] <- NA
pop1[4,2] <- NA
pop1[2, 2] <- NA
pop1

mean(is.na(pop1))

sign(sum(is.na(pop1[2, ])))

# сколько хороших строк
complete.cases(pop1)
mean(complete.cases(pop1))
# количество плохих строк
mean(!complete.cases(pop1))

pop1[complete.cases(pop1), ]

na.omit(pop1) # содержит информацию о пропущенных строках

na.action(na.omit(pop1)) -> na.pop
na.pop

na.omit(x)

pop1[na.pop,]   # "испорченные" строки


# ------
numPeople <-  100
sex <- sample(c(rep(c("male","female"),4),NA),numPeople,replace=T)
age <-  sample(18:90, numPeople, replace=T)
age[age > 85] <-NA
income <-  sample(seq(5:25)*10, numPeople, replace=T)
income[income>200] <- NA
income[is.na(sex)] <-c (NA,80,NA,100)

population <-  data.frame(sex, age, income,stringsAsFactors = T)

summary(population)
#-------------------------
mean(complete.cases(population)) # доля объектов с полной информацией

pop_bool <- as.data.frame(abs(is.na(population))) # 1 – пропущено, 0 -- нет

table(apply(pop_bool,1, sum)) # число объектов по числу пропусков

na_pop <- pop_bool[apply(pop_bool,2, sd) > 0] # (частично) неполные столбцы
cor(na_pop)
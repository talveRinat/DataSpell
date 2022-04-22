#Урок 1


# Categorical data

df <- read.csv("grants.csv")

str(df)


df$status <- as.factor(df$status)
levels(df$status) <- c("Not funded", "Funded")

df$status <- factor(df$status, labels = c("Not funded", "Funded"))


# 1d Table 
t1 <- table(df$status)
t1

dim(t1)


# 2d Table
t2 <- table(df$status, df$field)
t2
t2 <- table(status = df$status, field = df$field)

dim(t2)

prop.table(t2)

prop.table(t2, 1) # сумма в строчках равна 100%
prop.table(t2, 2) # сумма по столбцам равна 100%


# 3d Table
t3 <- table(Years = df$years_in_uni, Field = df$field, Status = df$status)
t3

dim(t3)



# plots

barplot(t1)
barplot(t2)
barplot(t2, legend.text = TRUE, args.legend = list(x = "topright"))
barplot(t2, legend.text = TRUE, args.legend = list(x = "topright"), beside = TRUE)

mosaicplot(t2)

##########################

# Binomial Test
binom.test(x = 5, n = 20, p = 0.5)
binom.test(t1)


# Chi-Square
t1
chisq.test(t1)

chi <- chisq.test(t1)
chi$exp
chi$obs


t2
chisq.test(t2)



# Fisher's Exact Test

fisher.test(t2)


# Пример 1
dimnames(HairEyeColor)
HairEyeColor[ , ,'Male']

red_men <- prop.table(HairEyeColor[, , 'Male'], 2)['Red', 'Blue']


# prop.table(HairEyeColor[ , 'Blue' ,'Male'])
# Получаем таблицу, в которой будут данные по всем голубоглазым мужчинам,
# сгруппированным по цвету волос (в процентах)
# [['Red']]
# и из полученного выбираем конкретное значение рыжеволосых.
# Поскольку нам нужна не таблица, а атомарное значение - двойные скобки.
red_men <- prop.table(HairEyeColor[ , 'Blue' ,'Male'])[['Red']]


# олд-скул решение
red_men <- HairEyeColor["Red", "Blue", "Male"] /
        sum(HairEyeColor[, "Blue", "Male"])


# Пример 2
green_female <- sum(HairEyeColor[, 'Green', 'Female'])

# Пример 3
library("ggplot2")
mydata <- as.data.frame(HairEyeColor)
obj <- ggplot(data = subset(mydata, Sex ==  "Female"), aes(x = Hair , y = Freq, fill = Eye)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))

?geom_bar

# Пример 4
brown_female <- as.data.frame(HairEyeColor['Brown', , 'Female'])
chisq.test(brown_female)

# Пример 5
?diamonds
str(diamonds)
chisq.test(table(diamonds$cut, diamonds$color))
main_stat <- chisq.test(table(diamonds$cut, diamonds$color))[1]
is.vector(main_stat)

# правильное решение
diamods_table <- table(diamonds$cut, diamonds$color)
chi_result <- chisq.test(diamods_table )
main_stat <- chi_result$statistic



# Пример 6

diamonds$factor_price <- factor(ifelse(diamonds$price >= mean(diamonds$price), 1, 0))
diamonds$factor_carat <- factor(ifelse(diamonds$carat >= mean(diamonds$carat), 1, 0))
main_stat_master <- chisq.test(diamonds$factor_price, diamonds$factor_carat)$statistic


# Пример 7
fisher_test <- fisher.test(table(mtcars$am, mtcars$vs))$p.value
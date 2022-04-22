# Урок 2

?iris
df  <- iris

str(df)

df1  <- subset(df, Species != "setosa")
str(df1)
table(df1$Species)

hist(df1$Sepal.Length)

ggplot(df1, aes(x =Sepal.Length ))+
  geom_histogram(fill = "white", col = "black", binwidth = 0.4)+
  facet_grid(Species ~ .)

ggplot(df1, aes(Sepal.Length, fill = Species ))+
  geom_density(alpha = 0.5)

ggplot(df1, aes(Species, Sepal.Length))+
  geom_boxplot()

shapiro.test(df1$Sepal.Length)

shapiro.test(df1$Sepal.Length[df1$Species == "versicolor"])
shapiro.test(df1$Sepal.Length[df1$Species == "virginica"])

#The same result in one line
by(df1$Sepal.Length, INDICES = df1$Species, shapiro.test)

bartlett.test(Sepal.Length  ~ Species, df1)


t.test(Sepal.Length  ~ Species, df1)
test1  <- t.test(Sepal.Length  ~ Species, df1)

str(test1)
test1$p.value

t.test(Sepal.Length  ~ Species, df1, var.equal = T)

t.test(df1$Sepal.Length, mu = 8)

t.test(df1$Petal.Length, df1$Petal.Width, paired = T)

ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.1)+
  stat_summary(fun.y = mean, geom = "point", size = 4)

ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", 
               size = 2)


?wilcox.test

test2  <- wilcox.test(Petal.Length ~ Species, df1)
pv  <- test2$p.value

ggplot(df1, aes(Species, Petal.Length))+
  geom_boxplot()


wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)

paired_wtest  <- wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)

paired_wtest$p.value


# Пример1
df  <- subset(ToothGrowth, (dose == 0.5 & supp == "OJ") | (dose == 2.0 & supp == "VC"))
t.test(len ~ supp, df)
t_stat <- t.test(len ~ supp, df)$statistic

#Сравните среднее значение длины зубов свинок, которые потребляли апельсиновый сок (OJ) с дозировкой 0.5 миллиграмм, со средним значением длины зубов свинок, которые потребляли аскорбиновую кислоту (VC) с дозировкой 2 миллиграмма.

?ToothGrowth
str(ToothGrowth)

hist(ToothGrowth$len)
#распределение с натяжкой нормальное

ggplot(ToothGrowth, aes(x = len ))+
    geom_histogram(fill = "white", col = "black", binwidth = 4)+
    facet_grid(supp ~ dose)
#распределение но дозами по соку и по витамину С. Видно, что сок более эффективен

ggplot(ToothGrowth, aes(len, fill = supp))+
    geom_density(alpha = 0.5)
#видно что эффект от сока больше

ggplot(ToothGrowth, aes(supp, len))+
    geom_boxplot()
#выбросов нет

shapiro.test(ToothGrowth$len)
by(ToothGrowth$len, INDICES = ToothGrowth$supp, shapiro.test)
#общий тест на нормальность пойден, а вот отдельно для группы потребляющей сок распределение нормальным назвать нельзя

hist(ToothGrowth$len[ToothGrowth$supp=='OJ'])
hist(ToothGrowth$len[ToothGrowth$supp=='VC'])
#проверяю визуально, и подтверждаю, что распределение для сока назвать нормальным сложно

bartlett.test(len  ~ supp, ToothGrowth)
#дисперисия гомогенна

#нализируем сабсет

by(df$len, INDICES = df$supp, shapiro.test)
#группы проходят тест на нормальность распределения

ggplot(df, aes(len, fill = supp))+
    geom_density(alpha = 0.5)
ggplot(df, aes(supp, len))+
    geom_boxplot()
#визуальный анализ подтверждает значимое различие

df <- rbind(subset(ToothGrowth, supp == 'OJ' & dose == 0.5), subset(ToothGrowth, supp == 'VC' & dose == 2))
t.stat <- t.test(len ~ supp, df)$statistic
#средние по группам значино различаются


# Пример 2
# По всем испытуемым сравните показатель давления до начала лечения (Pressure_before)
# с показателем давления после лечения (Pressure_after) при помощи t - критерия для зависимых выборок.
# В поле для ответа укажите значение t - критерия.
df2 <- read.csv("https://stepic.org/media/attachments/lesson/11504/lekarstva.csv")
str(df2)
# так как выборки зависимые указывает paired  True
t_ans <- t.test(df2$Pressure_before, df2$Pressure_after, paired = T)$statistic

# Пример 3
# Сначала с помощью теста Бартлетта проверьте гомогенность дисперсий двух выборок.
# В случае, если дисперсии значимо не отличаются (с уровнем 0.05), примените тест Стьюдента,
# иначе - непараметрический тест (Манна-Уитни).
# В поле для ответа введите получившийся p-value, с точностью четыре знака после запятой.
# Обратите внимание, что по умолчанию в t.test стоит var.equal = FALSE,
# так как мы будем применять его только в случае гомогенности дисперсий,
# измените значение этого параметра на  var.equal = TRUE.

df3 <- read.table("/Users/talverinat/Documents/R-project/stepik/Анализ данных в R/2 week/dataset_11504_15.txt",
               sep = " ",
               header = FALSE)

ifelse(bartlett.test(V1 ~ V2, df3)$p.value < 0.05,
       round(wilcox.test(V1 ~ V2, df3)$p.value, 4),
       round(t.test(V1 ~ V2, df3, var.equal = T)$p.value, 4))


# Пример 4
# В данных сохранены две количественные переменные,
# проверьте гипотезу о равенстве средних этих переменных при помощи t- теста для независимых выборок.
# Если обнаружены значимые различия (p < 0.05), то введите через пробел три числа:
# среднее значение первой переменной, среднее значение второй переменной, p - уровень значимости.
# Например: 22.45 12.56 0.04
# Если значимые различия не обнаружены, то в поле для ответа введите:
# "The difference is not significant"
# В этой задаче оставьте var.equal = FALSE

df4 <- read.table("/Users/talverinat/Documents/R-project/stepik/Анализ данных в R/2 week/dataset_11504_16.txt")
p_t_test <- t.test(df4$V1, df4$V2)$p.value
ifelse( p_t_test < 0.05,
       print(c(mean(df4$V1), mean(df4$V2), p_t_test )),
       print("The difference is not significant"))
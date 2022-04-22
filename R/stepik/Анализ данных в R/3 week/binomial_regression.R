# Финальный урок

library(ggplot2)

my_df <- read.csv("/Users/talverinat/Documents/R-project/stepik/Анализ данных в R/3 week/train.csv", sep=";")
str(my_df)

ggplot(my_df, aes(read, math, col = gender))+
  geom_point(size = 5)+
  facet_grid(.~hon)+
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=25,face="bold"))


fit  <- glm(hon ~ read + math + gender, my_df, family = "binomial")
summary(fit)

exp(fit$coefficients)

head(predict(object = fit))

head(predict(object = fit, type = "response"))

my_df$prob  <- predict(object = fit, type = "response")




library(ROCR)

pred_fit <- prediction(my_df$prob, my_df$hon)
perf_fit <- performance(pred_fit,"tpr","fpr")
plot(perf_fit, colorize=T , print.cutoffs.at = seq(0,1,by=0.1))
auc  <- performance(pred_fit, measure = "auc")
str(auc)



perf3  <- performance(pred_fit, x.measure = "cutoff", measure = "spec")
perf4  <- performance(pred_fit, x.measure = "cutoff", measure = "sens")
perf5  <- performance(pred_fit, x.measure = "cutoff", measure = "acc")

plot(perf3, col = "red", lwd =2)
plot(add=T, perf4 , col = "green", lwd =2)
plot(add=T, perf5, lwd =2)

legend(x = 0.6,y = 0.3, c("spec", "sens", "accur"), 
       lty = 1, col =c('red', 'green', 'black'), bty = 'n', cex = 1, lwd = 2)

abline(v= 0.225, lwd = 2)


my_df$pred_resp  <- factor(ifelse(my_df$prob > 0.225, 1, 0), labels = c("N", "Y"))

my_df$correct  <- ifelse(my_df$pred_resp == my_df$hon, 1, 0)


ggplot(my_df, aes(prob, fill = factor(correct)))+
  geom_dotplot()+
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=25,face="bold"))
  
mean(my_df$correct)


test_df  <- read.csv("test.csv", sep = ";")
test_df$hon  <- NA

test_df$hon  <- predict(fit, newdata = test_df, type = "response")
View(test_df)




##### Пример 1
# Используем данные mtcars. Сохраните в переменную логистическую регрессионную модель,
# где в качестве зависимой переменной выступает тип коробки передач(am), в качестве предикторов переменные disp, vs, mpg.

# Значения коэффициентов регрессии сохраните в переменную log_coef.
log_coef <- glm(am ~ disp + vs + mpg, data = mtcars, family = "binomial")$coefficients

# 1. Функция glm возвращает коэффициенты при уравнении регрессии,
# которые в дальнейшем могут быть использованы для предсказания значений Y (вопрос задания и ответ на него корректны)

# 2. Экспонента от коэффициента уравнения регрессии показывает,
# во сколько раз изменится значение Y при изменении Х на одну единицу.

#### Пример 2
# Дополните предложенный в задании код, чтобы построить следующий график по данным ToothGrowth.
# Изобразите различия длины зубов морских свинок в различных условиях дозировки и типа потребляемого продукта.

# По оси x - переменная supp.
# По оси y - переменная len.
# Цвет ящиков с усами (boxplot) - переменная dose.

library("ggplot2")
obj <- ggplot(data = ToothGrowth, aes(x = supp, y = len, fill = factor(dose)))+
  geom_boxplot()


#### Пример 3
# Используем модельные данные о соотношении среднего и высшего образования в американских школах.
# Данные доступны по ссылке: https://stepic.org/media/attachments/lesson/11478/data.csv
# Про часть испытуемых известно, поступили они в университет или нет (переменная admit, 1 = поступили, 0 = не поступили),
# про остальных таких данных нет (NA). Описание данных (обратите на него внимание при проведении подсчётов)


# По имеющимся данным в переменной admit постройте логистическую регрессионную модель,
# предсказывающую результат поступления по престижности учебного заведения среднего образования
# (переменная rank, 1 — наиболее престижное, 4 — наименее престижное) и результатов GPA (переменная gpa)
# с учётом их взаимодействия. Примените эту модель к той части данных, где результат поступления неизвестен.

# Ответом в задаче будет предсказанное моделью число поступивших из тех, для кого результат поступления был неизвестен.
# Считаем человека поступившим, когда вероятность его поступления не меньше 0.4.

dt <- read.csv("https://stepic.org/media/attachments/lesson/11478/data.csv")
str(dt)

fit  <- glm(admit ~ rank * gpa, dt, family = 'binomial' )

dt$pred <- predict(object = fit, type = "response", newdata = dt)
dt$res <- ifelse(is.na(dt$admit & dt$pred >= 0.4), 1, 0)
sum(dt$res)


#### Вывод данных

install.packages("stargazer")
library(stargazer)

install.packages("xtable")
library(xtable)

fit1  <- lm(mpg ~ cyl+disp, mtcars)
fit2 <- aov(mpg~am*vs,mtcars)

fit_table1 = xtable(fit1)
fit_table2 = xtable(fit2)

print(fit_table1, type = "html", file = "fit_table1.html")
print(fit_table2, type = "html", file = "fit_table2.html")

stargazer(fit1, type = "html",
          dep.var.labels = "mpg",
          covariate.labels = c("cyl","disp"), out = "models1.html")

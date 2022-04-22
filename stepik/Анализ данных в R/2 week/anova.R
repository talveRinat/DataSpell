# Урок 3

### ANOVA

library(ggplot2)

# formulae
?formula
# DV - зависимая переменная   IV - независимая переменная
DV ~ IV # One-way

DV ~ IV1 + IV2 # Two-way
#  : указвает о взаимодействии, сила влиение независимой переменой на зависимую зависит от другой независимой переменной
DV ~ IV1:IV2  # Two-way interaction

DV ~ IV1 + IV2 + IV1:IV2 # Main effects + interaction
#    *  это главный эффект плюс взаимодействии
DV ~ IV1 * IV2  # The same: Main effects + interaction

DV ~ IV1 + IV2 + IV3 + IV1:IV2
# ^2 двух факторное взаимодействие -> IV1+IV2 and IV2+IV3 and IV1+IV3
DV ~ (IV1 + IV2 + IV3)^2 # main effects and all possible interactions up to level 2
# межгрупповая + внутригрупповая
DV ~ IV1 + Error(subject/IV1) # repeated measures повторное измерение

# здесь немного про формулы с примерами: http://science.nature.nps.gov/im/datamgmt/statistics/r/formulas/
# https://www.statmethods.net/stats/anova.html

# Main Effect + interactions
DV ~ IV1 * IV2 ==  IV1 + IV2 + IV1:IV2

# Main effects and all possible interactions up to level 2
DV ~ (IV1 + IV2)^2 == IV1 + IV2 + IV1:IV2

# а как это правильно разложить???
DV ~ (IV1*IV2)^2 == (IV1 + IV2 + IV1:IV2)^2 == IV1 + IV2 + IV1:IV2

# reading data

mydata <- read.csv('../../../../../Downloads/shops.csv')


# One-way ANOVA

boxplot(price ~ origin, data=mydata)

ggplot(mydata, aes(x = origin, y = price)) + 
  geom_boxplot()



fit <- aov(price ~ origin, data=mydata)
summary(fit)


# Two-way ANOVA

fit1 <- aov(price ~ origin + store, data=mydata)
summary(fit1)

model.tables(fit1,"means")


# Interaction

pd = position_dodge(0.1)
ggplot(mydata, aes(x = store, y = price, color = origin, group = origin)) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.2, lwd = 0.8, position = pd)+  
  stat_summary(fun.data = mean_cl_boot, geom = 'line', size = 1.5, position = pd) +
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 5, position = pd, pch=15) +
  theme_bw()

fit3 <- aov(price ~ origin + store + origin:store, data=mydata)
summary(fit3)

fit4 <- aov(price ~ origin * store, data=mydata)
summary(fit4)



# Pairwise comparisons

ggplot(mydata, aes(x = food, y = price)) + 
  geom_boxplot()

fit5 <- aov(price ~ food, data=mydata)
summary(fit5)


TukeyHSD(fit5)




# Repeated measures

mydata2 <- read.csv('../../../../../Downloads/therapy_data.csv')
str(mydata2)

mydata2$subject <- as.factor(mydata2$subject)


fit1 <- aov(well_being ~ therapy, data = mydata2)
summary(fit1)
fit1b <- aov(well_being ~ therapy + Error(subject/therapy), data = mydata2)
summary(fit1b)


fit2 <- aov(well_being ~ therapy*price, data = mydata2)
summary(fit2)

ggplot(mydata2, aes(x = price, y = well_being)) + 
  geom_boxplot()

fit2b <- aov(well_being ~ therapy*price + Error(subject/(therapy*price)), data = mydata2)
summary(fit2b)

ggplot(mydata2, aes(x = price, y = well_being)) + 
  geom_boxplot() + 
  facet_grid(~subject)


fit3 <- aov(well_being ~ therapy*price*sex, data = mydata2)
summary(fit3)
fit3b <- aov(well_being ~ therapy*price*sex + Error(subject/(therapy*price)), data = mydata2)
summary(fit3b)


# Пример 1
# Воспользуемся встроенными данными npk, иллюстрирующими влияние применения различных удобрений на урожайность гороха (yield).
# Нашей задачей будет выяснить, существенно ли одновременное применение азота (фактор N) и фосфата (фактор P).
# Примените дисперсионный анализ, где будет проверяться влияние фактора применения азота (N),
# влияние фактора применения фосфата (P) и их взаимодействие.
# В ответе укажите p-value для взаимодействия факторов N и P.
str(npk)
ans1 <- aov(yield ~ N * P, data=npk)
summary(ans1)

cat("Оценка влияния фактора взаимодействия Pr(>F):",
    summary(ans1)[[1]]["N:P", "Pr(>F)"])

# Пример 2
# Теперь проведите трехфакторный дисперсионный анализ, где зависимая переменная - это урожайность (yield),
# а три фактора - типы удобрений (N, P, K).
# После проведения данного анализа вы получите три значения p - уровня значимости (о значимости каждого из факторов).

ans2 <- aov(yield ~ (N + P + K), data = npk)
summary(ans2)[[1]]["Pr(>F)"]


# Пример 3
# Проведите однофакторный дисперсионный анализ на встроенных данных iris.
# Зависимая переменная - ширина чашелистика (Sepal.Width), независимая переменная - вид (Species).
# Затем проведите попарные сравнения видов.
# Какие виды статистически значимо различаются по ширине чашелистика (p < 0.05)?

ans3 <- aov(Sepal.Width ~ Species, data=iris)
summary(ans3)

TukeyHSD(ans3)



# Пример 4
# В этой задаче вам дан набор данных, в котором представлена информация о температуре нескольких пациентов,
# которые лечатся разными таблетками и у разных врачей.
# Проведите однофакторный дисперсионный анализ с повторными измерениями:
# влияние типа таблетки (pill) на температуру (temperature) с учётом испытуемого (patient).
# Каково p-value для влияния типа таблеток на температуру?
# Не забудьте, важно перевести переменную patient в фактор!


df <- read.csv("https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv")
str(df)
df$patient  <- factor(df$patient)
ans4 <- aov(temperature ~ pill + Error(patient/pill), data = df)
summary(ans4)

summary(aov(temperature ~ pill + Error(as.factor(patient)/pill),
            data = read.csv(url("https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv"))))



# Пример 5

# Теперь вашей задачей будет провести двухфакторный дисперсионный анализ с повторными измерениями:
# влияние факторов doctor, влияние фактора pill и их взаимодействие на temperature.
# Учтите обе внутригрупповые переменные: и тот факт, что один и тот же больной принимает разные таблетки, и тот факт,
# что  один и тот же больной лечится у разных врачей!
# Каково F-значение для взаимодействия факторов доктора (doctor) и типа таблеток (pill)?

df5 <- read.csv("https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv")
df5$patient  <- factor(df5$patient)
ans5 <- aov(temperature ~ pill*doctor + Error(patient/(pill*doctor)), data = df5)
summary(ans5)



# Пример 6
# Вспомните графики из лекций и дополните шаблон графика в поле для ответа так (не добавляя еще один geom),
# чтобы объединить линиями точки, принадлежащие разным уровням фактора supp.
# Не забудьте подключить нужный для построение графика пакет.

# https://ggplot2.tidyverse.org/reference/geom_linerange.html

library(ggplot2)
library(Hmisc)
ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))
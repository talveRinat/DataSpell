install.packages("dplyr")
install.packages("caret")
install.packages("psych")
install.packages("ggplot2")

dt <- read.csv("Вар8.csv", header=T, na.strings=c("","NA"), sep=';')
head(dt)
str(dt)

name_na <- colnames(dt)[ apply(dt, 2, anyNA) ]
name_na

ind <- apply(dt, 1, function(x) sum(is.na(x))) > 0

dt[ind, name_na]

str(dt[name_na])

library("dplyr")

# функция на нахождения моды 
Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

mean_dt <- dt[, name_na] %>% mutate_if(is.numeric, funs(replace(.,is.na(.), mean(., na.rm = TRUE)))) %>%
  mutate_if(is.factor, funs(replace(.,is.na(.), Mode(na.omit(.)))))
    
mean_dt[ind, name_na]

# так как зависимая переменная должна быть числом а не фактором 
# то преобразуем пропущенные факторные значения в числовые 
dt[, name_na] <- lapply(dt[, name_na], function(x) as.integer(factor(x)))

dt$WBC <- as.numeric(dt$WBC)


fit <- lm(RBC ~ age + gender + Type , data = dt)
fit


library("caret")
pBag <- preProcess(dt[, name_na], method = 'bagImpute')
dt[, name_na] <- predict(pBag, dt[, name_na])
bag_dt <- dt[, name_na]
bag_dt[ind, name_na]

pKNN <- preProcess(dt[, name_na], method = 'knnImpute')
stand <- predict(pKNN, dt[, name_na])

stand[ind, name_na]

m <- pKNN$mean
sd <- pKNN$std
dt[, name_na] <- t(apply(stand, 1, function (r) m + r * sd))
knn_dt <- dt[, name_na]
knn_dt[ind, name_na]

# опишем все колонки кроме первой(так как там находится фамилии )
new_dt <- dt[-1]
summary(new_dt)

library("psych")
# или опишем другим способом 
psych::describe(new_dt)
        
# можем описать данные сгруппировав их по полу 
describeBy(new_dt, new_dt$gender)

# или можем сгруппировать их по типу болезни 
describeBy(new_dt, new_dt$Type)

library("ggplot2")
library("dplyr")
# так же можешь сгруппировать по трем признакам, по полу, возрасту и типу болезни. 
# Для примера сделаем просто подсчет   
by_age_gender_type <- new_dt %>% group_by(gender, Type, age)
by_gender <- by_age_gender_type %>% summarise(n = n(), .groups = 'drop')
by_gender

g <- ggplot(new_dt, aes(gender, age)) + 
    geom_point(aes(colour = factor(Type)), size = 4)
g

p <- ggplot(new_dt, aes(factor(Type)))+ 
    geom_bar(aes(fill = factor(gender)))
p


# удалим факторные показатели 
drop_cols <- c('gender', 'Type')
cor_dt <- new_dt %>% select(-one_of(drop_cols))

corPlot(cor_dt, cex = 1.2)

pairs.panels(cor_dt,
             smooth = TRUE,      
             scale = FALSE,      
             density = TRUE,     
             ellipses = TRUE,    
             method = "pearson", 
             pch = 21,           
             lm = FALSE,         
             cor = TRUE,         
             jiggle = FALSE,     
             factor = 2,         
             hist.col = 4,       
             stars = TRUE,      
             ci = TRUE) 

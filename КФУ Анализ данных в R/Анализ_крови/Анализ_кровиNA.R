# надо включить питон ядро 
%%writefile Анализ_кровиNA.R

library("openxlsx")
df <- read.xlsx("АнализКровиNA.xlsx", sheet = 11)

# если пропуски 
any(is.na(df))

str(df)

# Используем функцию complete.cases() для получения процента отсутствующих значений
nrow(df[!complete.cases(df), ])/nrow(df)*100

# функция производит замену пропусков
# type может быть  median, mean, min
impute <- function(data, type) {
  for (i in which(sapply(data, is.numeric))) {
    data[is.na(data[, i]), i] <- type(data[, i],  na.rm = TRUE)
  }
  return(data)}

df2 <- impute(df, median)


# остались ли пропуски 
any(is.na(df2))

nums <- unlist(lapply(df2, is.numeric))  
list_names <- colnames(df2[ , nums])[colnames(df2[ , nums]) != "age"]


# загрузим данные с нормами 
normq <- read.xlsx("АнализКровиNA.xlsx", sheet = 3)
colnames(normq)[colnames(normq) == "X1"] <- "gender"
normq


# функция которая вычисляет для каждого пациента и каждого анализа вычислите, выше он нормы, ниже или в пределах
f <- function(df_dan, df_norm){
    nums <- unlist(lapply(df_dan, is.numeric))
    list_names <- colnames(df_dan[ , nums])[colnames(df_dan[ , nums]) != "age"]
    list_normq <- colnames(df_norm)[colnames(df_norm) %in% list_names]

    men <- subset(df_dan, gender == 'male')
    women <- subset(df_dan, gender == 'female')

    y_m <- df_norm[c(1, 3),]
    y_w <- df_norm[c(2, 4),]

    for(r in 1:nrow(men)){
        for(c in list_normq){
            if(men[r, c] >= y_m[1, c] && men[r, c] < y_m[2, c]){
                men[r, c] <- 'mid'
            }else if(men[r, c] < y_m[1, c]){
                    men[r, c] <- 'low'
            }else if(men[r, c] >= y_m[2, c]){
                     men[r, c] <- 'hi'
            }
        }
    }

    for(r in 1:nrow(women)){
        for(c in list_normq){
            if(women[r, c] >= y_w[1, c] && women[r, c] < y_w[2, c] ){
                women[r, c] <- 'mid'
            }else if(women[r, c] < y_w[1, c]){
                    women[r, c] <- 'low'
            }else if(women[r, c] >= y_w[2, c]){
                     women[r, c] <- 'hi'
            }
        }
    }    
    res <- rbind(men, women)
        
    for(c in list_normq){
    res[, c] <- as.factor(res[, c])
}    
    return(res)
}

# новый датафрейм, где указано анализы выше, ниже или в норме 
res <- f(df2, normq)
str(res)
head(res)


# универсальная функция куда мы всталяем наши данные и критерий 
prov <- function(x, y, test, res=c("Отвергаем H_0", "Принимаем H_0"), alfa=0.05) {
    test(x,y)-> test_xy
    cat("Проверяется", test_xy$method,"\n")
    cat("На уровне", format(test_xy$p.value,dig=2),
        res[1+(test_xy$p.value>alfa)],"\n")
}

# Проверим, есть ли взаимосвязь данных с диагнозом ( с помощью критерия хи-квадрат)
x<- as.factor(res$Type)
for(z in list_names){
    cat(z)
    prov(x, res[, z], chisq.test)
    cat('-------------------------------------\n')
}


# Графики
for(nnn in list_names){
  chisq.test(df2$Type, df2[,nnn])$p.value->pv
  hist(df2[, nnn], main=nnn, sub=format(pv, dig=2),
        xlab="p-value", col.sub=1+(pv<0.05))
  qqnorm(df2[, nnn])
  abline(c(mean(df2[, nnn]), sd(df2[, nnn])), col=2)
}


        
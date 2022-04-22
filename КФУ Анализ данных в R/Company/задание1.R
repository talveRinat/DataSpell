# Title     : Пошаговое улучшение скрипта
# Objective : TODO
# Created by: talverinat
# Created on: 01.03.2021

# Создадим код шаг за шагом.
# NB. Могу я попросить вас не использовать пакеты? Не могу установить их на домашний ноутбук.
# Более того, лучше изучить базовый R, прежде чем использовать расширения.
# Задача содержит несколько версий R-кода, каждая из которых является улучшением предыдущей.
# Что делает исходный код? Что было улучшено? Пишите об этом после каждого скрипта
# (вы можете сделать это в файле Improve it.txt. Потом верните мне)


# Step to step
read.csv2("/Users/talverinat/Documents/R-project/КФУ Анализ данных в R/Company/Sinderella.csv")->data

sink("/Users/talverinat/Documents/R-project/КФУ Анализ данных в R/Company/Sinderella.txt")
   cat("Explore the correlation between age ana salary","\n")
   cor.test(data$age,data$salary)
sink()


pdf("/Users/talverinat/Documents/R-project/КФУ Анализ данных в R/Company/Sinderella.pdf")
   plot(data[2:3], col=factor(data$Department), pch= as.character(data$gender))
   boxplot(data[,2]~data[,4], xlab= "Department", ylab = "age")
   boxplot(data[,2]~data[,5], xlab= "gender", ylab = "age")
   boxplot(data[,3]~data[,4], xlab= "Department", ylab = "salary")
   boxplot(data[,3]~data[,5], xlab= "gender", ylab = "salary")
dev.off()
--------------------------------------------------------
# в файл sinderella.txt записаны резьлтат вычисления кореляции между возрастом и зарплатой
# в pdf записали один точечный график и 4 диаграммы размахов

# 1st improvment
------------------------------------------------
firmName <-"Sinderella"
directory <- "/Users/talverinat/Documents/R-project/КФУ Анализ данных в R/Company/"
read <- file.path(directory, paste0(firmName, ".csv"))
read.csv2(read)->data

sink(paste0(directory,firmName,".txt"))
   cat("Name of the company",firmName,"\n","\n")
   cat("Explore the correlation between",names(data[2]),
              "and",names(data[3]),"\n","\n")
   cor.test(data[,2], data[,3])
sink()

depNames <- levels(factor(data[,4]))

pdf(paste0(directory, firmName,".pdf"))
   plot(data[2:3], col=factor(data[,4]), pch= as.character(data[,5]))
   legend("topright", fill = 1:length(depNames), legend=depNames,
          col = 1:length(depNames), cex = 0.6)
   for (i in 2:3)
   for (j in 4:5)
   {
       boxplot(data[,i]~data[,j], xlab= names(data[i]), ylab = names(data[j]))
   }
dev.off()
# -----------------------------
# в txt файл добавили заголовок и название между чем была коррелция добавилась черз общащение по индексам
# в pdf  к точесному графику добавили легенду в правый вверхний угол, постороение не по именам п о индексу

# 2nd improvment
# ------------------------------------------------
firmName <- "Sinderella"
directory <- "/Users/talverinat/Documents/R-project/КФУ Анализ данных в R/Company/"

read.csv2(paste0(directory, firmName, ".csv"))->data

levels(factor(data[,4]))->depNames

sink(paste0(directory, firmName, ".txt"))

pdf(paste0(directory, firmName, ".pdf"))

   cat("Name of the company",firmName,"\n","\n")

   cat("Correlation between",names(data[2]),"and",names(data[3]),
          "is equal to", format(cor(data[,2],data[,3]),dig=2),"\n" ,"\n")

   cor.test(data[,2],data[,3])$p.value ->pvcor

   cat("There is",if (pvcor>0.05) {"no"} else {"the"},
             "correlation between", names(data[2]),"and",names(data[3]),
             "\n", "with p.value", format(pvcor, dig=2),"\n" )

   plot(data[2:3], col=factor(data[,4]), pch= as.character(data[,5]))
   legend("topright", fill = 1:length(depNames), legend = depNames,
                            col = 1:length(depNames), cex = 0.6)

   for (i in 2:3)
   for (j in 4:5) {
       oneway.test(data[,i]~data[,j])$p.value -> pvij
       inform<- paste("There is",if (pvij>0.05) {"no"} else{"some"},
                   "difference in",  names(data[i]),"by",names(data[j]))
       cat(inform,"\n","with p.value = ",format(pvij,dig=2),"\n")
       boxplot(data[,i]~data[,j], xlab= names(data[i]), ylab = names(data[j]))
       title(main = inform)
   }

sink()
dev.off()
# -----------------------------
# в txt  произвели корреляция между каждым столбцом
# в pdf  добавили заголовки для каждого "ящиков с усами"


# Several ideas. What are these commands for?
#---------------------------
rm(list=ls())
# удалить все предыдущие вычисления

#---------------------------
summary(data)
# выводит обобщенную информацию об объекте dаta;
# как правило, это набор статистических параметров, описывающих datа

#---------------------------
num <- sapply(data,is.numeric) # logical vector, TRUE means that column is numeric
num.data <- data[num] # choose only numeric columns
num[1] <- TRUE
char.data <- data[!num] # choose non-numeric columns, exept the first one

plot(num.data, col=factor(char.data[,1]), pch=as.character(char.data[,2]), main = firmName)
str(num.data)
str(data)

# Создаем новую таблицу из data в которую входит только столбцы с числовыми значениями и называем ее num.data
# строим стандартную диаграмму рассеивания
# по оси х и у  унас численные значение, в качестве обозначение точки используем чаровское значние обозначающие пол
# из таблицы char.data, а цветом обзначаем значение из какого департамента работник


# --------------------------
for(i in 1:2){
  for(j in 1:2){
    chisq.test(cut(num.data[,i],5), char.data[,j])$p.value->pvij
    print(pvij)
  }
}

# в pvij записывает значение p-value после выполнения функции критерия хи-квадрата,
# х и у оба могут быть факторными критериями или х - значение, а у фактор

# сначала проходит тест между возростом и департаментом, возрост полом;
# а потом проходит тест между зарплатой и департаментом, зарплатой и полом


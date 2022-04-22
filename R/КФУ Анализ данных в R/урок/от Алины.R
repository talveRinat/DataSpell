# Title     : TODO
# Objective : TODO
# Created by: talverinat
# Created on: 18.02.2021

x<-sample(LETTERS[1:5]) #выборка
?sample
x
x<-sample(LETTERS[1:5],10,rep=TRUE)
x
x<-sample(LETTERS[1:5],rep=TRUE)
x
x<-sample(LETTERS[1:5],10,rep=TRUE)
x[c(3,4,3,4,3)]
x
(1:10)[x=="A"]
x
which(x=="C")
x
x[x=="C"]
x
sum(x=="C")
mean(x=="C")


#создали скрипт отдельный
ocen<-sample(2:5,20,rep=TRUE)
ocen_word<-c("неуд","удовл","хор","отл")
ocen_word[ocen-1]

#Второй вариант
ocen_word2 <-  factor(ocen, labels = c("неуд","удовл","хор","отл"))

Titanic[,2,,] #данные о женщинах
sum(Titanic[,2,,]) #кол-во женщин
apply(Titanic,2,sum) #отдельно сумм ж и м
apply(Titanic,c(1,4),sum)


dim(1:20)
x<-1:20
dim(x)<-c(4,5)
x
x[2,3]
x[6] #ищет в векторе
x[,3]
str(x)
is.vector(x)
is.matrix(x)
is.array(x)
dim(x)<-NULL
dim(x)<-20
cbind(2:6,3:7)
rbind(2:6,3:7)
rbind(a=2:6,n=3:7)
cbind(a=2:6,n=3:7)[8]

#Фактор
ocen_f<-as.factor(ocen_word)
ocen_f
ocen_f<-as.factor(ocen_word[ocen-1])
ocen_f
levels(ocen_f)
str(ocen_f)
as.numeric(ocen_f)
as.character(ocen_f)
str(iris)
plot(iris)
plot(iris[,-5])
plot(iris[,-5],col=as.numeric(iris[,5]))
plot(iris[,-5],col=(iris[,5]))
plot(1:10, pch=1:10)
plot(1:10, pch=LETTERS[1:10])
plot(iris[,-5],pch=as.character(iris[,5]))
plot(iris[,1:2],pch=as.character(iris[,5]))
summary(iris)
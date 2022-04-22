

apply(iris, 2, mean) # так не получится 

apply(iris[1:4], 2, mean)
# лучше так для столбцов 
sapply(iris[1], mean)

# теперь получится 
sapply(iris[1], is.numeric)

# и можем взять теперь только числовые столбцы
iris[sapply(iris, is.numeric)]

# тест Стьдента
oneway.test(num_iris[, 1] ~ cat_iris[, 1])
# box-plot
plot(num_iris[, 1] ~ cat_iris[, 1])

# создадим цикл для печати ящика с усами
# и выводы статистики 
# сохраним в pdf
pdf('Iris.pdf')
for(num in names(num_iris))
for(cat in names(cat_iris)){
    pv <- oneway.test(num_iris[, num] ~ cat_iris[, cat])$p.value
    plot(num_iris[, num] ~ cat_iris[, cat], xlab=names(cat_iris[cat]), 
         ylab=format(pv, dig=2), main=i)
}
# закрыть последнее окно 
dev.off()

aggregate(num_iris, by=cat_iris, mean)

ff <- function(x) {oneway.test(x~cat_iris[, 1])$p.value}
sapply(num_iris, ff)


# так как у нас список то нужно обращаться к элементу по двойным скобках
txy[[5]][2]

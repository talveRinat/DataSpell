#  построим график что бы выяснить какое у нас распределние
df$X <-  sort(df$X)
n <-  length(df$X)  # размер выборки
plot(ecdf(df$X))
lines (df$X, (1:n)/n, type = 's', col="blue")
uniDF <- data.frame(X = unique(df$X),
                    r = as.vector(table(df$X)))

# по графику видно что у нас распределение Бернулли

# найдем значения p и q
p <- r[2] / n
q <- r[1] / n




vec <- df[['X']]
pp <-  with(rle(vec), lengths[values==1])/ n
qq <- with(rle(vec), lengths[values==0]) / n

summary(df$X)
# Дисперсия(смещённая оценка)
n <- length(df$X)
z <- (1/n * sum( (df$X - mean(df$X))^2 ))
z
# Дисперсия(не смещённая оценка), аналог R-функция var
y <- (1/(n-1) * sum( (df$X - mean(df$X))^2 ))
y

# урок 3

# control statements

mydata <- read.csv('/stepik/evals.csv')



# if

a <- -10

if (a > 0){
  print('positive')
} else {
  print('not positive')
}


if (a > 0){
  print('positive')
} else print('not positive')


if (a > 0){
  print('positive')
} else if (a < 0) {
  print('negative')
} else print('zero')



# ifelse

a <- 10


ifelse(a > 0, 'positive', 'not positive')

a <- c(1, -1)


# for 

for (i in 1:100){
  print(i)
}


for (i in 1:nrow(mydata)){
  print(mydata$score[i])
}



# for + if
for (i in 1:nrow(mydata)){
  if (mydata$gender[i] == 'male'){
    print(mydata$score[i]) 
  }
}



# for + if  VS  ifelse

mydata$quality <- rep(NA, nrow(mydata))

for (i in 1:nrow(mydata)){
  if (mydata$score[i] > 4){
    mydata$quality[i] <- 'good'
  } else mydata$quality[i] <- 'bad'
}




mydata$quality2 <- ifelse(mydata$score > 4, 'good', 'bad')




# while

i <- 1

while(i < 51){
  print(mydata$score[i])
  i <- i+1
}





# Задачи
data(mtcars)
# номер 1
# Создайте новую числовую переменную  new_var в данных mtcars, которая содержит единицы в строчках,
# если в машине не меньше четырёх карбюраторов (переменная "carb") или больше шести цилиндров (переменная "cyl").
# В строчках, в которых условие не выполняется, должны стоять нули.

# Решение
mtcars$new_var <- ifelse(mtcars$carb >= 4 | mtcars$cyl > 6, 1 , 0 )

# Номер 2
# В уже существующей переменной my_vector сохранен вектор из 50 чисел.
# Решите задачу используя конструкцию:
# if () {
#
# } else {
#
# }
# Если среднее значение вектора my_vector больше 20, в переменную result сохраните "My mean is great",
# если среднее значение my_vector меньше или равно 20 то в переменную result сохраните  строку "My mean is not so great".

my_vector <- c(20.67, 23.34, 22.65, 17.11, 22.1, 26.32, 20.39, 21.04, 23.78, 31.11, 21.13, 22.44, 23.21,
               27.02, 18.64, 20.9, 20.77, 20.0)
a <- mean(my_vector)

if (a > 20){
  result <- 'My mean is great'
} else result <- 'My mean is not so great'


# Номер 3
# В этой задаче от вас потребуется узнать некоторую информацию о типах данных в R самостоятельно!
# Встроенные в R данные AirPassengers - это новый для нас формат данных типа Time-Series.
# Изучите структуру этих данных, прежде чем начать решение задачи! Например напишите команды:
#
#> ?AirPassengers # справка о данных
#> str(AirPassengers) # структура данных

# В встроенных в R данных AirPassengers хранится 144 значения (количество пассажиров в месяц) с 1949 по 1960 год.
# Данные Time-Series очень похожи на вектор по своей структуре, например мы можем обратиться к любому из 144 элементов
# используя уже знакомую нам индексацию AirPassengers[1] или AirPassengers[56].
#
# Можно вообще перевести исходные данные в вектор при помощи команды as.vector(AirPassengers)
# и продолжить с ними работу как с вектором.
#
# И так ваша задача создать переменную good_months и сохранить в нее число пассажиров только в тех месяцах,
# в которых это число больше, чем показатель в предыдущем месяце.
#
# Важный момент! В R оператор : для создания последовательности имеет приоритет над арифметическими действиями.
# Таким образом, если у вас есть переменная i, равная 10, и вы хотите создать вектор от 1 до i - 1,
# воспользуйтесь скобками, чтобы указать последовательность действий.

?AirPassengers
str(AirPassengers)
help(AirPassengers)


good_months<-c()
for (i in 1:(length(AirPassengers)-1)) {
  if (AirPassengers[i+1]>AirPassengers[i]) {
    good_months<-append(good_months,AirPassengers[i+1])
  }
}

# Пример решения с циклом:
good_months <- c()
index <- 1
for (i in 2:length(AirPassengers)) {
	if (AirPassengers[i]>AirPassengers[i-1]){
		good_months[index] <- AirPassengers[i]
		index <- index + 1
		}
	}

# Пример правильного решения без цикла:
good_months <- AirPassengers[-1][AirPassengers[-1] > AirPassengers[-144]]


# номер 4
# Для встроенных в R данных AirPassengers рассчитайте скользящее среднее с интервалом сглаживания равным 10.
# Напечатайте получившийся результат (первым значением в выводе должно быть среднее для элементов 1:10,
# во втором значении - среднее для элементов 2:11 и т.д., в последнем  - среднее для элементов 135 :144)
# Все полученные значения средних сохраните в переменную moving_average.


# Пример правильного решения:
moving_average <- numeric(135) # создаем пустой числовой вектор из 135 элементов
last_index <- length(AirPassengers) - 9
for (i in 1:last_index) {
	end <- i + 9
	moving_average[i] <- mean(AirPassengers[i:end])
}

# Можно решить и без цикла при помощи разностей кумулятивных сумм!
n <- 10
d <- AirPassengers
cx <- c(0, cumsum(d))
moving_average <- (cx[(n + 1):length(cx)] - cx[1:(length(cx) - n)]) / n
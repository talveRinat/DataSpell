import random

k = int(input())
index_array = [i for i in range(0,k)]
c = 0
for j in range(10000):
    array = random.sample(range(0, k), k)
    if all(x == y for x,y in zip(index_array, array)):
        c += 1


print(c/10000)





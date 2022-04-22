
import random

fanti = []
for i in range(1,17):
    fanti.append(i)
kol = 0
for k in range(1000000):
    temp = random.sample(fanti,16)
    for i in range(16):
        temp[i] -= fanti[i]
    if all(temp):
        kol += 1
print('Шансы, что никому не достанется свой фант, равны ',kol/1000000 , 'из миллиона (1 000 000)')

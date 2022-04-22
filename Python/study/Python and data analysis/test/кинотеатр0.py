import random

sit = []
d = []
c = set()
l=0
N = 1000000
for i in range(1, 100000):
    sit.append((i, i+1))
answer = 0
for l in range(N):
    while len(sit) != 0:
        k = random.sample(sit, 1)
        ind = sit.index(*k)
        if k[0][0] in c:
            c.remove(k[0][0])
        if k[0][1] in c:
            c.remove(k[0][1])
        if ind == 0:
            d.append(*k)
            if len(sit) > 1:
                if sit[ind+1][0] - k[0][1] == 0:
                    c.add(sit[ind+1][1])
                    sit.pop(ind + 1)
            sit.pop(ind)
        elif ind == len(sit) - 1:
            d.append(*k)
            sit.pop(ind)
            if k[0][0] - sit[ind - 1][1] == 0:
                c.add(sit[ind-1][0])
                sit.pop(ind - 1)
        else:
            d.append(*k)
            if sit[ind+1][0] - k[0][1] == 0:
                c.add(sit[ind+1][1])
                sit.pop(ind + 1)
            sit.pop(ind)
            if k[0][0] - sit[ind - 1][1] == 0:
                c.add(sit[ind-1][0])
                sit.pop(ind - 1)

    answer += len(c)
print(answer)
print(answer / (100000 * N))
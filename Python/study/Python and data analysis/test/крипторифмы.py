from itertools import permutations
import time
"""
полезные ссылки:
https://habr.com/ru/post/189050/ 
https://ridero.ru/books/chislovye_rebusy/freeText
"""


def word_val(letter_values, power_of_ten):
    val = 0
    for i in range(len(letter_values)):
        val += letter_values[i] * power_of_ten[i]
    return val


if __name__ == "__main__":
    x = input("Первое слово: ").upper()
    y = input("Второе слово: ").upper()
    z = input("Сумма двух слов: ").upper()

    t0 = time.time()

    # Рассчитаем силу 10, которые соответствуют каждой букве.
    # чтобы сэкономить время при каждом вызове функции word_val.
    xProd = [10 ** i for i in range(len(x) - 1, -1, -1)]
    yProd = [10 ** i for i in range(len(y) - 1, -1, -1)]
    zProd = [10 ** i for i in range(len(z) - 1, -1, -1)]

    # Найдем все разные буквы, которые существуют в крипторифметическом уравнении.
    letters = []
    for i in x + y + z:
        if i not in letters:
            letters.append(i)

    # Если len(letters) > 10 решений не существует, потому что каждая буква должна иметь уникальное значение 0-9.
    num_of_letters = len(letters)
    if num_of_letters > 10:
        print("Нет Решения, слишком много разных букв.")
        exit()

    permList = (list(permutations([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], num_of_letters)))

    firstLetterIndex = [letters.index(x[0]), letters.index(y[0]), letters.index(z[0])]

    # Если да, то первая буква результирующего слова - 1.
    carryFlag = False
    if len(z) > len(x) and len(z) > len(y):
        carryFlag = True

    # printFlag: чтобы проверить, найдено ли решение или нет.
    # Добавление в список без необходимости замедляет работу программы.
    printFlag = False
    print(f"{x} + {y} = {z}")
    for perm in permList:

        if carryFlag and perm[firstLetterIndex[-1]] != 1:
            continue
        for let_index in firstLetterIndex[:-1]:
            if perm[let_index] != 0:
                continue

        x_val = [0] * len(x)
        y_val = [0] * len(y)
        z_Val = [0] * len(z)

        for i in range(num_of_letters):
            indices = [k for k, j in enumerate(x) if j == letters[i]]
            for j in indices:
                x_val[j] = perm[i]
            indices = [k for k, j in enumerate(y) if j == letters[i]]
            for j in indices:
                y_val[j] = perm[i]
            indices = [k for k, j in enumerate(z) if j == letters[i]]
            for j in indices:
                z_Val[j] = perm[i]

        xEquiv = word_val(x_val, xProd)
        yEquiv = word_val(y_val, yProd)
        zEquiv = word_val(z_Val, zProd)
        if xEquiv + yEquiv == zEquiv:
            xSol = ''.join(str(e) for e in x_val)
            ySol = ''.join(str(e) for e in y_val)
            zSol = ''.join(str(e) for e in z_Val)
            print(f"{xSol} + {ySol} = {zSol}")
            printFlag = True

    if not printFlag:
        print("Решений не найдено.")
        exit()

    t1 = time.time()
    print(f"\nВыполнено за: {round(t1 - t0)}s")

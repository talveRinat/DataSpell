import time


def solve(cryptarithm: str):
    starting_letter = set()
    not_starting_letters = set()
    in_word = False
    for c in cryptarithm:
        if str.isalpha(c):
            if not in_word:
                starting_letter.add(c)
                in_word = True
            else:
                not_starting_letters.add(c)
        else:
            in_word = False
    not_starting_letters -= starting_letter
    all_letters = (''.join(not_starting_letters) + ''.join(starting_letter))
    if len(all_letters) > 10:
        print('больше 10 букв, это не криптарифм.')
    for possible_sol in generate_possible_sol([str(i) for i in range(10)], len(starting_letter),
                                              len(not_starting_letters)):
        equation = cryptarithm.translate(str.maketrans(all_letters, possible_sol))
        try:
            if eval(equation):
                print(equation)
        except ArithmeticError:
            pass
        except:
            print('Неправильное выражение, это не криптарифм.')


def load(m, start, size, length):
    if length == 0:
        yield m
        if size > 1:
            if size % 2 or size == 2:
                m[start], m[start + size - 1] = m[start + size - 1], m[start]
            elif size == 4:
                m[start: start + 3], m[start + 3] = m[start + 1: start + 4], m[start]
            else:
                m[start: start + 2], m[start + 2: start + size - 2], m[start + size - 2], \
                m[start + size - 1] = m[start + size - 3: start + size - 1], \
                                      m[start + 1: start + size - 3], m[start + size - 1], m[start]
    else:
        size -= 1
        length -= 1
        for i in range(size):
            for m in load(m, start, size, length):
                yield m
            if size % 2:
                m[start + i], m[start + size] = m[start + size], m[start + i]
            else:
                m[start], m[start + size] = m[start + size], m[start]
        for m in load(m, start, size, length):
            yield m


def generate_possible_sol(m, size1, size2):
    adjusted_size2 = size2
    if size2 == 10 - size1:
        adjusted_size2 -= 1
    adjusted_size1 = size1
    if size1 == 9:
        adjusted_size1 -= 1
    start = 10 - size1 - size2
    for m in load(m, 1, 9, adjusted_size1):
        mm = list(m)
        for mm in load(mm, 0, 10 - size1, adjusted_size2):
            yield ''.join(mm[start:])


if __name__ == "__main__":
    #t0 = time.time()
    cryptarithm = 'ДВА * ДВА == ЧЕТЫРЕ'
    print(cryptarithm)
    solve(cryptarithm)
    #t1 = time.time()
    #print(f"\nВыполнено за: {round(t1 - t0)}s")

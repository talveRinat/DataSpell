from string import ascii_uppercase


def caesar(text, key, alphabet=ascii_uppercase):
    secret_word = ''
    for char in text.upper():
        if char in alphabet:
            secret_word += alphabet[(alphabet.find(char) + key) % len(alphabet)]
    return secret_word


# print(caesar(text='Ave, Caesar', key=3))


def jarriquez_encryption(text, key, alphabet='ABCDEFGHIJKLMNOPQRSTUVWXYZ', reverse=False):
    one = 1
    if reverse:
        one = -1

    key = list(map(int, str(key)))
    text = text.upper()
    L = len(alphabet)
    Lk = len(key)
    res = ''
    i = 0
    for s in text:
        if s.isalpha() or s.isdigit():
            ind = alphabet.index(s)
            res += alphabet[(ind + one * key[i % Lk]) % L]
            i += 1
    return res


text = 'ТЛБЛДУЭППТКЛФЧУВНУПБКЗИХТЛТТЫХНЛОИНУВЖММИНПФНПШОКЧЛЕРНТФНАХЖИДМЯКЛТУБЖИУЕЖЕАХЛГЩЕЕЪУВНГАХИЯШПЙАОЦЦПВТЛБФТТИИНДИДНЧЮОНЯОФВТЕАТФУШБЛРЮЮЧЖДРУУШГЕХУРПЧЕУВАЭУОЙБДБНОЛСКЦБСАОЦЦПВИШЮТППЦЧНЖОИНШВРЗЕЗКЗСБЮНЙРКПСЪЖФФШНЦЗРСЭШЦПЖСЙНГЭФФВЫМЖИЛРОЩСЗЮЙФШФДЖОИЗТРМООЙБНФГОЩЧФЖООКОФВЙСЭФЖУЬХИСЦЖГИЪЖДШПРМЖПУПГЦНВКБНРЕКИБШМЦХЙИАМФЛУЬЙИСЗРТЕС'
alphabet = ''.join(sorted(set(text)))
w1 = 'алмаз'.upper()
w2 = 'Дакоста'.upper()
for i in range(1000, 1000000):
    w1e = jarriquez_encryption(w1, i, alphabet)
    w2e = jarriquez_encryption(w2, i, alphabet)
    decr = jarriquez_encryption(text, i, alphabet, True)
    if (w1 in decr) and (w2 in decr):
        print(i, decr)

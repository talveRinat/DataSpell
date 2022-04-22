import random
from collections import UserList


class MyList(UserList):
    def __init__(self, info=None):
        UserList.__init__(self)
        if info is None:
            info = []
        self.info = info

    def add_iterable(self, iterable):
        for i in iterable:
            if i in self:
                print(f'{i} -  уже есть в списке.')
            else:
                UserList.append(self, i)
        return self

    def __add__(self, something_new):
        if hasattr(something_new, '__iter__'):
            return self.add_iterable(something_new)
        else:
            return UserList.append(self, something_new)

    def append(self, something_new):
        if something_new in self:
            print(f'{self} - уже есть в списке.')
        else:
            return UserList.append(self, something_new)

    def extend(self, something_new):
        return self.add_iterable(something_new)

    def __radd__(self, other):
        return other + str(self)

    def __len__(self):
        return len(self.info)

    def _mean(self):
        return sum(self.info, 0.0) / len(self.info)

    def _cmp(self, other):
        return len(self.info) - len(other.info)

    def __lt__(self, other):
        return self._cmp(other) < 0

    def __le__(self, other):
        return self._cmp(other) <= 0

    def __eq__(self, other):
        return self._cmp(other) == 0

    def __ne__(self, other):
        return self._cmp(other) != 0

    def __ge__(self, other):
        return self._cmp(other) >= 0

    def __gt__(self, other):
        return self._cmp(other) > 0

    def _sort(self):
        return sorted(self.info)

    def _median(self):
        n = len(self.info)
        if n < 1:
            return None
        if n % 2 == 1:
            return sorted(self.info)[n // 2]
        else:
            return sum(sorted(self.info)[n // 2 - 1:n // 2 + 1]) / 2.0

    def _join(self, l):
        l_tmp = [str(x) for x in l]
        return super(MyList, self)._join(l_tmp)


a_list = MyList([random.randrange(1, 10, 1) for i in range(3)])
a_list.__add__([random.randrange(1, 10, 1) for i in range(4)])
a_list.append(0)
a_list.extend([random.randrange(1, 10, 1) for i in range(7)])
print(a_list)

b_list = MyList([random.randrange(1, 50, 1) for i in range(7)])
print(len(b_list))
print(b_list._mean())
print(b_list._sort())
print(b_list._median())

print(b_list._join(a_list))

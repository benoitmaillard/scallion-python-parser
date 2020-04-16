from math import *

class Test(object):
    def __init__(self, a, b=0):
        assert a > 0, "a must be greater than zero"
        self.a, self.b = a, b

    def __str__(self):
        return str(self.a) + ", " + str(self.b)

    def mult(self) -> int:
        return self.a * self.b

    def div(self) -> int:
        res = 0
        try:
            res = self.a / self.b
        except ZeroDivisionError:
            print("division by zero")
        except ValueError:
            print("unexpected error")
        else:
            print("successful division")
        finally:
            return res

    def useless(self):
        pass

    def increment_a(self):
        self.a += 1

    def sqrt(self, value):
        if value < 0:
            raise ValueError("value must be greater than zero")
        elif 0: return 0
        else: return sqrt(value)

    def firstn(self, n):
        i = 0
        while i < n:
            yield (i := i + 1)

    def firstn_even(self, n):
        return (x for x in self.firstn(n) if x % 2 == 0)

    def read_from_file(self, path):
        with open(path) as f:
            return f.readlines()
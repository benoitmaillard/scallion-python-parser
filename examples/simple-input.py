import x as y
import x.x as y
from module.x import y
from ..x import y
from .... import *

pass
continue
break
raise SomeError(msg)

global a, b, c
nonlocal a, b, c

assert x != None, msg
return x

x, y, z = y, s, d = a, b, c; x = y
a = b
x = y if a else z
x = a and b
x = not x is not b
x = x < y < z == a
x = 1 + 4j
x = True
a.x = 5
x + 1 if True else (2 * test)
a = some_call(x, y)
a = x[5:3:4]
a = x[0, 3]
x = some_call()[4].some_other_call(x, y:=3, a=y, *args, **kwargs)()[i]
a, b, c = 1, 2, 3
a, b, c = (1, 2, 3)
a, b, c = (x + y for x in l if 1 < 2 if 1 < 3 for y in l2 if True)
a = (yield x)
a = (yield)
b = (yield from x)
a = [1, 2, 3]
x += 5
x: int
x: int = 5
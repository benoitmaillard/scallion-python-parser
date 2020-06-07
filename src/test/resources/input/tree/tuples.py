# atomParens
t = (1, 2, 3,)
t = (1, 2, 3)
e = (1) # normal value (no tuple)
t = (1,) # 1-tuple

[x for (x, y, z) in test]
[x for (x, y, z,) in test]
[x for (x,) in test]
[x for (x) in test]

for (x, y, z) in test: pass
for (x, y, z,) in test: pass
for (x,) in test: pass
for (x) in test: pass

x, y, z = 1, 2, 3
x, y, z, = 1, 2, 3
x, y, z = 1, 2, 3,
x = 1
x, = 1,
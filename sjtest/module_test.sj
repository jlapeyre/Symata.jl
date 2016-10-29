T Module([], 1) == 1

T Module([x],(Return(0);x)) == 0
T Module([x],(Return();x)) == Null

# This returns gensyms
T Not(Module([x],x) === Module([x],x))

# This returns the same variable.
T Module([x],y) === Module([x],y)

# Again, different gensym, each time the Module is evaluated.
 f := Module([x],x)
T  Not(f === f)

# Mma gives these variables the attribute 'Temporary'
T Module([x], (Attributes(x))) == [Temporary]

 x = 3
T Module([x=x], (x = x + 2)) == 5
T x == 3

 y = x^2
T Module([x=5], x + y) == 5 + x^2
T y == x^2

 ClearAll(f,x0,x)
 f(x0_) := Module([x = x0], ( While(x > 0, x = Log(x)), x))
T Chop(f(2.0) + 0.36651292058166435) == 0

# FIXME. Does not work. Maybe Mod is defined differently ?
# gcd(m0_, n0_) :=
#  Module([m = m0, n = n0, ntmp, cnt = 0],
#   (While(n != 0, (Increment(cnt), ntmp = n, n = Mod(m, n), m = ntmp, Println(m," ",n), If(cnt>10,Break()) )), m))

ClearAll(incr,x,y)

T ReplaceAll( incr(y), incr(x_) => Module([y = 1], x + y)) == y + 1

ClearAll(gcd, incr, f,g,x,y)

f(x_) := Module([], (If(x < 10, x*f(x+1), x),))
T f(1) == 3628800
T f(9) == 90
T f(10) == 10
T f("dog") == "dog"
 ClearAll(f,x)

 ClearTemporary()

@testex testUserSyms

@testex Module([], 1) == 1

@testex Module([x],(Return(0);x)) == 0
@testex Module([x],(Return();x)) == Null

# This returns gensyms
@testex Not(Module([x],x) === Module([x],x))

# This returns the same variable.
@testex Module([x],y) === Module([x],y)

# Again, different gensym, each time the Module is evaluated.
@ex f := Module([x],x)
@testex  Not(f === f)

# Mma gives these variables the attribute 'Temporary'
@testex Module([x], (Attributes(x))) == [Temporary]

@ex x = 3
@testex Module([x=x], (x = x + 2)) == 5
@testex x == 3

@ex y = x^2
@testex Module([x=5], x + y) == 5 + x^2
@testex y == x^2

@ex ClearAll(f,x0,x)
@ex f(x0_) := Module([x = x0], ( While(x > 0, x = Log(x)), x))
@testex Chop(f(2.0) + 0.36651292058166435) == 0

# FIXME. Does not work. Maybe Mod is defined differently ?
# gcd(m0_, n0_) :=
#  Module([m = m0, n = n0, ntmp, cnt = 0],
#   (While(n != 0, (Increment(cnt), ntmp = n, n = Mod(m, n), m = ntmp, Println(m," ",n), If(cnt>10,Break()) )), m))

# FIXME. Should return y + 1, but returns 2
# ReplaceAll( incr(y), incr(x_) -> Module([y = 1], x + y))

@ex ClearAll(gcd, f,g,x,y)
###

@ex f(x_) := Module([], (If(x < 10, x*f(x+1), x),))
@testex f(1) == 3628800
@testex f(9) == 90
@testex f(10) == 10
@testex f("dog") == "dog"
@ex ClearAll(f,x)

@ex ClearTemporary()
@testex testUserSyms


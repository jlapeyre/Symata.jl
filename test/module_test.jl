@ex ClearAll(f,g,x,y, gcd)

@testex Module([], 1) == 1

# This returns gensyms
@testex Not(Module([x],x) === Module([x],x))

# This returns the same variable.
@testex Module([x],y) === Module([x],y)

# Again, different gensym, each time the Module is evaluated.
@ex f := Module([x],x)
@testex  Not(f === f)

# Mma gives these variables the attribute 'Temporary'
# We give them no attribute.
@testex Module([x], (Attributes(x))) == List()

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

using Base.Test

@ex ClearAll(x,y,F,f,q)

@testex ExpToTrig(E^x) == Cosh(x) + Sinh(x)
@testex ExpToTrig(F^x) == F^x
@testex ExpToTrig(q + 1/f(Exp(x+y))) ==  q + f(Cosh(x + y) + Sinh(x + y))^-1

@ex ClearAll(x,y,F,q)

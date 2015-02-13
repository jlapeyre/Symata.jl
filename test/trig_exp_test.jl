using Base.Test

@ex ClearAll(a,x,y,F,f,q)

@testex ExpToTrig(E^x) == Cosh(x) + Sinh(x)
@testex ExpToTrig(F^x) == F^x
@testex ExpToTrig(q + 1/f(Exp(x+y))) ==  q + f(Cosh(x + y) + Sinh(x + y))^-1
@testex Log(1) == 0

@testex a + 1/Cos(x) == a + Sec(x)
@testex a + 1/Sec(x) == a + Cos(x)
@testex Sin(ASin(x)) == x
@testex Log(Sin(ASin(1))) == 0

@ex ClearAll(a,x,y,F,q)

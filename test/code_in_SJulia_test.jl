using Base.Test

@testex Length(UserSyms()) == 0

# These are tests for functions that are written in SJulia. The code
# that is tested is in code_in_SJulia.jl.
# eg ExpToTrig() is implemented as an SJulia DownValue and rule.
# This is separated because loading things like ExpToTrig causes a lot of jit
# and slows loading time considerably.

@ex ClearAll(a,x,y,F,f,q,z)

@testex ExpToTrig(E^x) == Cosh(x) + Sinh(x)
@testex ExpToTrig(F^x) == F^x
@testex ExpToTrig(q + 1/f(Exp(x+y))) ==  q + f(Cosh(x + y) + Sinh(x + y))^-1

# TODO make this work. Need to hash lhs of down values
# We currently can't make two rules like this because 1 == 1.0, and the
# solution is more complicated than choosing == or === in one place
# @test  @ex(Log(1)) === 0
# @test  @ex(Log(1.0)) === 0.0

@testex a + 1/Cos(x) == a + Sec(x)
@testex a + 1/Sec(x) == a + Cos(x)
@testex Sin(ASin(x)) == x

# TODO make this work. Need to hash lhs of down values
#@testex Log(Sin(ASin(1))) == 0

@testex  Sin(-z) == -1*Sin(z)
@testex  Sin(-z^2)^2 == Sin(z^2)^2

@ex ClearAll(a,x,y,F,q,z)

@testex Length(UserSyms()) == 0

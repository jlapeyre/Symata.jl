# These are tests for functions that are written in  The code
# that is tested is in code_in_jl.
# eg ExpToTrig() is implemented as an SJulia DownValue and rule.
# This is separated because loading things like ExpToTrig causes a lot of jit
# and slows loading time considerably.

 ClearAll(a,x,y,F,f,q,z)

T ExpToTrig(E^x) == Cosh(x) + Sinh(x)
T ExpToTrig(F^x) == F^x
T ExpToTrig(q + 1/f(Exp(x+y))) ==  q + f(Cosh(x + y) + Sinh(x + y))^-1

# TODO make this work. Need to hash lhs of down values
# We currently can't make two rules like this because 1 == 1.0, and the
# solution is more complicated than choosing == or === in one place
# T  (Log(1)) === 0
# T  (Log(1.0)) === 0.0

T a + 1/Cos(x) == a + Sec(x)
T a + 1/Sec(x) == a + Cos(x)
T Sin(ASin(x)) == x

# TODO make this work. Need to hash lhs of down values
#T Log(Sin(ASin(1))) == 0

T  Sin(-z) == -1*Sin(z)
T  Sin(-z^2)^2 == Sin(z^2)^2

 ClearAll(a,x,y,F,q,z)

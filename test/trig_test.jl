#### Cosh, ACosh

@testex testUserSyms

@testex ACosh(1) == 0
@testex Cosh(0) == 1
@testex Cosh(ACosh(1)) == 1
@testex ACosh(0) == Pi*I/2
@testex Cosh(Pi*I/2) == 0
@testex ACosh(1/2) == Pi*I/3

@testex D(Cosh(x),x) == Sinh(x)
@testex D(Cosh(x),[x,4]) == Cosh(x)
@testex Integrate(Cosh(x),x) == Sinh(x)

@ex ClearAll(x)
@testex testUserSyms




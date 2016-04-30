@ex If( Length(UserSyms()) > 0 ,  Println("\n**********", UserSyms(), "\n"))
@testex Length(UserSyms()) == 0

#### Erf

@testex Erf(0) == 0
@testex Head(Erf(0)) == Int
@testex Erf(DirectedInfinity(I)) == DirectedInfinity(I)

#### Gamma

@testex Chop(Gamma(.5) - 1.772453850905516) == 0
@testex Gamma(1/2) == Pi^(1/2)
@testex Gamma(0) == ComplexInfinity
@testex Chop(Gamma(1,.5) - 0.6065306597126334) == 0
#@testex isapprox(Gamma(.5), 1.772453850905516)  don't know if this is worth the trouble
# @testex 
# @testex
# @testex
# @testex
# @testex
# @testex
# @testex
# @testex
# @testex 




@ex If( Length(UserSyms()) > 0 ,  Println("\n**********", UserSyms(), "\n"))
@testex Length(UserSyms()) == 0


@testex testUserSyms

#### BigIntInput

@ex BigIntInput(True)
@testex   2^1000 != 0
@ex BigIntInput(False)
@testex   2^1000 == 0

#### BigFloatInput

@ex BigFloatInput(True)
@testex   2.0^1000 != Infinity
@ex BigFloatInput(False)
@testex   2.0^10000 == Infinity

## FIXME   1 < Infinity, etc. are not implemented

#### Erf

@testex Erf(0) == 0
@testex Head(Erf(0)) == Int
@testex Erf(DirectedInfinity(I)) == DirectedInfinity(I)
@testex Erf(I*Infinity) == DirectedInfinity(I)
@testex Erf(-Infinity) == -1
@testex Erf(-z) == -Erf(z)
@testex Conjugate(Erf(-z)) == -Erf(Conjugate(z))
@testex Args(Conjugate(Erf(-z))) == [-1,Erf(Conjugate(z))]

#### Gamma

@testex Chop(Gamma(.5) - 1.772453850905516) == 0
@testex Gamma(1/2) == Pi^(1/2)
@testex Gamma(3/2) == 1/2 * (Pi ^ (1/2))
@testex Gamma(0) == ComplexInfinity
@testex Gamma(1) == 1
@testex Gamma(4) == 6
@testex Chop(Gamma(1,.5) - 0.6065306597126334) == 0
#@testex isapprox(Gamma(.5), 1.772453850905516)  don't know if this is worth the trouble
@testex Gamma(1,2) == E^(-2)
@testex Gamma(a,0) == Gamma(a)
@testex Gamma(a, Infinity) == 0
@testex D(Gamma(x),x) == Gamma(x) * (PolyGamma(0,x))
@testex Gamma(3,x) == 2 * (E ^ (-x)) + 2 * (E ^ (-x)) * x + E ^ (-x) * (x ^ 2)
@testex Gamma(-1/2,x) == 2 * (E ^ (-x)) * (x ^ (-1/2)) + -2 * (Pi ^ (1/2)) * (1 + -Erf(x ^ (1/2)))
@testex Gamma(-2,x) == x ^ (-2) * (ExpIntegralE(3,x))
# @testex
# @testex
# @testex
# @testex 

# FIXME.
# Cutting and pasting the output of the Series
# is not equal to the output. The ordering of terms is different.
# No idea why.
@testex Series(Gamma(x), [x, 0, 3])[1] == -EulerGamma

# Its not clear this should be an automatic evaluation
@testex Conjugate(Gamma(x)) == Gamma(Conjugate(x))

@ex ClearAll(a,x,z)
@testex testUserSyms

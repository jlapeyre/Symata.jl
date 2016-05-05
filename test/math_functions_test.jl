
@testex testUserSyms

#### ArcSin

# FIXME N(ArcSin(3)) domain error. julia requires explicity comlex input. We need to check for this.

#### BigIntInput

@ex bigintval = BigIntInput(True)
@testex   2^1000 != 0
@ex BigIntInput(False)
@testex   2^1000 == 0
@ex BigIntInput(bigintval)
@ex ClearAll(bigintval)

#### BigFloatInput

@ex bigfloatval = BigFloatInput(True)
@testex   2.0^1000 != Infinity
@ex BigFloatInput(False)
@testex   2.0^10000 == Infinity
@ex  BigFloatInput(bigfloatval)
@ex ClearAll(bigfloatval)

## FIXME   1 < Infinity, etc. are not implemented

#### BesselJ

@testex Rewrite(BesselJ(nu,z), jn) == (2^(1/2))*(Pi^(-1/2))*(z^(1/2))*(SphericalBesselJ((-1/2 + nu),z))

#### CatalanNumber

@testex  Table(CatalanNumber(i), [i,10]) == [1,2,5,14,42,132,429,1430,4862,16796]
@testex  D(CatalanNumber(n),n) == CatalanNumber(n)*(Log(4) + -(PolyGamma(0,2 + n)) + PolyGamma(0,1/2 + n))
@testex  Rewrite(CatalanNumber(n), HypergeometricPFQ) == HypergeometricPFQ(([1 + -n,-n]),[2],1)
@testex  Rewrite(CatalanNumber(n), Gamma) == (4^n)*(Pi^(-1/2))*(Gamma(2 + n)^(-1))*Gamma(1/2 + n)
@testex  Rewrite(CatalanNumber(1/2), Gamma) == (8/3)*(Pi^(-1))
@testex  Rewrite(CatalanNumber(n), Binomial) == (Binomial((2n),n))*((1 + n)^(-1))
@testex  CombSimp(CatalanNumber(n+1)/CatalanNumber(n)) == (4^(-n))*(4^(1 + n))*(Gamma(1/2 + n)^(-1))*Gamma(3/2 + n)*((2 + n)^(-1))
# @testex  Rewrite(CombSimp(CatalanNumber(n+1)/CatalanNumber(n)), Binomial)  FIXME.
# FIXME. A number is converted to a float
#@testex  Rewrite(CatalanNumber(I), Gamma) == (0.183457 + 0.983028I)*(Pi^(-1/2))*(Gamma(2 + I)^(-1))*Gamma(1/2 + I)


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
# The first term in the || is for sympy < 1.0 (0.7 something). The second is for sympy 1.0
@testex ( Gamma(-1/2,x) == 2 * (E ^ (-x)) * (x ^ (-1/2)) + -2 * (Pi ^ (1/2)) * (1 + -Erf(x ^ (1/2))) ) ||
         ( Gamma(-1/2,x) ==  2(E^(-x))*(x^(-1/2)) + -2(Pi^(1/2))*Erfc(x^(1/2)))
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

@testex Conjugate(Gamma(x)) == Gamma(Conjugate(x))

#### HermiteH

@testex   HermiteH(0,x) == 1
@testex   HermiteH(1,x) == 2 * x
@testex   HermiteH(2,x) == -2 + 4*(x^2)
@testex   D(HermiteH(n,x), x) == 2*n*(HermiteH((-1 + n),x))
@testex   HermiteH(n,-x) == (-1)^n*(HermiteH(n,x))

@ex ClearAll(a,x,z)
@testex testUserSyms

# FIXME. this returns false. should return true
# c= Exp( Sin(Sqrt(2)) + BesselJ(3,4))
#  NumericQ(c)

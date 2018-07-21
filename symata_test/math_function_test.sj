### CubeRoot

T CubeRoot(8) == 2
T CubeRoot(-8) == -2
T CubeRoot(-8.0) == -2.0
T CubeRoot(8.0) == 2.0
T Abs(CubeRoot(-2.0) + 1.2599210498948732) < 10^-8
T Abs(CubeRoot(2.0) - 1.2599210498948732) < 10^-8
T CubeRoot(2) == 2^(1/3)
T CubeRoot(-2) == -2^(1/3)
T Head(CubeRoot(I)) == CubeRoot
T CubeRoot(2/3) == 2^(1/3)*3^(-1/3)
T CubeRoot(-2/3) == -2^(1/3)*3^(-1/3)
T CubeRoot(x) == Surd(x,3)
T CubeRoot(-x) == -Surd(x,3)
T CubeRoot(-x^2) == -Surd(x^2,3)
T CubeRoot(-2x^2) == -2^(1/3)*Surd(x^2,3)
T CubeRoot([-3, -2, -1, 0, 1, 2, 3]) == [-3^(1/3),-2^(1/3),-1,0,1,2^(1/3),3^(1/3)]

### Surd

T Surd(-2,5) == - 2^(1/5)
T Surd(-2.0,5) == -(2.0^(1/5))

### DirichletEta

### At present, we follow Mma with this, but may want to hold this unevaluated

T DirichletEta(s) == (1 - (2^(1 - s)))*Zeta(s)
T DirichletEta(1) == Log(2)
T DirichletEta(3) == (3/4)*Zeta(3)
T Abs(DirichletEta(3.0) - 0.9015426773696964) < 10^(-10)

### PolyLog

T Head(PolyLog(x,y)) == PolyLog
T Head(PolyLog(3,2)) == PolyLog
T Isa(PolyLog(3,2.0),Number)
T Isa(PolyLog(3.0,2),Number)
T PolyLog(s,1) == Zeta(s)
T PolyLog(1,z) == -Log(1-z)
T PolyLog(s,-1) == -DirichletEta(s)
T PolyLog(1,-1) == -Log(2)
T PolyLog(0,z) == z*((1 - z)^(-1))
T PolyLog(-1,z) == z*((-1 + z)^(-2))

### Abs

T  Abs(-1) == 1
T  Abs(-2.0) == 2.0
T  Abs(z^3) == Abs(z)^3
T  Abs(z^4.1) == Abs(z)^4.1
T  Abs(-z) == Abs(z)
T  Abs(-z^(1/2))^2 == Abs(z)
T  Abs(Pi) == Pi
T  Abs(E) == E
T  Abs(Sqrt(2)) ==  Sqrt(2)

#### ArcSin

# FIXME N(ArcSin(3)) domain error. julia requires explicity comlex input. We need to check for this.

#### BigIntInput

 bigintval = BigIntInput(True)
T   2^1000 != 0
 BigIntInput(False)
T   2^1000 == 0
 BigIntInput(bigintval)
 ClearAll(bigintval)

#### BigFloatInput

 bigfloatval = BigFloatInput(True)

## FIXME! reimplment this
#T   2.0^1000 != Infinity
 BigFloatInput(False)
T   2.0^10000 == Infinity
  BigFloatInput(bigfloatval)
 ClearAll(bigfloatval)

## FIXME   1 < Infinity, etc. are not implemented

#### Binomial

T CombSimp(Binomial(n+1, k+1)/Binomial(n, k)) == ((1 + k)^(-1))*(1 + n)

#### BesselJ

T Rewrite(BesselJ(nu,z), jn) == (2^(1/2))*(Pi^(-1/2))*(z^(1/2))*(SphericalBesselJ((-1/2 + nu),z))

# FIXME: translate jn somehow
ClearAll(jn)

#### CatalanNumber

T  Table(CatalanNumber(i), [i,10]) == [1,2,5,14,42,132,429,1430,4862,16796]
T  D(CatalanNumber(n),n) == CatalanNumber(n)*(Log(4) + -(PolyGamma(0,2 + n)) + PolyGamma(0,1/2 + n))
T  Rewrite(CatalanNumber(n), HypergeometricPFQ) == HypergeometricPFQ(([1 + -n,-n]),[2],1)
T  Rewrite(CatalanNumber(n), Gamma) == (4^n)*(Pi^(-1/2))*(Gamma(2 + n)^(-1))*Gamma(1/2 + n)
T  Rewrite(CatalanNumber(1/2), Gamma) == (8/3)*(Pi^(-1))
T  Rewrite(CatalanNumber(n), Binomial) == (Binomial((2n),n))*((1 + n)^(-1))
T  CombSimp(CatalanNumber(n+1)/CatalanNumber(n)) == (4^(-n))*(4^(1 + n))*(Gamma(1/2 + n)^(-1))*Gamma(3/2 + n)*((2 + n)^(-1))
T  CombSimp(Rewrite(CatalanNumber(n+1)/CatalanNumber(n), Binomial)) ==  2(1 + 2n)*((2 + n)^(-1))
# FIXME. A number is converted to a float
#T  Rewrite(CatalanNumber(I), Gamma) == (0.183457 + 0.983028I)*(Pi^(-1/2))*(Gamma(2 + I)^(-1))*Gamma(1/2 + I)

### Conjugate

T Conjugate(1) == 1
T Conjugate(I) == -I
T Conjugate(1-I) == 1 + I
T Head(Conjugate(a)) == Conjugate
T Conjugate(a+b+c)  == Conjugate(a) + Conjugate(b) + Conjugate(c)

#### EllipticE

T EllipticE(0) == (1/2)*Pi
T Series(EllipticE(z,m), [z,0,6]) == z + (-1/6)*m*(z^3) + ((1/30)*m + (-1/40)*(m^2))*(z^5) + Order((z^6),[z,0])
T Chop(EllipticE(2.0 - I) - (0.991052601328069 + 0.8187942139560901I)) == 0

#### EllipticF

T Series(EllipticF(z,m), [z,0,6]) == z + (1/6)*m*(z^3) + ((-1/30)*m + (3/40)*(m^2))*(z^5) + Order((z^6),[z,0])

#### EllipticK

T EllipticK(0) == Pi/2
T Chop(EllipticK(1.0 + I) - (1.509236954051273 + 0.6251464152026969I)) == 0
T Series(EllipticK(z), [z,0,3]) == (1/2)*Pi + (1/8)*Pi*z + (9/128)*Pi*(z^2) + Order((z^3),[z,0])

#### EllipticPi

T Series(EllipticPi(n,z,m), [z,0,4]) == z + ((1/6)*m + (1/3)*n)*(z^3) + Order((z^4),[z,0])

#### Erf

T Erf(0) == 0
T Head(Erf(0)) == Int
T Erf(DirectedInfinity(I)) == DirectedInfinity(I)
T Erf(I*Infinity) == DirectedInfinity(I)
T Erf(-Infinity) == -1
T Erf(-z) == -Erf(z)
T Conjugate(Erf(-z)) == -Erf(Conjugate(z))
T Args(Conjugate(Erf(-z))) == [-1,Erf(Conjugate(z))]

#### Factorial

T CombSimp(Factorial(n)/Factorial(n-3)) == n*(-2 + n)*(-1 + n)
T Head(Factorial(a)) == Factorial
T Map( Factorial, [-2,-3/2,-1,0,1/2,1,2,3,7/2]) == [-Infinity,-2(Pi^(1/2)),-Infinity,1,(1/2)*(Pi^(1/2)),1,2,6,(105/16)*(Pi^(1/2))]
# BigInt below
T Factorial(21) == 51090942171709440000
T Factorial(Infinity) == Infinity
T Factorial(-Infinity) == Indeterminate
T Factorial(I*Infinity) == 0
T Factorial(-I*Infinity) == 0
T Head(Factorial((I+1)*Infinity)) == Factorial

#### Gamma

T Chop(Gamma(.5) - 1.772453850905516) == 0
T Gamma(1/2) == Pi^(1/2)
T Gamma(3/2) == 1/2 * (Pi ^ (1/2))
T Gamma(0) == ComplexInfinity
T Gamma(1) == 1
T Gamma(4) == 6
T Chop(Gamma(1,.5) - 0.6065306597126334) == 0
#T isapprox(Gamma(.5), 1.772453850905516)  do not know if this is worth the trouble
T Gamma(1,2) == Exp(-2)
T Gamma(a,0) == Gamma(a)
T Gamma(a, Infinity) == 0
T D(Gamma(x),x) == Gamma(x) * (PolyGamma(0,x))
#T Gamma(3,x) == 2Exp(-x) + (x^2)*Exp(-x) + 2x*Exp(-x)
T Gamma(3,x) == 2 * (E ^ (-x)) + 2 * (E ^ (-x)) * x + E ^ (-x) * (x ^ 2)
# FIXME: Canonical order bug. The RHS above and below are not put in the same order, even when pasted at the CLI
#T Gamma(3,x) == 2 * (E ^ (-x)) +  E ^ (-x) * (x ^ 2) +  2 * (E ^ (-x)) * x
# The first term in the || is for sympy < 1.0 (0.7 something). The second is for sympy 1.0
T ( Gamma(-1/2,x) == 2 * (E ^ (-x)) * (x ^ (-1/2)) + -2 * (Pi ^ (1/2)) * (1 + -Erf(x ^ (1/2))) ) ||
         ( Gamma(-1/2,x) ==  2(E^(-x))*(x^(-1/2)) + -2(Pi^(1/2))*Erfc(x^(1/2)))
T Gamma(-2,x) == x ^ (-2) * (ExpIntegralE(3,x))

T CombSimp(Gamma(x)*Gamma(1-x)) == Pi*(Sin(Pi*x)^(-1))

T Gamma(3,5) == 37*Exp(-5)

T Series(Gamma(x), [x,0,3]) == -EulerGamma + x^(-1) + ((1/2)*EulerGamma^2 + (1/12)*Pi^2)*x + Order(x^3,[x,0]) + x^2*((-1/6)*EulerGamma^3 + (-1/12)*EulerGamma*Pi^2 + (1/6)*PolyGamma(2,1))

T Conjugate(Gamma(x)) == Gamma(Conjugate(x))

#### HermiteH

T   HermiteH(0,x) == 1
T   HermiteH(1,x) == 2 * x
T   HermiteH(2,x) == -2 + 4*(x^2)
T   D(HermiteH(n,x), x) == 2*n*(HermiteH((-1 + n),x))
T   HermiteH(n,-x) == (-1)^n*(HermiteH(n,x))

ClearAll(a,x,z)

#### IntegerDigits

T IntegerDigits(100) == [1,0,0]
T IntegerDigits(100,2) == [1,1,0,0,1,0,0]

#### Log

T Series(Log(1 + x), [x, 0, 5]) == x + (-1/2)*(x^2) + (1/3)*(x^3) + (-1/4)*(x^4) + Order((x^5),[x,0])
T Log(E) == 1
T Log(2,1024) == 10
T Log(3,3^(-12)) == -12
# FIXME Can't do these
# T Log(Pi,Pi^(1/2))
#
# FIXME: This should be -Infinity
T Log(0) == ComplexInfinity
# This should be Indeterminate
T Log(0.0) == -Inf
# This is what Mma does:
# Log(ComplexInfinity) == Infinity
T NumericQ(Log(1+E))
T Log(x,y) == Log(y)/Log(x)
T Log(2,8) == 3
T Log(2,9) == Log(9)/Log(2)

#### NDigits

T NDigits(10) == 2
T NDigits(99) == 2
T NDigits(2^10-1,2) == 10

ClearAll(x,y)

#### N

T Head(N(1,30)) == BigFloat
bigintval = BigIntInput(False)
T Head(N(1,16)) == Float64
BigIntInput(bigintval)
ClearAll(bigintval)

#### Sign

T Sign(1) == 1
T Sign(-1) == -1
T Sign(0) == 0
T Sign(1.0) == 1 && IntegerQ(Sign(1.0))
T Sign(I+1) == (1 + I)*(2^(-1/2))
T Abs(Sign(I+1)) == 1
T Sign((I+1)*x) == (1 + I)*(2^(-1/2))*Sign(x)
T Sign(-3*x*y) == -Sign(x*y)
T Sign(3.0*x*y) == Sign(x*y)
T Sign(E) == Sign(Pi) == Sign(EulerGamma) == 1
T Sign(I) == I
T Sign(x*I) == I * Sign(x)

ClearAll(x,y)

#### Max

T Max(1,2,3) == 3
T Max(Pi,E,Sqrt(2)) == Pi
T Max(x,x,x) == x
T Max(x,Max(x,y),z) == Max(x,y,z)

T Max( 1-x, x, 1 + x) == Max( 1 + x, 1 - x)

T Max() == -Infinity
T Max([1,2,3],[[8,9],4,5,6],7) == 9

T Table(i*j*k, [i, 3], [j, 3], [k, Max(i,j), 3]) == [[[1,2,3],[4,6],[9]],[[4,6],[8,12],[18]],[[9],[18],[27]]]
T Rest(FoldList(Max, 0, [4, 2, 8, 3, 9, 12, 11, 18, 10])) == [4,4,8,8,9,12,12,18,18]


### Min

T Min(1,2,3) == 1
T Min(Pi,E,Sqrt(2)) == Sqrt(2)

T Min() == Infinity

T Min([1,2,3],[[8,0],4,5,6],7) == 0

ClearAll(x,y,z)

# FIXME. this returns false. should return true
# c= Exp( Sin(Sqrt(2)) + BesselJ(3,4))
#  NumericQ(c)

T Plus() == 0
T Times() == 1

T Plus(a) == a
T Times(a) == a

### Singularities

T Singularities(x^2,x) == []
T Singularities(x/(x^2+3*x +2),x) == [x => (-2),x => (-1)]
T Singularities(1/(y^2 + 1),y) == [y => I,y => -I]
T Singularities(y/(y^3 + 1),y) == [y => (-1),y => (1/2 + (I * -1/2)*3^(1/2)),y => (1/2 + (I * 1/2)*3^(1/2))]
T Singularities(1/(y^2 + 2I*y + 1),y) == [y => (-I + I*2^(1/2)),y => (-I + -I*2^(1/2))]

### GammaRegularized

T GammaRegularized(0,0) == 0
T GammaRegularized(0,0.0) == 0.0
T GammaRegularized(0.0,0) == 0.0
T GammaRegularized(1,0) == 1
T GammaRegularized(1.0,0.0) == 1
T GammaRegularized(2.3,0.0) == 1
T GammaRegularized(2.3+I,0.0) == 1
T GammaRegularized(-2.3,0) == ComplexInfinity
T GammaRegularized(-2,0) == ComplexInfinity
T GammaRegularized(-2+I,0) == ComplexInfinity
T GammaRegularized(I,0) == Indeterminate
T GammaRegularized(2*I,0) == Indeterminate
## Mma does the following. But, for a = 0, this is wrong
## T GammaRegularized(-1,a) == 0
T  GammaRegularized(-1,1) == 0
T  GammaRegularized(-1,-1) == 0
T  Abs(GammaRegularized(2,1.5) - 0.5578254003710745) < 10^(-8)

### MittagLeffler

## FIXME: MittagLeffler has been removed. We will add it again
## with a package dependency, rather than including the numeric code here.
# T MittagLefflerE(1/2,z) == (E^(z^2))*(2 - Erfc(z))
# T MittagLefflerE(-1/2,z) == (E^(z^2))*Erfc(z)
# T MittagLefflerE(0,z) == (1 - z)^(-1)
# T MittagLefflerE(1,z) == E^z
# T MittagLefflerE(3,z) == (1/3)*(E^(z^(1/3)) + 2(E^((-1/2)*(z^(1/3))))*Cos((1/2)*(3^(1/2))*(z^(1/3))))
# T MittagLefflerE(4,z) == (1/2)*(Cos(z^(1/4)) + Cosh(z^(1/4)))
# ## Maybe the following is OK.
# #T MittagLefflerE(α,1,z) == MittagLefflerE(α,z)
# T MittagLefflerE(α,β,0) == Gamma(β)^(-1)
# T MittagLefflerE(1,2,z) == (-1 + E^z)*(z^(-1))
# T MittagLefflerE(2,2,z) == (z^(-1/2))*Sinh(z^(1/2))

# T MittagLefflerE(α,4,0) == 1/6
# T MittagLefflerE(2,5,0) == 1/24
# T MittagLefflerE(1,2,3) == (1/3)*(-1 + E^3)
# ## FIXME ! error
# ##T Abs(MittagLefflerE(1,2,3.0) - 6.361845641062556) < 10^(-8)
# T MittagLefflerE(1,1,3) == E^3
# T MittagLefflerE(2,1,5) == Cosh(5^(1/2))
# T MittagLefflerE(3,1,5) == (1/3)*(E^(5^(1/3)) + 2(E^((-1/2)*(5^(1/3))))*Cos((1/2)*(3^(1/2))*(5^(1/3))))
# T MittagLefflerE(4,1,5) == (1/2)*(Cos(5^(1/4)) + Cosh(5^(1/4)))

# T Head(MittagLefflerE(.6,x)) == MittagLefflerE
# T Head(MittagLefflerE(.6,.2,x)) == MittagLefflerE

# ## Limit of special forms

# T Map(Function(a, Limit(MittagLefflerE(a,1,z), z => 0)), [0,1,2,3,4]) == [1,1,1,1,1]

# mittest(α_, β_, z_) := Abs(MittagLefflerE(N(α), N(β), N(z)) - N(MittagLefflerE(α,β,z)))

# T mittest(3,1,5) < 10^(-14)

## FIXME: The LHS is not accurate. This is a problem with numerical integration
## T mittest(4,1,5)
## T mittest(2,1,5)

### Norm

ClearAll(a,b,c)

T Norm(1) == 1
T Norm(-1) == 1
T Norm(-I) == 1
T Norm(I - 2) == 5^(1/2)
T Norm(2) == 2
T Norm(-2) == 2
T Norm(-1/2) == 1/2
T Norm([1,2]) == 5^(1/2)
T Norm([1,I]) == 2^(1/2)
T Norm([a,b,c]) == (Abs(a)^2 + Abs(b)^2 + Abs(c)^2)^(1/2)

Apply(ClearAll, UserSyms())

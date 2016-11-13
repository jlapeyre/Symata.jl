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

T MittagLefflerE(1/2,z) == (E^(z^2))*(2 - Erfc(z))
T MittagLefflerE(0,z) == (1 - z)^(-1)
T MittagLefflerE(1,z) == E^z
T MittagLefflerE(3,z) == (1/3)*(E^(z^(1/3)) + 2(E^((-1/2)*(z^(1/3))))*Cos((1/2)*(3^(1/2))*(z^(1/3))))
T MittagLefflerE(4,z) == (1/2)*(Cos(z^(1/4)) + Cosh(z^(1/4)))
## Maybe the following is OK.
#T MittagLefflerE(α,1,z) == MittagLefflerE(α,z)
T MittagLefflerE(α,β,0) == Gamma(β)^(-1)
T MittagLefflerE(1,2,z) == (-1 + E^z)*(z^(-1))
T MittagLefflerE(2,2,z) == (z^(-1/2))*Sinh(z^(1/2))

T MittagLefflerE(α,4,0) == 1/6
T MittagLefflerE(2,5,0) == 1/24
T MittagLefflerE(1,2,3) == (1/3)*(-1 + E^3)
## FIXME ! error
##T Abs(MittagLefflerE(1,2,3.0) - 6.361845641062556) < 10^(-8)
T MittagLefflerE(1,1,3) == E^3
T MittagLefflerE(2,1,5) == Cosh(5^(1/2))
T MittagLefflerE(3,1,5) == (1/3)*(E^(5^(1/3)) + 2(E^((-1/2)*(5^(1/3))))*Cos((1/2)*(3^(1/2))*(5^(1/3))))
T MittagLefflerE(4,1,5) == (1/2)*(Cos(5^(1/4)) + Cosh(5^(1/4)))

## Limit of special forms

T Map(Function(a, Limit(MittagLefflerE(a,1,z), z => 0)), [0,1,2,3,4]) == [1,1,1,1,1]

mittest(α_, β_, z_) := Abs(MittagLefflerE(N(α), N(β), N(z)) - N(MittagLefflerE(α,β,z)))

T mittest(3,1,5) < 10^(-14)

## FIXME: The LHS is not accurate. This is a problem with numerical integration
## T mittest(4,1,5)
## T mittest(2,1,5)

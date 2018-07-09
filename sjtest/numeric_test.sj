ClearAll(approxeq, f, f1, x)

# Check that someone does not pick up the global (Symata) binding
# upgrade 0.7 . Why did the following happen ?
#x = 1.0

approxeq(x_, y_) := Abs(x-y) < 1.0*10^(-8)

# Different ways to give integrand to NIntegrate

# Issue #71
T  approxeq( NIntegrate(x, [0,3])[1], 4.5)
T  approxeq( NIntegrate(x, [x,0,3])[1], 4.5)

# Explicitly compile Symata to Julia function
## FIXME: method too new
#T  approxeq( NIntegrate(Compile([x], x^2), [0,2])[1], 8.0/3)

## FIXME: method too new
# ex = Compile([x], x^2)
# T  approxeq( NIntegrate(ex, [0,2])[1], 8.0/3)

# Wrap Symata expression
   f(x_) := x^2
T  approxeq(NIntegrate(f(x), [x,0,2])[1], 8.0/3)

# Wrap Symata expression
T  approxeq(NIntegrate(x^2, [x,0,2])[1], 8.0/3)

# Use Function compiled from Julia code
##  FIXME: method too new
#    f1 = J( x -> x^2)
# T  approxeq(NIntegrate(f1, [0,2])[1], 8.0/3)

# Test evaluation succeeds with deep expression.
# We forgot Attribute HoldAll
T (NIntegrate( (Exp(-x)^2 + x/(1+x))^2 + x , [x,0.0,2]), True)

T ( NIntegrate( Exp(-x), [x,0.0,2]), True)

a = 2.0

T approxeq(NIntegrate(Exp(-x)*x^(a-1), [x,0,Infinity])[1], 1)

mygamma(a_) := NIntegrate(Exp(-x)*x^(a-1), [x,0,Infinity])[1]

T approxeq(mygamma(3.0), Gamma(3.0))
T approxeq(mygamma(4.0), Gamma(4.0))
T approxeq(mygamma(5.0), Gamma(5.0))

ClearAll(f,f1,x)

# FIXME: Remove this. We require explicit parameter list.
# f = Compile(1/(1+x))
# f1 = Compile([x], 1/(1+x))
# T f(2) == f1(2)

f = Compile([x], E^x)

T approxeq(f(1), N(E))

f = Compile([x], Cos(Pi*x))

T approxeq(f(1), -1)

ClearAll(x)

T ToJuliaString( x^2 + Cos(x), NoSymata => False) == "mplus(mpow(x,2),Cos(x))"
T ToJuliaString( x^2 + Cos(x), NoSymata => True) == "x ^ 2 + cos(x)"

ClearAll(approxeq, f, f1, x, a, mygamma, ex)
ClearTemporary()

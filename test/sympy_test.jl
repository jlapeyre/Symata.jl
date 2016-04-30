using Base.Test

@testex Length(UserSyms()) == 0

@ex ClearAll(a,b,x,y,z,p,q,s,t,res,f)

#### Factor, Expand

@testex  Factor(Expand( (a+b)^2 )) == (a+b)^2
@testex  Expand( (a + f(x)) ^2 ) == a ^ 2 + 2 * a * f(x) + f(x) ^ 2
@ex      p = Expand((x-1)*(x-2)*(x-3)*(x^2 + x + 1))
@testex  p == -6 + 5 * x + -1 * (x ^ 2) + 6 * (x ^ 3) + -5 * (x ^ 4) + x ^ 5
@testex  Factor(p) == (-3 + x) * (-2 + x) * (-1 + x) * (1 + x + x ^ 2)

#### Cancel

@testex Cancel( (2*x^2-2)/(x^2-2*x+1)) == (-1 + x) ^ (-1) * (2 + 2 * x)

#### Limit

@testex Limit(x, x => 0) == 0
@testex Limit(Sin(x)/x, x => Infinity) == 0
@ex     f = :( fff(x) = x^10 )
@testex Limit( (f(x+h) - f(x))/h, h => 0) == 10 * (x^9)
# We need to fix this. Inf is a Float64. Convert it to SJulia
@testex Limit( 1/(x^(Log(Log(Log(Log((1/x)))))-1)), x => 0) == :( Inf )
# Mma 3 cannot do the following:
@testex Limit( Log(Log(x*Exp(x*Exp(x)) + 1)) - Exp(Exp(Log(Log(x)) + 1/x)), x => Infinity) == 0

#### Integrate

@ex ClearAll(r,y,x)
@testex Integrate(x,x) == 1//2 * x^2
@testex Integrate(x,[x,0,1]) == 1//2
@testex Integrate(x,[x,0,1],y) == 1//2 * y

@ex     r = Integrate(1/Cos(x + a), x)
# @testex r == Log(Sec(a + x) + Tan(a + x))  # if not loading code_in_SJulia.jl
@testex r == -1 * Log(-1 + Tan((1//2) * a + (1//2) * x)) + Log(1 + Tan((1//2) * a + (1//2) * x)) # are loading code_in_SJulia.jl

@testex Integrate(E^(-x^2),x) == (1//2) * (π ^ (1//2)) * Erf(x)
@testex Integrate(E^(x^2),x) == (1//2) * (π ^ (1//2)) * Erfi(x)

# Fixed, we can now return conditions as well.
@testex Integrate( Exp(-t)*t^(a-1),[t,0,Infinity], conds => "none") == Gamma(a)

# SymPy examples
@testex Integrate(x^2 + x + 1 ,x) == x + 1/2 * (x ^ 2) + 1/3 * (x ^ 3)
@testex Integrate( x/(x^2 + 2*x + 1), x) == Log(1 + x) + (1 + x) ^ (-1)
@testex Integrate( x^2 * Exp(x) * Cos(x), x) ==
        -1/2 * (E ^ x) * Cos(x) + 1/2 * (E ^ x) * (x ^ 2) * Cos(x) + 1/2 * (E ^ x) * Sin(x) + 1/2 * (E ^ x) * (x ^ 2) * Sin(x) + -(E ^ x) * x * Sin(x)
@testex Integrate( Exp(-x^2)*Erf(x), x) == 1/4 * (π ^ (1/2)) * (Erf(x) ^ 2)


@testex Integrate(x*y,x) == 1//2 * y * (x ^ 2)
@testex Integrate(Log(x),x) == -x + x * Log(x)
@testex Integrate(Log(x), [x, 1, a])  == 1 + -a + a * Log(a)
@testex Integrate(x) == 1//2 * (x ^ 2)    # Should we disallow this ?
@testex Integrate(Sqrt(1+x), [x,0,x]) == -2/3 + 2/3 * ((1 + x) ^ (3/2))
@testex Integrate(Sqrt(1+x), x) == 2//3 * ((1 + x) ^ (3//2))
@testex Integrate(x^a * Exp(-x), [x,0,Infinity]) == [Γ(1 + a),-Re(a) < 1]
@testex Integrate(x^a * Exp(-x), [x,0,Infinity], conds => "none") == Γ(1 + a)

# Following works if 1/cos does not go to sec
# @testex r == -1 * Log(-1 + Tan((1//2) * a + (1//2) * x)) + Log(1 + Tan((1//2) * a + (1//2) * x))

# Following works if 1/cos does not go to sec
#@testex Simplify(D(r,x)) == 1/Cos(x+a)
@ex ClearAll(r,y,x)

####  MellinTransform

# FIXME infinity unicode ∞ is not translated to Infinity on reading
@testex MellinTransform(Exp(-x),x,s) == [Γ(s),[0,Infinity],True]

####  InverseMellinTransform

@testex InverseMellinTransform(Gamma(s),s,x, [0,Infinity]) == Exp(-x)
@ex f = 1/(s^2 - 1)
@testex InverseMellinTransform(f,s,x,[-Infinity,-1]) == HeavisideTheta(-1 + x) * (-1/2 * (x ^ (-1)) + 1/2 * x)
@ex ClearAll(f,s,x)

#### LaplaceTransform

# Use these if noconds=false
# @testex LaplaceTransform(t^a,t,s) == [(s ^ -1) * (s ^ (-1 * a)) * Gamma(1 + a),0,-1 * Re(a) < 1]
# @testex LaplaceTransform(Cos(t),t,s) == [s * ((1 + s ^ 2) ^ -1),0,True]
# @testex LaplaceTransform(Exp(3*t),t,s) == [(-3 + s) ^ -1,3,Unequality(((1//3) * s),1)]  # TODO: translate

@testex LaplaceTransform(t^a,t,s) == (s ^ -1) * (s ^ (-1 * a)) * Gamma(1 + a)
@testex LaplaceTransform(Cos(t),t,s) == s * ((1 + s ^ 2) ^ -1)
@testex LaplaceTransform(Exp(3*t),t,s) == (-3 + s) ^ -1

#### InverseLaplaceTransform

@testex InverseLaplaceTransform(1/s,s,t) == HeavisideTheta(t)
@testex InverseLaplaceTransform(Exp(-a*s)/s, s, t) == HeavisideTheta(t + -a)

#### FourierTransform

@testex FourierTransform( E^(-x^2), x, k) == E ^ (-(k ^ 2) * (π ^ 2)) * (π ^ (1/2))
@testex InverseFourierTransform(E ^ (-(k*Pi) ^ 2) * (π ^ (1/2)), k, x) == E ^ (-(x ^ 2))

#### SineTransform

@testex SineTransform(x*Exp(-a*x^2), x, k) == 1/4 * (2 ^ (1/2)) * (a ^ (-3/2)) * (E ^ (-1/4 * (a ^ (-1)) * (k ^ 2))) * k

@ex ClearAll(x,k,a)

#### HankelTransform

@ex ht = HankelTransform(1/r^m, r, k, nu)
@testex  ht == 2 * (2 ^ (-m)) * (k ^ (-2 + m)) * Γ(1 + 1/2 * nu + -1/2 * m) * (Γ(1/2 * m + 1/2 * nu) ^ (-1))
@testex  InverseHankelTransform(ht,k,r,nu) == r^(-m)

@ex ClearAll(r,m,k,nu,ht)

#### D

@testex D(x^2,x) == 2 * x
@testex D(x^3,x,2) == 6 * x
# FIXME Exp is not translated properly
# We convert Exp(x) = E^x in an apprules. Probably better to convert it
# postprocessing sympy output or something. Anyway, the tests pass
@testex  D(Exp(x),x) == Exp(x)
@testex  D(Exp(Exp(x)),x) == Exp(x) * Exp(Exp(x))
@testex  D(ArcTan(x),x) == (1 + x ^ 2) ^ -1
@testex  D(BesselJ(1,x),x) == (1//2) * (BesselJ(0,x)) + (-1//2) * (BesselJ(2,x))

#### Together, Apart

@ex     z = ( 1/x + 1/(x+1))
@testex Together(z) == (x ^ -1) * ((1 + x) ^ -1) * (1 + 2 * x)
@testex Apart(Together(z)) == z

## Simplify

@testex Simplify( Cos(x)^2 + Sin(x)^2) == 1

## FullSimplify

@testex FullSimplify( -Sqrt(-2*Sqrt(2)+3)+Sqrt(2*Sqrt(2)+3) ) == 2

## Solve

@ex      res = Solve([x+y-1, x - y + 1], [x,y])
@testex  res[x] == 0
@testex  res[y] == 1
@testex  Solve(x^4-1,x) == [-1,1,-1I,I]
@testex  Solve(x^3-1,x) == [1,-1//2 + (-1//2*I) * (3 ^ (1//2)),-1//2 + (1//2*I) * (3 ^ (1//2))]

## Roots

@ex      q = x^2 - 8x + 8
@testex  Roots(q) == [[4 + -2 * (2 ^ (1//2)),1],[4 + 2 * (2 ^ (1//2)),1]]

@ex ClearAll(a,b,x,y,z,p,q,rex,f)

## Series

@testex Sum(x^i/Factorial(i), [i,0,Infinity]) == E^x

## Orthoganal Polynomials, etc.

@testex JacobiP(1,2,3,x)  == -1//2 + (7//2) * x

## Trig
@testex Sin(Pi/4) == (1//2) * (2 ^ (1//2))

@testex Integrate(DiracDelta(x-1), [x,-Infinity, Infinity]) == 1
@testex Integrate(DiracDelta(x-1), [x,2, Infinity]) == 0
@testex Integrate(DiracDelta(x-1), [x,-1000, 1000]) == 1
@testex Head(DiracDelta(0)) ==  DiracDelta
@testex DiracDelta(1) == 0

@ex ClearAll(x,i,conds,h,res,t,s)

@ex ex = x^5 - x^3 - x^2 + 1
@testex FactorSquareFree(ex) == ((-1 + x) ^ 2) * (1 + 2 * x + 2 * (x ^ 2) + x ^ 3)
@testex Factor(ex) == ((-1 + x) ^ 2) * (1 + x) * (1 + x + x ^ 2)
@ex ClearAll(x,ex)


#### BellB

@testex BellB(30) == 846749014511809332450147
@testex BellB(6,4,[x1,x2,x3]) == 45 * (x1 ^ 2) * (x2 ^ 2) + 20 * (x1 ^ 3) * x3
@testex BellB(4,t) == t + 7 * (t ^ 2) + 6 * (t ^ 3) + t ^ 4


#### Divisors

@testex 10000/Divisors(10000) == Reverse(Divisors(10000))

#### Misc...

@testex D(Gamma(x),x) == Gamma(x) * (PolyGamma(0,x))

# FIXME. The returned expression is large. and does not '==' the
# input. Something is not being fully evaluated when the series is returned
@testex Series(Gamma(x), [x, 0, 3])[1] == -EulerGamma


@ex ClearAll(x1,x2,x3,t,x)

################

@ex If( Length(UserSyms()) > 0 ,  Println("**********", UserSyms()))
@testex Length(UserSyms()) == 0

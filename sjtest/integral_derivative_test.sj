### Sum

T Sum(i^3,[i,1,5]) == Apply(Plus,Table(i^3, [i,5]))
T Sum(i^3,[i,1,5]) == 225
T Factor(Sum(i^2,[i,1,n]))  == (1/6)*n*(1 + n)*(1 + 2n)
  r = Factor(Sum(i^3,[i,1,n]))
T r == (1/4)*(n^2)*((1 + n)^2)
T (r ./ (n => 10)) == Sum(i^3,[i,1,10])
T Sum(x^i/Factorial(i), [i,0,Infinity]) == E^x
T Sum(1/n^2, [n,1,Infinity]) == Pi^2/6
T Sum( x^n/Factorial(n)^2 , [n,0,Infinity])  == BesselI(0,2(x^(1/2)))

expr = i^2
T Sum(expr, [i,1,n]) == (1/6)*n + (1/2)*(n^2) + (1/3)*(n^3)  # This is the behavior we want.
T Sum(Evaluate(expr), [i,1,n]) == (1/6)*n + (1/2)*(n^2) + (1/3)*(n^3)

# FIXME: infinite loop. Bug is in Symata, not sympy
# Map(Function(n, Sum(1/i^n,[i,1,Infinity])), [2])

g(n_) := Sum(1/i^n,[i,1,Infinity])
T Map(g, [2,3,4,5,6]) == [(1/6)*(Pi^2),Zeta(3),(1/90)*(Pi^4),Zeta(5),(1/945)*(Pi^6)]

T Sum((j + i)^(1), [i,1,3], [j,1,i]) == 24
# Following is returned unevaluated. Bug is in sympy
# T Sum((j + i)^(-1), [j,1,i], [i,1,3])

r = Sum(x^n, [n,0,Infinity])
T r == ConditionalExpression(((1 - x)^(-1)),Abs(x) < 1)
x = 7
T r == Undefined
x = 1/2
T r == 2
ClearAll(r,x)

### Integrate

 ClearAll(r,y,x)
T Integrate(x,x) == 1//2 * x^2
T Integrate(x,[x,0,1]) == 1//2
T Integrate(x,[x,0,1],y) == 1//2 * y

     r = Integrate(1/Cos(x + a), x)
# T r == Log(Sec(a + x) + Tan(a + x))  # if not loading code_in_jl

T r == -1 * Log(-1 + Tan((1//2) * a + (1//2) * x)) + Log(1 + Tan((1//2) * a + (1//2) * x)) # are loading code_in_jl

T Integrate(E^(-x^2),x) == (1//2) * (π ^ (1//2)) * Erf(x)
T Integrate(E^(x^2),x) == (1//2) * (π ^ (1//2)) * Erfi(x)

# Fixed, we can now return conditions as well.
T Integrate( Exp(-t)*t^(a-1),[t,0,Infinity], conds => "none") == Gamma(a)

# SymPy examples
T Integrate(x^2 + x + 1 ,x) == x + 1/2 * (x ^ 2) + 1/3 * (x ^ 3)
T Integrate( x/(x^2 + 2*x + 1), x) == Log(1 + x) + (1 + x) ^ (-1)
T Integrate( x^2 * Exp(x) * Cos(x), x) ==
        -1/2 * (E ^ x) * Cos(x) + 1/2 * (E ^ x) * (x ^ 2) * Cos(x) + 1/2 * (E ^ x) * Sin(x) + 1/2 * (E ^ x) * (x ^ 2) * Sin(x) + -(E ^ x) * x * Sin(x)
T Integrate( Exp(-x^2)*Erf(x), x) == 1/4 * (π ^ (1/2)) * (Erf(x) ^ 2)


T Integrate(x*y,x) == 1//2 * y * (x ^ 2)
T Integrate(Log(x),x) == -x + x * Log(x)
T Integrate(Log(x), [x, 1, a])  == 1 + -a + a * Log(a)
T Integrate(x) == 1//2 * (x ^ 2)    # Should we disallow this ?
T Integrate(Sqrt(1+x), [x,0,x]) == -2/3 + 2/3 * ((1 + x) ^ (3/2))
T Integrate(Sqrt(1+x), x) == 2//3 * ((1 + x) ^ (3//2))
res = Integrate(x^a * Exp(-x), [x,0,Infinity])
T res == ConditionalExpression(Γ(1 + a),-Re(a) < 1)

T Integrate(x^a * Exp(-x), [x,0,Infinity], conds => "none") == Γ(1 + a)
a = 1/2
T res == (1/2)*(Pi^(1/2))
a = -3/2
T res == Undefined
ClearAll(a)
T res == ConditionalExpression(Gamma(1 + a),-Re(a) < 1)

T Integrate(Exp(-x^2),  [x,0,Infinity]) == (1/2)*(Pi^(1/2))

# Following works if 1/cos does not go to sec
# T r == -1 * Log(-1 + Tan((1//2) * a + (1//2) * x)) + Log(1 + Tan((1//2) * a + (1//2) * x))

# Following works if 1/cos does not go to sec
#T Simplify(D(r,x)) == 1/Cos(x+a)
 ClearAll(r,y,x,res,a)

####  MellinTransform

T MellinTransform(Exp(-x),x,s) == [Gamma(s),[0,Infinity],True]

####  InverseMellinTransform

T InverseMellinTransform(Gamma(s),s,x, [0,Infinity]) == Exp(-x)
 f = 1/(s^2 - 1)
T InverseMellinTransform(f,s,x,[-Infinity,-1]) == HeavisideTheta(-1 + x) * (-1/2 * (x ^ (-1)) + 1/2 * x)
 ClearAll(f,s,x)

#### LaplaceTransform

# Use these if noconds=false
# T LaplaceTransform(t^a,t,s) == [(s ^ -1) * (s ^ (-1 * a)) * Gamma(1 + a),0,-1 * Re(a) < 1]
# T LaplaceTransform(Cos(t),t,s) == [s * ((1 + s ^ 2) ^ -1),0,True]
# T LaplaceTransform(Exp(3*t),t,s) == [(-3 + s) ^ -1,3,Unequality(((1//3) * s),1)]  # TODO: translate

T LaplaceTransform(t^a,t,s) == (s ^ -1) * (s ^ (-1 * a)) * Gamma(1 + a)
T LaplaceTransform(Cos(t),t,s) == s * ((1 + s ^ 2) ^ -1)
T LaplaceTransform(Exp(3*t),t,s) == (-3 + s) ^ -1

#### InverseLaplaceTransform

T InverseLaplaceTransform(1/s,s,t) == HeavisideTheta(t)
T InverseLaplaceTransform(Exp(-a*s)/s, s, t) == HeavisideTheta(t + -a)

# This uses the 'old' SymPy assumptions system
Assume(ta,Real)
T InverseLaplaceTransform( 1/(s*ta + 1)^n,s,t) == (E^(-t*(ta^(-1))))*(t^(-1 + n))*(ta^(-n))*(Gamma(n)^(-1))*HeavisideTheta(t)

# FIXME: This does not remove the SymPy assumption
ClearAll(ta)


#### FourierTransform

T FourierTransform( E^(-x^2), x, k) == E ^ (-(k ^ 2) * (π ^ 2)) * (π ^ (1/2))
T InverseFourierTransform(E ^ (-(k*Pi) ^ 2) * (π ^ (1/2)), k, x) == E ^ (-(x ^ 2))

#### SineTransform

T SineTransform(x*Exp(-a*x^2), x, k) == 1/4 * (2 ^ (1/2)) * (a ^ (-3/2)) * (E ^ (-1/4 * (a ^ (-1)) * (k ^ 2))) * k

#### CosineTransform

T CosineTransform(Exp(-a*x),x,k) == (2^(1/2))*a*((a^2 + k^2)^(-1))*(Pi^(-1/2))

 ClearAll(x,k,a)

#### HankelTransform

 ht = HankelTransform(1/r^m, r, k, nu)
T  ht == 2 * (2 ^ (-m)) * (k ^ (-2 + m)) * Gamma(1 + 1/2 * nu + -1/2 * m) * (Gamma(1/2 * m + 1/2 * nu) ^ (-1))
T  InverseHankelTransform(ht,k,r,nu) == r^(-m)

 ClearAll(r,m,k,nu,ht)

#### D

T  D(f,x) == 0
T D(x^2,x) == 2 * x
T D(x^3,x,2) == 6 * x
# FIXME Exp is not translated properly
# We convert Exp(x) = E^x in an apprules. Probably better to convert it
# postprocessing sympy output or something. Anyway, the tests pass
T  D(Exp(x),x) == Exp(x)
T  D(Exp(Exp(x)),x) == Exp(x) * Exp(Exp(x))
T  D(ArcTan(x),x) == (1 + x ^ 2) ^ -1
T  D(BesselJ(1,x),x) == (1//2) * (BesselJ(0,x)) + (-1//2) * (BesselJ(2,x))

#### Product

T Product(i, [i,1,k]) == Factorial(k)
T Product(m, [i,1,k]) == m^k
T Product(i, [i,1,k], [k,1,n]) == Product(Factorial(k),[k,1,n])
T Product(1/(1+i), [i,1,n]) == (Pochhammer(2,n))^(-1)


#ClearAll(x,s,t,conds,f,i,m,k)
Map(ClearAll, UserSyms())

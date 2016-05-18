T testUserSyms

#### Integrate

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
T Integrate(x^a * Exp(-x), [x,0,Infinity]) == ConditionalExpression(Γ(1 + a),-Re(a) < 1)
T Integrate(x^a * Exp(-x), [x,0,Infinity], conds => "none") == Γ(1 + a)

# Following works if 1/cos does not go to sec
# T r == -1 * Log(-1 + Tan((1//2) * a + (1//2) * x)) + Log(1 + Tan((1//2) * a + (1//2) * x))

# Following works if 1/cos does not go to sec
#T Simplify(D(r,x)) == 1/Cos(x+a)
 ClearAll(r,y,x)

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


#### Sum

T Sum(x^i/Factorial(i), [i,0,Infinity]) == E^x

 ClearAll(x,s,t,conds,f,i,m,k)

T testUserSyms

T testUserSyms

#### Cancel

T Cancel( (2*x^2-2)/(x^2-2*x+1)) == (-1 + x) ^ (-1) * (2 + 2 * x)

#### Collect

T  Args(Collect(a*x^2 + b*x^2 + a*x - b*x + c, x)) == [c,(a + -b) * x,(a + b) * (x ^ 2)]
T  Args(Collect(x^2 + y*x^2 + x*y + y + a*y, [x, y])) == [x * y,(1 + a) * y,(x ^ 2) * (1 + y)]
T  Collect(a*Sin(2*x) + b*Sin(2*x), Sin(2*x)) == (a + b) * Sin(2 * x)
T  Collect(a*x*Log(x) + b*(x*Log(x)), x*Log(x)) == (a + b) * x * Log(x)
T  Collect(a*x^c + b*x^c, x) == a*(x^c) + b*(x^c)
T  Collect(a*x^c + b*x^c, x^c) == (a + b)*(x^c)

T Collect(a*x + b*y + c*x, x) == (a + c) * x + b * y
T Collect(a*x^(2*c) + b*x^(2*c), x^c) == (a + b)*(x^(2*c))
T Collect(a*Exp(2*x) + b*Exp(2*x), Exp(x)) == (a + b)*(E^(2*x))

      q = x*y + x*y^2 + x^2*y + x
T  Collect(q,x) == (x^2)*y + x*(1 + y + y^2)
T  Collect(q,y) == x + (x + x^2)*y + x*(y^2)
      ClearAll(q,x,y)

# sympy does not do this unless we use Expand. Mma does the expansion
T Collect(Expand((1+a+x)^4), x) == 1 + 4*a + 6*(a^2) + 4*(a^3) + a^4 + (4 + 12*a + 12*(a^2) + 4*(a^3))*x + (6 + 12*a + 6*(a^2))*(x^2) + (4 + 4*a)*(x^3) + x^4

# This differs from sympy. We cannott quite do Derivative(1)f yet
T Collect( a*D(f(x),x) + b*D(f(x),x), D(f(x),x))  == (D(f(x),x))*(a + b)

#### Common subexpression

# FIXME: the replacements should be a list of rules, in reverse order. Then Fold(newexpr, rules) returns the original expression.

T Cse(Sqrt(Sin(x)+5)*Sqrt(Sin(x)+4)) == [[[x0,Sin(x)]],[((4 + x0)^(1/2))*((5 + x0)^(1/2))]]
T Cse(Sqrt(Sin(x+1) + 5 + Cos(y))*Sqrt(Sin(x+1) + 4 + Cos(y))) == [[[x0,Cos(y) + Sin(1 + x)]],[((4 + x0)^(1/2))*((5 + x0)^(1/2))]]
T Cse((x-y)*(z-y) + Sqrt((x-y)*(z-y))) == [[[x0,-y],[x1,(x + x0)*(x0 + z)]],[x1 + x1^(1/2)]]

#### Factor, Expand

T  Factor(Expand( (a+b)^2 )) == (a+b)^2
T  Expand( (a + f(x)) ^2 ) == a ^ 2 + 2 * a * f(x) + f(x) ^ 2
T  Expand(  (x+y+z)^2 ) == x^2 + 2*x*y + y^2 + 2*x*z + 2*y*z + z^2
T  Expand( Exp(x+y) ) == (E^x)*(E^y)
T  Expand(x + I * y, Complex => True) == I*Im(x) + -Im(y) + Re(x) + I*Re(y)
T  Expand(Sin(x + I*y), Complex => True) == Cosh(Im(x) + Re(y))*Sin(-Im(y) + Re(x)) + I*Cos(-Im(y) + Re(x))*Sinh(Im(x) + Re(y))
      p = Expand((x-1)*(x-2)*(x-3)*(x^2 + x + 1))
T  p == -6 + 5 * x + -1 * (x ^ 2) + 6 * (x ^ 3) + -5 * (x ^ 4) + x ^ 5
T  Collect( Expand(x + I *y, Complex => True), I) == -Im(y) + I*(Im(x) + Re(y)) + Re(x)

T  Factor(p) == (-3 + x) * (-2 + x) * (-1 + x) * (1 + x + x ^ 2)
T  Factor(2*x^5 + 2*x^4*y + 4*x^3 + 4*x^2*y + 2*x + 2*y) == 2*((1 + x^2)^2)*(x + y)
T  Factor(x^2 + 1, Modulus => 2) == (1 + x)^2
T  Factor(x^10 - 1, Modulus => 2) == ((1 + x)^2)*((1 + x + x^2 + x^3 + x^4)^2)
T  Factor(x^2 + 1, Gaussian => True) == (-1I + x)*(I + x)
T  Factor((x^2 + 4*x + 4)^10000000*(x^2 + 1)) == ((2 + x)^20000000)*(1 + x^2)
T  Factor( 2^(x^2 + 2*x + 1), Deep => True ) == 2^((1 + x)^2)
# T  Factor(x^2 - 2, extension => Sqrt(2))  FIXME. raises exception

 ClearAll(a,b,c,p,x,y,f)

#### ExpandFunc

T  ExpandFunc(Gamma(x+2)) == x*Gamma(x)*(1 + x)
T  Simplify(x*Gamma(x)*(1 + x)) == Gamma(x+2)
# FIXME  Canononical order should be  x*(1+x)*Gamma(x).  We get x*Gamma(x)*(1+x)

T  ExpandLog(Log(x^2), Force => True) == 2Log(x)
T  ExpandLog(Log(x/y), Force => True) == Log(x) + -Log(y)
T  ExpandLog(Log(x^n), Force => True) == n*Log(x)
T  LogCombine( Log(x) + Log(y), Force => True) == Log(x*y)
T  LogCombine( n*Log(z), Force => True) == Log(z^n)

#### ExpandTrig

T ExpandTrig(Sin(x + y)) == Cos(y)*Sin(x) + Cos(x)*Sin(y)

#### Together, Apart

# Note, Mma and others have this FullForm, but display a 1/(x*y), etc.
     z = ( 1/x + 1/(x+1))
T Together(z) == (x ^ -1) * ((1 + x) ^ -1) * (1 + 2 * x)
T Apart(Together(z)) == z
     ClearAll(z)
T Together(1/x + 1/y + 1/z) == (x^(-1))*(y^(-1))*(z^(-1))*(x*y + x*z + y*z)
T Together(1/(x*y) + 1/y^2) == (x^(-1))*(y^(-2))*(x + y)
T Together(1/(1 + 1/x) + 1/(1 + 1/y)) == ((1 + x)^(-1))*((1 + y)^(-1))*(x*(1 + y) + (1 + x)*y)
T Together(1/Exp(x) + 1/(x*Exp(x))) == (E^(-x))*(x^(-1))*(1 + x)
T Together(1/Exp(2*x) + 1/(x*Exp(3*x))) == (E^((-3)*x))*(x^(-1))*(1 + (E^x)*x)

T Together(Exp(1/x + 1/y), Deep => True) == E^((x^(-1))*(y^(-1))*(x + y))
T Together(Exp(1/x + 1/y)) == E^(x^(-1) + y^(-1))
T Factor((x^2 - 1)/(x^2 + 4*x + 4)) == (-1 + x)*(1 + x)*((2 + x)^(-2))

#### PowSimp

T PowSimp(x^a * x^b) == x^(a + b)
T PowSimp(t^c * z^c, Force => True) == (t*z)^c

#### PowDenest

T PowDenest( (x^a)^b, Force => True  ) ==  x^(a*b)

#### RatSimp

T RatSimp(1/x + 1/y) == (x^(-1))*(y^(-1))*(x + y)

#### TrigSimp

T TrigSimp(2*Sin(x)^2 + 2* Cos(x)^2) == 2
T TrigSimp(f(2*Sin(x)^2 + 2* Cos(x)^2)) == f(2)
T TrigSimp( 3*Tanh(x)^7 - 2/Coth(x)^7) == Tanh(x)^7

#### Simplify

T Simplify( Cos(x)^2 + Sin(x)^2) == 1
T Simplify( (x + x^2)/(x*Sin(y)^2 + x*Cos(y)^2) ) == 1 + x
T Cancel(TrigSimp((x + x^2)/(x*Sin(y)^2 + x*Cos(y)^2))) == 1 + x

#### FullSimplify

T FullSimplify( -Sqrt(-2*Sqrt(2)+3)+Sqrt(2*Sqrt(2)+3) ) == 2

#### Rewrite

ClearAll(nu,z,p,a)

T Rewrite(Tan(x), Sin)  ==  2(Sin(2x)^(-1))*(Sin(x)^2)
# T Rewrite(ExpIntegralE(nu,z), gamma) == (z^(-1/2 + -a*(p^(-1))))*(Gamma((1/2 + a*(p^(-1))),z))


 ClearAll(x,y,z,f,deep,gaussian,modulus)

T testUserSyms

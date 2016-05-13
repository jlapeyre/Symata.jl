#### Cosh, ACosh

T testUserSyms


T Exp(0) == 1
T Exp(1) == E
T Exp(2) == E^2
T Exp(a) == E^a
T Exp(Pi*I) == -1
T Exp(Pi*I/2) == I
T Exp(3*Pi*I/2) == -I

T Chop(Abs(Cos(1.0 + 2.0 * I) - (2.0327230070196656 + -3.0518977991518 * I))) == 0
T Cos(Pi) == -1
T Cos(Pi/2) == 0
T Cos(3*Pi/2) == 0
T Cos(Pi/3) == 1/2

T Cos(0) == 1
T Cos(I*a) == Cosh(a)

T Sin(0) == 0
T Sin(Pi/2) == 1
T Sin(Pi) == 0
T Sin(Pi/4) == Cos(Pi/4) ==  (1/2)*(2^(1/2))
T Sin(Pi/6) == 1/2
T Sin(I*a) == I*Sinh(a)

T If( BigIntInput(), Head(N(1)) == BigFloat, Head(N(1)) == Float64)
T Head(N(Cos(1))) == Float64   # because of sympy this comes back as an ordinary double

ClearAll(a)

T ACosh(1) == 0
T Cosh(0) == 1
T Cosh(ACosh(1)) == 1
T ACosh(0) == Pi*I/2
T Cosh(Pi*I/2) == 0
T ACosh(1/2) == Pi*I/3

T D(Cosh(x),x) == Sinh(x)
T D(Cosh(x),[x,4]) == Cosh(x)
T Integrate(Cosh(x),x) == Sinh(x)



 ClearAll(x)
T testUserSyms




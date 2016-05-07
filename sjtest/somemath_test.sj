
# Much of this file is really testing mpow.
# Numerical powers are either passed to mpow or else
# returned unevaluated by do_Power, depending on the type of args.
# We are missing several test cases here.
# But, do_Power and mpow should now reproduce, more or less what Mma does.

T testUserSyms

# This tests apprules and canonializer

T  Abs(-1) == 1
T  Abs(-2.0) == 2.0
T  Abs(z^3) == Abs(z)^3
T  Abs(z^4.1) == Abs(z)^4.1
T  Abs(-z) == Abs(z)
T  Abs(-z^(1/2))^2 == Abs(z)

# We fixed a bug, so this is no longer true
#T N(Cos(E),100) == :( cos(big(e)))

T Arg(Complex(1.0,1.0)) - N(Arg(Complex(1,1))) == 0.0
T Arg(Complex(1,1)) == π/4

 ClearAll(z)

T 27^(1/3) == 3
T 27^(1/3) == 3

# TODO: we don't want the following
T Apply(List, 26^(1/3)) == [13 ^ (1//3),2 ^ (1//3)]  # FIXME

T 27^(2/3) == 9
T (-27)^(2/3) == (-1) ^ (2//3) * 9
T (-27)^(1/3) == 3 * (-1) ^ (1//3)

# Neg. Int to float power took a long time to fix, because we do not print full error message (domain error).
T  Chop((-1)^(3.2) + 0.8090169943749477 + 0.5877852522924728 * I) == 0
T 3^0 == 1
# two bug fixes in the next one
T  If( BigIntInput() , Head((3^0)) == BigInt ,  Head((3^0)) == Int)

T Apply(List,3^(1/2)) == [3,1//2]
T 4^(1/2) == 2
T Apply(List, 28^(1/3)) == [7 ^ (1//3),2 ^ (2//3)]
T Apply(List,(49*7*3*3)^(1/3)) == [7,3^(2//3)]
T 27^(1/3) == 3
T Chop((-1.0)^(1//3) - (0.5 + 0.8660254037844387 * I)) == 0
T Chop(N((-1)^(1//3)) - (0.5 + 0.8660254037844387 * I)) == 0

T 1/I == -I
T (I + 1)^(-2) ==  -1/2 * I
T (I + 1)^(-3) ==  -1/4 - 1/4 * I
T (I + 2)^(-3) == 2/125 - 11/125 * I

T Log(2,8) == 3
T Apply(List,Log(2,9)) == [2,9]

T Cos(0) == 1

T Chop(Abs(Cos(1.0 + 2.0 * I) - (2.0327230070196656 + -3.0518977991518 * I))) == 0
T Cos(Pi) == -1
T Cos(Pi/2) == 0
T Cos(3*Pi/2) == 0
# Cos is handled in  It follows Mma...
T Cos(z * I) == Cosh(z)
# Sin is done by sympy, which leaves this untouched

T If( BigIntInput(), Head(N(1)) == BigFloat, Head(N(1)) == Float64)
T Head(N(Cos(1))) == Float64   # because of sympy this comes back as an ordinary double

# fixes a bug.
T Head((a*b)^(1/2)) == Power

 ClearAll(z,a,b)
T testUserSyms

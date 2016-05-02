using Base.Test

# Much of this file is really testing mpow.
# Numerical powers are either passed to mpow or else
# returned unevaluated by do_Power, depending on the type of args.
# We are missing several test cases here.
# But, do_Power and mpow should now reproduce, more or less what Mma does.

@testex testUserSyms

# This tests apprules and canonializer

@testex  Abs(-1) == 1
@testex  Abs(-2.0) == 2.0
@testex  Abs(z^3) == Abs(z)^3
@testex  Abs(z^4.1) == Abs(z)^4.1
@testex  Abs(-z) == Abs(z)
@testex  Abs(-z^(1/2))^2 == Abs(z)

@testex N(Cos(E),100) == :( cos(big(e)))

@testex Arg(Complex(1.0,1.0)) - N(Arg(Complex(1,1))) == 0.0
@testex Arg(Complex(1,1)) == π/4

@ex ClearAll(z)

@testex 27^(1/3) == 3
@testex 27^(1/3) == 3

# TODO: we don't want the following
@testex Apply(List, 26^(1/3)) == [13 ^ (1//3),2 ^ (1//3)]  # FIXME

@testex 27^(2/3) == 9
@testex (-27)^(2/3) == (-1) ^ (2//3) * 9
@testex (-27)^(1/3) == 3 * (-1) ^ (1//3)

# Neg. Int to float power took a long time to fix, because we do not print full error message (domain error).
@testex  Chop((-1)^(3.2) + 0.8090169943749477 + 0.5877852522924728 * I) == 0
@testex 3^0 == 1
@test  typeof(@ex(3^0)) == Int  # bug fix

@testex Apply(List,3^(1/2)) == [3,1//2]
@testex 4^(1/2) == 2
@testex Apply(List, 28^(1/3)) == [7 ^ (1//3),2 ^ (2//3)]
@testex Apply(List,(49*7*3*3)^(1/3)) == [7,3^(2//3)]
@testex 27^(1/3) == 3
@testex Chop((-1.0)^(1//3) - (0.5 + 0.8660254037844387 * I)) == 0
@testex Chop(N((-1)^(1//3)) - (0.5 + 0.8660254037844387 * I)) == 0

@testex 1/I == -I
@testex (I + 1)^(-2) ==  -1/2 * I
@testex (I + 1)^(-3) ==  -1/4 - 1/4 * I
@testex (I + 2)^(-3) == 2/125 - 11/125 * I

@testex Log(2,8) == 3
@testex Apply(List,Log(2,9)) == [2,9]

@testex Cos(0) == 1

@testex Chop(Abs(Cos(1.0 + 2.0 * I) - (2.0327230070196656 + -3.0518977991518 * I))) == 0
@testex Cos(Pi) == -1
@testex Cos(Pi/2) == 0
@testex Cos(3*Pi/2) == 0
# Cos is handled in SJulia. It follows Mma...
@testex Cos(z * I) == Cosh(z)
# Sin is done by sympy, which leaves this untouched

@testex Head(N(1)) == Float64
@testex Head(N(Cos(1))) == Float64

# fixes a bug.
@testex Head((a*b)^(1/2)) == Power

@ex ClearAll(z,a,b)
@testex testUserSyms

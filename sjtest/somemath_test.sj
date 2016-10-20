# Much of this file is really testing mpow.
# Numerical powers are either passed to mpow or else
# returned unevaluated by do_Power, depending on the type of args.
# We are missing several test cases here.
# But, do_Power and mpow should now reproduce, more or less what Mma does.

T testUserSyms

T Arg(Complex(1.0,1.0)) - N(Arg(Complex(1,1))) == 0.0
T Arg(Complex(1,1)) == π/4

ClearAll(z)

T 100^(1/2) == 10  # fix bug in commit following 06ca924038273a0309b2dbb2ea7c84d540bd84dd

# Fix bug. we can't do setfixed in canonexpr!(mx::Mxpr{:Power}) or this is broken
res = 102^(1/2)
T res^2 == 102

ClearAll(res)

T 27^(1/3) == 3

# TODO: we don't want the following
T Apply(List, 26^(1/3)) == [2 ^ (1//3), 13 ^ (1//3)]  # FIXME

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
T Apply(List, 28^(1/3)) == [2 ^ (2//3), 7 ^ (1//3)]
T Apply(List,(49*7*3*3)^(1/3)) == [7,3^(2//3)]

T Chop((-1.0)^(1//3) - (0.5 + 0.8660254037844387 * I)) == 0
T Chop(N((-1)^(1//3)) - (0.5 + 0.8660254037844387 * I)) == 0

T 1/I == -I
T (I + 1)^(-2) ==  -1/2 * I
T (I + 1)^(-3) ==  -1/4 - 1/4 * I
T (I + 2)^(-3) == 2/125 - 11/125 * I


T (-27/64)^(2/3) == 9/16*((-1)^(2/3))

# the result printed incorrectly: 9/16*((-1)^2/3)
# We fixed this. But, we still got a comparison error:
# FIXME: we get a comparison error if we use the incorrectly printed value

# fixes a bug.
T Head((a*b)^(1/2)) == Power

 ClearAll(z,a,b)
T testUserSyms

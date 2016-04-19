using Base.Test

# This tests apprules and canonializer
@ex ClearAll(z)

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

# mpow in arithmetic.jl gets this wrong
@testex 27^(2/3) == 9
@testex (-27)^(2/3) == (-1) ^ (2//3) * 9
@testex (-27)^(1/3) == 3 * (-1) ^ (1//3)
# TODO: Fix domain error
#  (-1)^(.455)

@testex 3^0 == 1
@test  typeof(@ex(3^0)) == Int  # bug fix

@testex Apply(List,3^(1/2)) == [3,1//2]
@testex 4^(1/2) == 2
@testex Apply(List, 28^(1/3)) == [7 ^ (1//3),2 ^ (2//3)]
@testex Apply(List,(49*7*3*3)^(1/3)) == [7,3^(2//3)]
@testex 27^(1/3) == 3
@testex Chop((-1.0)^(1//3) - (0.5 + 0.8660254037844387 * I)) == 0
@testex Chop(N((-1)^(1//3)) - (0.5 + 0.8660254037844387 * I)) == 0


@testex Log(2,8) == 3
@testex Apply(List,Log(2,9)) == [2,9]

@testex Cos(0) == 1

@testex Chop(Abs(Cos(1.0 + 2.0 * I) - (2.0327230070196656 + -3.0518977991518 * I))) == 0
@testex Cos(Pi) == -1
@testex Cos(Pi/2) == 0
@testex Cos(3*Pi/2) == 0
# Cos is handled in SJulia. It follows Mma...
@testex Cos(z * I) == Cosh(z)
# Sin is done by sympy, which leave this untouched

# see top of math_functions.jl
# Currently, either this works, or pattern matching:
# Some code relies on :Float64 being unbound in SJulia
# other code relies on it being bound to ::Type{Float64}.
# It can be made consistent however.
#@testex Head(N(1)) == Float64
#@testex Head(N(Cos(1))) == Float64

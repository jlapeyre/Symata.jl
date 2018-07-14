# Much of this file is really testing mpow.
# Numerical powers are either passed to mpow or else
# returned unevaluated by do_Power, depending on the type of args.
# We are missing several test cases here.
# But, do_Power and mpow should now reproduce, more or less what Mma does.

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

# fixed 98e02317aee46e7eceaac47d98c4401ef23682f0
T (-27/64)^(2/3) == 9/16*((-1)^(2/3))

# fixed error raised in Comparison
T (((9/16)*((-1)^(2/3)) == 3/16), True)

T 27^(1/2) == 3(3^(1/2))

# fixes a bug.
T Head((a*b)^(1/2)) == Power

# Check that pi --> float(pi) when needed. This is not automatic in Julia
T  Isa(J( mpow(pi,-3) ), Real)

 ClearAll(z,a,b)

### End old somemath_test.sj

T Head(1//1) == Int64
T Head(3//1) == Int64

 ClearAll(a,b,ar)
T Head(Re(a)) == Re
T Re(I*a)[2] == Im(a)
T Im(I*a) == Re(a)
T Im(I*a*b) == Re(a*b)
T Re(I*a*b) == -Im(a*b)
T Re(2*a) == 2*Re(a)
T Im(2*a) == 2*Im(a)

T 1/0 == DirectedInfinity()
T 1/0.0 == ComplexInfinity
T 1/big"0.0" == ComplexInfinity
T 1//0 == DirectedInfinity()
T 1/DirectedInfinity() == 0
T DirectedInfinity() == ComplexInfinity
T DirectedInfinity()^(2) == DirectedInfinity()
T DirectedInfinity()^(-2) == 0
T DirectedInfinity() * a * 0 == Indeterminate
T ComplexInfinity^(-2) == 0
T Infinity^(-2) == 0
T  Apply(Times, [DirectedInfinity(),a,a,a,0]) == Indeterminate
T  0 * Indeterminate == Indeterminate
T  0 * Infinity == Indeterminate
T  0 * ComplexInfinity == Indeterminate
T  1/0 == ComplexInfinity
T 0/0 == Indeterminate
T  a * 3 * ComplexInfinity == ComplexInfinity

T Infinity * (1+I) == DirectedInfinity((1 + I)*(2^(-1/2)))
T Args(a*Infinity) == [a,Infinity]
T Args(Infinity * -a) == [a,-Infinity]


T Infinity + 1 == Infinity
T Infinity + 10 == Infinity
T Infinity - 10 == Infinity
T Infinity * 10 == Infinity
T Infinity * -10 == -Infinity

# FIXME
# T - Infinity * -10 == -Infinity
# T - Infinity * 10 == -Infinity
# T -Infinity - 10

T J( mpow(1,0) == 1 )
T J( mpow(1,1) == 1 )
T Isa(J(mpow(1,-1)), Integer)
T J( mpow(2,1//2) == mxpr(:Power, 2, 1//2) )
T J( mpow(2,-1//2) == mxpr(:Power, 2, -1//2) )
T J( mpow(4,1//2) == 2 )
T J( mpow(4,-1//2) == 1//2 )
T J( mpow(7^3,1//2) == mxpr(:Times, 7, mxpr(:Power, 7, 1//2)) )
T J(  mpow(8*5,1//2) == mxpr(:Times, 2, mxpr(:Power, 2, 1//2), mxpr(:Power, 5, 1//2)) )
T J( mpow(9, 2//3) == mxpr(:Times, 3, mxpr(:Power, 3, 1//3)) )
T J( mpow(-9,2//3) == mxpr(:Times, 3, mxpr(:Power, 3, 1//3), mxpr(:Power,-1,2//3)))

T J(mmul(2 + 2*im, 1//2) == Complex(1,1))

## FIXME: this behavior is strange and wrong.
n1 = 1/(2+2I)
n2 = (2+2I)
r = n1 * n2
T (Isa(r,Integer) && r == 1)
T (r === (n2 * n1))
T Isa(n2 * n1, Integer)
T Not( n2 * n1 === n1 * n2)
T Not(Isa(n1 * n2, Integer))
# We cant change these. The user must use mpow here.
# T J( 2^(1//2) ) == mxpr(:Power, 2, 1//2)
# T J( 2^(-1//2)) == mxpr(:Power, 2, -1//2) )
#T J(4^1//2) == 2
#T J(4^-1//2) == 1//2
# T J( 7^3^1//2) == mxpr(:Times, 7, mxpr(:Power, 7, 1//2))
# T J(  (8*5)^1//2) == mxpr(:Times, 2, mxpr(:Power, 2, 1//2), mxpr(:Power, 5, 1//2))
# T J( 9^2//3) == mxpr(:Times, 3, mxpr(:Power, 3, 1//3))
# T J( -9^2//3) == mxpr(:Times, 3, mxpr(:Power, 3, 1//3), mxpr(:Power,-1,2//3))

T Sqrt(-1) == I      # fixes bug in mpow{T<:Integer, V<:Integer}(x::T,y::Rational{V})
T Sqrt(-1)^2 == -1   # same bug

## Fix domain error bug in _mpow{T<:Integer, V<:Integer}(x::T,y::Rational{V})
## d02b2a74cc6a1ee99597486c23a6d85de292cce9
T (2^(1/3))^(-4) == (1/2)*2^(-1/3)

ClearAll(a,b,n1,n2)

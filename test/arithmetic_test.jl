using Base.Test

@testex Head(1//1) == Int64
@testex Head(3//1) == Int64

@ex ClearAll(a,b,ar)
@testex Head(Re(a)) == Re
@testex Re(I*a)[2] == Im(a)
@testex Im(I*a) == Re(a)
@testex Im(I*a*b) == Re(a*b)
@testex Re(I*a*b) == -Im(a*b)

# -Inf is caught during  parsing
# Todo: This is failing now
# @testex -Inf == :( -Inf )
@testex 1/0 == DirectedInfinity()
@testex 1//0 == DirectedInfinity()
@testex 1/DirectedInfinity() == 0
@testex DirectedInfinity()^(2) == DirectedInfinity()
@testex DirectedInfinity()^(-2) == 0
@testex DirectedInfinity() * a * 0 == Indeterminate
@testex ComplexInfinity^(-2) == 0
@testex Infinity^(-2) == 0
# BROKEN: this returns 0
#@test Apply(Times, [DirectedInfinity(),a,a,a,0]) == Indeterminate
@testex  0 * Indeterminate == Indeterminate
@testex  0 * Infinity == Indeterminate
@testex  0 * ComplexInfinity == Indeterminate
@testex  1/0 == ComplexInfinity
@testex  a * 3 * ComplexInfinity == ComplexInfinity

import SJulia: mpow, mxpr

@test mpow(1,0) == 1
@test mpow(1,1) == 1
# Do we want the following ? Or better 1
@test mpow(1,-1) == 1//1
@test mpow(2,1//2) == mxpr(:Power, 2, 1//2)
@test mpow(2,1//2) == mxpr(:Power, 2, 1//2)
@test mpow(2,-1//2) == mxpr(:Power, 2, -1//2)
@test mpow(4,1//2) == 2
@test mpow(4,-1//2) == 1//2
@test mpow(7^3,1//2) == mxpr(:Times, 7, mxpr(:Power, 7, 1//2))
@test mpow(8*5,1//2) == mxpr(:Times, 2, mxpr(:Power, 2, 1//2), mxpr(:Power, 5, 1//2))
@test mpow(9, 2//3) == mxpr(:Times, 3, mxpr(:Power, 3, 1//3))
@test mpow(-9,2//3) == mxpr(:Times, 3, mxpr(:Power, 3, 1//3), mxpr(:Power,-1,2//3))

using Base.Test

## replacement arithmetic

@test mmul(1,1) === 1
@test mmul(2.0,2) === 4.0
@test mmul(4,1//3) == 4//3
@test mmul(3,1//3) === 1
@test mmul(6,1//3) === 2
@test mmul(1//3,6) === 2
@test mmul(2.0,3.0) === 6.0
@test mplus(1,1) === 2
@test mplus(1,1.0) === 2.0
@test mplus(1//3,1//3) == 2//3
@test mplus(2//3,1//3) === 1
@test mdiv(2,3) == 2//3
@test mdiv(2,1//3) === 6
@test mdiv(2.0,3) == 2/3
@test mpow(2,3) === 8
@test mpow(2.0,3) === 2.0^3
@test mpow(2,-3) === 1//8
# bug: add nary methods
@test mplus(1,1,1) === 3
@test mplus(1,1,1,1) === 4
@test mmul(1,2,3,4) === 24

@test @sj 8 * 1//8 === 1
@test @sj 4 * 1//3 === 4//3

# Mxpr are never constructed, Expr is evaluated directly
@test @sj 1 + 2 + 3 + 4 === 10
@test @sj 1 * 2 * 3 * 4 === 24
@test @sj 1//3 + 1//3 + 1//3 === 1
@test (:a + :b ) + (:c + :d) == :a + :b + :c + :d

# FIXME
#  ERROR: DivideError: integer division error
# @test @sj(0/0) == Something

@test @sj( + aaa ) == @sj( aaa )
@test @sj(a * 0) == @sj(0*a) == 0

using Base.Test

## Simple numbers

@test (@sj 1)  == 1
let a
    @test (@sj a)  == :a
end

__bbbb = 1  # can't get local scope somehow
# Test meval(sym::Symbol)
@test (@sj __bbbb)  == 1
@test (@sj 1 + 1) == 2

@if_no_sjulia let c     # bug in sjulia causes :( c() ) to evaluate to c
    @test mxpr(:c) == @sj( c() )
end

## Canonical ordering

@test (@sj z + a + m + 2 + 1) == (@sj a + m + z + 3)

## Arithmetic (esp. Integer and Rational)

@test  (@sj 3 / 4) == 3//4
@test  (@sj 4 * (3 / 4)) == 3
@test  (@sj (3 / 4) * 4) == 3
@test  (@sj 6 / 3) == 2
@test  (@sj (3 / 4) * 1.0) == 3/4 == 0.75

## Test that Julia assignment and Mjulia assignement give the same thing
r1 = (@sj r = 3/4)
@test r1 == r == 3//4

@test typeof(Cos(1)) == Mxpr{:Cos}
@test typeof(Cos(:c)) == Mxpr{:Cos}
@test typeof(Cos(1.0)) == Float64

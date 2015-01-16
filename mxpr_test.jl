using Base.Test

## Simple numbers

@test (@jm 1)  == 1
let a
    @test (@jm a)  == :a
end

__bbbb = 1  # can't get local scope somehow
@test (@jm __bbbb)  == 1
@test (@jm 1 + 1) == 2


## Canonical ordering

@test (@jm z + a + m + 2 + 1) == (@jm a + m + z + 3)

## Arithmetic (esp. Integer and Rational)

@test  (@jm 3 / 4) == 3//4
@test  (@jm 4 * (3 / 4)) == 3
@test  (@jm (3 / 4) * 4) == 3
@test  (@jm 6 / 3) == 2
@test  (@jm (3 / 4) * 1.0) == 3/4 == 0.75

## Test that Julia assignment and Mjulia assignement give the same thing
r1 = (@jm r = 3/4)
@test r1 == r == 3//4

@test typeof(Cos(1)) == Mxpr{:Cos}
@test typeof(Cos(:c)) == Mxpr{:Cos}
@test typeof(Cos(1.0)) == Float64

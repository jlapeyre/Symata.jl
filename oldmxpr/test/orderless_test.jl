using Base.Test

## Test canonical ordering of 'orderless' (commutative) expressions.

# Restore these after fixing
let s,a

    @test @sj( 0 ) == 0
    @test @sj( -0 ) == 0
    @test @sj( 1 ) == 1
    @test @sj( -1 ) == -1
    @test @sj( 1 - 1 ) == 0
    @test @sj( 1 + 1 ) == 2

    a = :a
    @test @sj( a ) == a
    @test @sj( 0 + a ) == a
    @test @sj( a + 0 ) == a
    @test @sj( a - 0 ) == a
    @test @sj( 1 * a ) == a
    @test @sj( 0 * a ) == 0
    @test @sj( a * 1 ) == a
    @test @sj( a * 0 ) == 0
    @test @sj( a * -0 ) == 0
    @test @sj( -0 * a ) == 0

    s = @sj(1 + a + b + c)
    s[1] = :z
    @test margs(s) == @sj([a,b,c,z])  # terms are sorted
    margs(s)[1] = q
    @test margs(s) == @sj([q,b,c,z])  # terms are not sorted

    s = @sj(2 * a * b * c)
    s[2] = :z
    @test margs(s) == @sj([2,b,c,z])  # terms are sorted
    margs(s)[1] = q
    @test margs(s) == @sj([q,b,c,z])  # terms are not sorted
    @ma(s,1) = q1               # use macro
    @test margs(s) == @sj([q1,b,c,z])  # terms are not sorted

    @test margs(@sj(a + a)) == [2,a]
    @test margs(@sj(a + a + a)) == [3,a]
    @test margs(@sj(a + a^2 + a^3)) == [a,a^2,a^3]
    @test margs(@sj(a + 2*a^2 + a^3)) == [a,2*a^2,a^3]
    @test margs(@sj(a + a * a + a^3)) == [a,a^2,a^3]
    @test @sj(a + a * a - a * a) == a
    @test margs(@sj(a + z + 10*a^2 + 2 * z^2)) == [a,10*a^2,z,2*z^2]
    @test margs(@sj( a * a + 1 / ((z + y) * (z + y)) )) == [a ^ 2,(y + z) ^ -2]
end

nothing

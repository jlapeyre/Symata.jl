using Base.Test

# Restore these after fixing
let s,a

    @test @sj( 0 ) == 0
    @test @sj( -0 ) == 0
    @test @sj( 1 ) == 1
    @test @sj( -1 ) == -1
    @test @sj( 1 - 1 ) == 0
    @test @sj( 1 + 1 ) == 2
    
    # Fix handling scope! so we can make this local !
    a = :a
    @test @sj( a ) == a
    @test @sj( 0 + a ) == a
    @test @sj( a + 0 ) == a
    @test @sj( a - 0 ) == a

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
    
end

nothing

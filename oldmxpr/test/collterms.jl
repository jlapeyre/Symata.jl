using Bast.Test

let a,b
    @test newfunc(a,b)  == (false,0,a)
    @test newfunc(a,a)  == (true,2,a)
    @test newfunc(a,2a) == (true,3,a)

end

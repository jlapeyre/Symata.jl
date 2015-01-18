using Base.Test

let a,x
    @test  @sj(Cos(a))[0] == :Cos
    @test  @sj(Cos(a))[1] == :a
    @test  @sj(Cos(a*x))[0] == :Cos
    @test  @sj(Cos(a*x))[1] == @sj(a*x)
    @test  @sj(Cos(1))[1] == SJOne()
    @test  @sj(Cos(4Pi)) ==  SJOne()
    @test  @sj(Cos(3Pi)) == -SJOne()
    @test  @sj(typeof(Cos(1.0))) == SJFloat
    @test  @sj(Cos(1//3))[1] == 1//3
    @test  @sj(Cos(1/3))[1] == 1//3
    @test  @sj(Cos(0)) == SJZero()
    @test  @sj(Cos(ACos(x))) == :x
    @test  @sj(Cos(ASin(x))) == @sj((1-:x^2)^(1//2))
end

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
end

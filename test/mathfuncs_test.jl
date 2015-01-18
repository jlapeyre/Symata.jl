using Base.Test

let a
    @test  string(@sj(Cos(a))) == "Cos(a)"
    @test  string(@sj(Cos(1))) == "Cos(1)"
    @test  @sj(Cos(4Pi)) ==  SJOne()
    @test  @sj(Cos(3Pi)) == -SJOne()
    @test  @sj(typeof(Cos(1.0))) == SJFloat
end

# Test reading SJulia expressions from a file

@ex codefile = :( joinpath(Pkg.dir(), "SJulia", "test", "sjulia_code.mx") )
@testex Get(codefile) == [0, cosfixedpoint]

@ex codefile = :( joinpath(Pkg.dir(), "SJulia", "test", "replacerepeated.mx") )
@ex Get(codefile)

@testex replacerepeated(x^2 + y^6 , List(x => 2 + a, a => 3)) == 25 + y ^ 6

@ex ClearAll(codefile, cosfixedpoint,replacerepeated, x,i, y,a)

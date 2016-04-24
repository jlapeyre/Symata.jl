# Test reading SJulia expressions from a file

@ex codefile = :( joinpath(Pkg.dir(), "SJulia", "test", "sjulia_code.mx") )
@testex Get(codefile) == [0, cosfixedpoint]

@ex ClearAll(codefile, cosfixedpoint,x,i)

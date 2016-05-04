# Test reading SJulia expressions from a file

@ex ClearTemporary()
@testex testUserSyms

@ex codefile = :( joinpath(Pkg.dir(), "SJulia", "test", "sjulia_code.mx") )
@testex Get(codefile) == [0, cosfixedpoint]

@ex codefile = :( joinpath(Pkg.dir(), "SJulia", "test", "replacerepeated.mx") )
@ex Get(codefile)

@testex replacerepeated(x^2 + y^6 , List(x => 2 + a, a => 3)) == 25 + y ^ 6

@ex ClearAll(codefile, cosfixedpoint,replacerepeated, x,i, y,a)

@ex ClearTemporary()

# FIXME, I think  f(x_) = x does not give the correct result
#  f(x_) := x does work.
@ex ClearAll(f)
@ex f(x_,y_) := x^y
@ex f(x_Integer) := "Integer"
@ex f(x_AbstractString) := "String"
@ex file = TempName()
@ex Save(file,f)
@ex ClearAll(f)
@ex Get(file)
@ex Delete(file)
@testex f(2,3) == 8
@testex f(2) == "Integer"
@testex f("dog") == "String"

@ex ClearAll(f,file,x,y)


@testex testUserSyms

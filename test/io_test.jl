# Test reading SJulia expressions from a file

@ex ClearTemporary()
@testex testUserSyms

# Use julia code to create the path to the test file
@ex codefile = :( joinpath(Pkg.dir(), "SJulia", "test", "sjulia_code.mx") )
# The code defines a function, uses it for a calculation and returns the result
@testex Get(codefile) == [0, cosfixedpoint]

# This is an SJulia implementation of ReplaceRepeated
@ex codefile = :( joinpath(Pkg.dir(), "SJulia", "test", "replacerepeated.mx") )
@ex Get(codefile)
@testex replacerepeated(x^2 + y^6 , List(x => 2 + a, a => 3)) == 25 + y ^ 6

@ex ClearAll(codefile, cosfixedpoint,replacerepeated, x,i, y,a)

@ex ClearTemporary()
# FIXME, I think  f(x_) = x does not give the correct result
#  f(x_) := x does work.

@ex ClearAll(f)

# Create some definitions for f
@ex f(x_,y_) := x^y
@ex f(x_Integer) := "Integer"
@ex f(x_AbstractString) := "String"

# Save the definitions to a temporary file
@ex file = TempName()
@ex Save(file,f)

# Clear the definitions for f from memory
@ex ClearAll(f)
# Test that the definition is gone.
@testex Head(f(3)) == f

# Read the definitions back from the file and delete the file
@ex Get(file)
@ex Delete(file)

# Test that the defintions are restored
@testex f(2,3) == 8
@testex f(2) == "Integer"
@testex f("dog") == "String"

@ex ClearAll(f,file,x,y)


@testex testUserSyms

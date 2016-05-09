# Test reading SJulia expressions from a file

 ClearTemporary()
T testUserSyms

# Use julia code to create the path to the test file
 codefile = :( joinpath(Pkg.dir(), "SJulia", "test", "sjulia_code.mx") )
# The code defines a function, uses it for a calculation and returns the result
T Get(codefile) == [0, cosfixedpoint]

# This is an SJulia implementation of ReplaceRepeated
 codefile = :( joinpath(Pkg.dir(), "SJulia", "test", "replacerepeated.mx") )
 Get(codefile)
T replacerepeated(x^2 + y^6 , List(x => 2 + a, a => 3)) == 25 + y ^ 6

 ClearAll(codefile, cosfixedpoint,replacerepeated, x,i, y,a)

 ClearTemporary()

 ClearAll(f)

# Create some definitions for f
 f(x_,y_) := x^y
 f(x_Integer) := "Integer"
 f(x_AbstractString) := "String"

# Save the definitions to a temporary file
 file = TempName()
 Save(file,f)

# Clear the definitions for f from memory
 ClearAll(f)
# Test that the definition is gone.
T Head(f(3)) == f

# Read the definitions back from the file and delete the file
 Get(file)
### TODO. check that file is deleted
 DeleteFile(file)

# Test that the defintions are restored
T f(2,3) == 8
T f(2) == "Integer"
T f("dog") == "String"

 ClearAll(f,file,x,y)


T testUserSyms

# Test printing of expressions

T testUserSyms
 savestate = CompactOutput(True)

T ToString( (a+b)*x ) == "(a + b)*x"
T ToString( -2 * a) == "-2a"
T ToString( -2a) == "-2a"
T ToString( -1 * a) == "-a"
T ToString( -I ) == "-I"
T ToString(-1 * I) == "-I"
T ToString(-2 * I) == "-2I"
T ToString( (-1)^n ) == "(-1)^n"
T ToString( -1^n ) == "-(1^n)"
# FIXME
T If(BigIntInput(), True, ToString( - 1.0 * I ) == "-0.0 + -1.0I")
#T ToString( - 1.0 * I ) == "-0.0 + -1.0I"
T ToString(-I * a) == "-I*a"

#### HoldForm

f(x_) := HoldForm(x^2)
T ToString(Sum(f(i), [i,1,10])) == "1^2 + 2^2 + 3^2 + 4^2 + 5^2 + 6^2 + 7^2 + 8^2 + 9^2 + 10^2"
T Map(ReleaseHold, Sum(f(i), [i,1,10])) == 385

CompactOutput(savestate)
ClearAll(a,b,x,savestate,f,x,ex)
T testUserSyms

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

 CompactOutput(savestate)
 ClearAll(a,b,x,savestate)
T testUserSyms

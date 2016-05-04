# Test printing of expressions

@testex testUserSyms
@ex savestate = CompactOutput(True)

@testex ToString( (a+b)*x ) == "(a + b)*x"
#@testex ToString( -2 * a) == "-2*a"
@testex ToString( -2 * a) == "-2a"
@testex ToString( -2a) == "-2a"
@testex ToString( -1 * a) == "-a"
@testex ToString( -I ) == "-I"
@testex ToString(-1 * I) == "-I"
@testex ToString(-2 * I) == "-2I"
@testex ToString( (-1)^n ) == "(-1)^n"
@testex ToString( -1^n ) == "-(1^n)"
# FIXME
@testex If(BigIntInput(), True, ToString( - 1.0 * I ) == "-0.0 + -1.0I")
#@testex ToString( - 1.0 * I ) == "-0.0 + -1.0I"
@testex ToString(-I * a) == "-I*a"

@ex CompactOutput(savestate)
@ex ClearAll(a,b,x,savestate)
@testex testUserSyms

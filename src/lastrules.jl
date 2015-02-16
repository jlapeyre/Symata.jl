# We just put this here because there was some problem with load order.

unprotect(:ExpToTrig)

@ex ( ExpToTrig(ex_) := ReplaceRepeated( ex , E^(x_) => Cosh(x) + Sinh(x)) )

protect(:ExpToTrig)

@sjdoc ExpToTrig "
ExpToTrig(expr) replaces exponentials with trigonometric functions in expr.
But, the transformation from Cosh to Cos is not implemented.
"

# These are for testing downrules.
# We don't really want to eat our own dogfood with Log. These
# should be implemented more efficiently.
const directed_infinitym1 = setfixed(mxpr(:DirectedInfinity,-1))
unprotect(:Log)
@ex Log(1) := 0
# This is slow because the julia expression is parsed every time
@ex Log(0) := DirectedInfinity(-1)
protect(:Log)

@sjdoc Log "
Log(x) represents the natural logarithm of x.
Log(b,x) represents the base \"b\" logarithm of x.
"

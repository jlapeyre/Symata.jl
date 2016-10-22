# This code is unused

# All code written in Symata itself is here.
# There are more examples in the test directory. eg. io_test.jl
# These are examples; many are not practical.
#
# It is tested in the  file code_in_Symata_test.jl.

# Examples of using down values

# With the present implmentation, this is slower than
# writing an apprule directly in Julia. Patterns are not optimized
unset_attribute(:Sin,:Protected)
unset_attribute(:Tan,:Protected)
unset_attribute(:Cot,:Protected)
unset_attribute(:Cos,:Protected)
unset_attribute(:Sec,:Protected)
unset_attribute(:Csc,:Protected)

# The multiplication rules won't work generally until AC matching is implemened
# Some are not evaled far enough.
# Unfix is a workaround for bugs that prevent evaluation
@ex  Sin(-1*x_) := -1 * Sin(x)
@ex  Tan(-1*x_) := -1 * Tan(x)
@ex  Tan(ArcCos(x_)) := Unfix(Sqrt(1-x^2)/x)
@ex  Tan(ArcSin(x_)) := Unfix(x/Sqrt(1-x^2))
@ex  Sin(ArcSin(x_)) := x
@ex  Sin(ArcCos(x_)) := Unfix((1-x^2)^(1/2))
@ex  Cos(-1*x_) := Cos(x)

@ex  Power(Cos(x_),-1) ^= Sec(x)
@ex  Power(Sec(x_),-1) ^= Unfix(Cos(x)) # need to fix bug that requires Unfix!
@ex  Power(Sin(x_),-1) ^= Csc(x)
@ex  Power(Csc(x_),-1) ^= Sin(x)
@ex  Power(Tan(x_),-1) ^= Cot(x)
@ex  Power(Cot(x_),-1) ^= Tan(x)

set_attribute(:Sin,:Protected)
set_attribute(:Cos,:Protected)
set_attribute(:Cot,:Protected)
set_attribute(:Tan,:Protected)
set_attribute(:Sec,:Protected)
set_attribute(:Csc,:Protected)

# the symbol x should be local, anyway.
@ex ClearAll(x)

# We just put this here because there was some problem with load order.

unprotect(:ExpToTrig)

@ex ( ExpToTrig(ex_) := ReplaceRepeated( ex , E^(x_) => Cosh(x) + Sinh(x)) )

protect(:ExpToTrig)

@sjdoc ExpToTrig """
    ExpToTrig(expr)

replace exponentials with trigonometric functions in expr.
But, the transformation from Cosh to Cos is not implemented.
"""

# These are only for testing downrules.
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

unprotect(:Zeta)
@ex Zeta(1) := ComplexInfinity
protect(:Zeta)

### These would work for symata system code, but they
### add a few seconds to startup because fo jit.

### TODO: find a way to autoload this code

unprotect(:StringQ)
@ex StringQ = MatchQ(_AbstractString)
protect(:StringQ)

unprotect(:ListQ)
@ex ListQ = MatchQ(_List)
protect(:ListQ)

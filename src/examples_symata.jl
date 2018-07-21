# This code is unused
# Code written in Symata that is actually loaded is in ./symsrc

# Some code written in Symata itself is here.
#
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
@sym  Sin(-1*x_) := -1 * Sin(x)
@sym  Tan(-1*x_) := -1 * Tan(x)
@sym  Tan(ArcCos(x_)) := Unfix(Sqrt(1-x^2)/x)
@sym  Tan(ArcSin(x_)) := Unfix(x/Sqrt(1-x^2))
@sym  Sin(ArcSin(x_)) := x
@sym  Sin(ArcCos(x_)) := Unfix((1-x^2)^(1/2))
@sym  Cos(-1*x_) := Cos(x)

@sym  Power(Cos(x_),-1) ^= Sec(x)
@sym  Power(Sec(x_),-1) ^= Unfix(Cos(x)) # need to fix bug that requires Unfix!
@sym  Power(Sin(x_),-1) ^= Csc(x)
@sym  Power(Csc(x_),-1) ^= Sin(x)
@sym  Power(Tan(x_),-1) ^= Cot(x)
@sym  Power(Cot(x_),-1) ^= Tan(x)

set_attribute(:Sin,:Protected)
set_attribute(:Cos,:Protected)
set_attribute(:Cot,:Protected)
set_attribute(:Tan,:Protected)
set_attribute(:Sec,:Protected)
set_attribute(:Csc,:Protected)

# the symbol x should be local, anyway.
@sym ClearAll(x)

# These are only for testing downrules.
const directed_infinitym1 = setfixed(mxpr(:DirectedInfinity,-1))
unprotect(:Log)
@sym Log(1) := 0
# This is slow because the julia expression is parsed every time
@sym Log(0) := DirectedInfinity(-1)
protect(:Log)

@sjdoc Log "
Log(x) represents the natural logarithm of x.
Log(b,x) represents the base \"b\" logarithm of x.
"

unprotect(:Zeta)
@sym Zeta(1) := ComplexInfinity
protect(:Zeta)

### These would work for symata system code, but they
### add a few seconds to startup because fo jit.

### TODO: find a way to autoload this code

unprotect(:StringQ)
@sym StringQ = MatchQ(_AbstractString)
protect(:StringQ)

unprotect(:ListQ)
@sym ListQ = MatchQ(_List)
protect(:ListQ)

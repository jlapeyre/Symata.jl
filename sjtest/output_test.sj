# Test printing of expressions
 savestate = CompactOutput(True)

T ToString((-27/64)^(2/3)) == "(9/16)*(-1)^(2/3)"

T ToString( (a+b)*x ) == "(a + b)*x"
T ToString( -2 * a) == "-2a"
T ToString( -2a) == "-2a"
T ToString( -1 * a) == "-a"
T ToString(a - b) == "a - b"
T ToString(a + -3 * b) == "a - 3b"

T ToString(3 * 3^(1/3)) == "3*3^(1/3)"

T ToString((I+1)* a) == "(1 + I)*a"

T ToExpression(ToString(Expand((a-b)^5))) == Expand((a-b)^5)

T ToString( -I ) == "-I"
T ToString(-1 * I) == "-I"
T ToString(-2 * I) == "-2I"
T ToString("1/5 * I") == "1/5 * I"
T ToString( (-1)^n ) == "(-1)^n"
T ToString( -1^n ) == "-1^n"
# FIXME
T If(BigIntInput(), True, ToString( - 1.0 * I ) == "-0.0 + -1.0I")
#T ToString( - 1.0 * I ) == "-0.0 + -1.0I"
T ToString(-I * a) == "-I*a"

T ToString(Infinity) == "Infinity"
T ToString(-Infinity) == "-Infinity"
T ToString(1/0) == "ComplexInfinity"
T ToString(0/0) == "Indeterminate"

### HoldForm

f(x_) := HoldForm(x^2)

## FIXME. The first expression is what we want. If we don't evaluate the summand before doing
## the sum, we do get the first version.
#T ToString(Sum(f(i), [i,1,10])) == "1^2 + 2^2 + 3^2 + 4^2 + 5^2 + 6^2 + 7^2 + 8^2 + 9^2 + 10^2"
T ToString(Sum(f(i), [i,1,10])) == "1 + 4 + 9 + 16 + 25 + 36 + 49 + 64 + 81 + 100"

T Map(ReleaseHold, Sum(f(i), [i,1,10])) == 385

### Compound head

T ToString((f+g)(x)) == "(f + g)(x)"

### Format
T Format("{1}, {2}, {1}", (a+b)^3, 10) == "(a + b)^3, 10, (a + b)^3"
T Format("{1:04d} {2:*>10}",10,f+g) == "0010 *****f + g"

## Bug is in format
T If(BigIntInput(), True, Isa(Format(1/2, Conversion => "e"), String))
T Isa(Format(1.0, Conversion => "e"), String)
T If(BigIntInput(), True, Isa(Format(1, Conversion => "e"), String))

##  Issue #56
ToString(FullForm(f(x)(y))) == "f(x)(y)"

CompactOutput(savestate)

ClearAll(a,b,x,savestate,f,x,ex,n)

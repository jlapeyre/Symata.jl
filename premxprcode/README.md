### Experiment 1. Using Julia `Expr`

This older code and tests work:

```julia
include("PatRule.jl")
include("patrule_test_eval.jl")
```

The oldest is in the directory `premxprcode`. This uses Julia `Expr` and
Julia `Symbol` types to represent symbolic expressions. A
Mathematica-like pattern matcher is partly implemented, along with
`replace`, `replaceall`, etc.  Matching with conditions or a test on
the matched terms is works.  Eg. from the test code, this replaces
`x/x` by 1, unless the expression is `0/0`.

```julia
r =  @rule _ / _::((x)-> x != 0)  => 1
@test replaceall( :( 0 / 0) , r) == :( 0 / 0 )
@test replaceall( :( (a+b) / (a+b) ) , r) == 1
```

using Base.Test

@ex ClearAll(fib)
@ex fib(1) := 1
@ex fib(2) := 1
@ex fib(n_) := fib(n-1) + fib(n-2)
@testex fib(10) == 55
@ex Clear(fib)   # does not remove definition
@testex fib(11) == fib(10) + fib(9)
@ex ClearAll(fib)  # removes definition
@testex Head(fib(11)) == fib  # does not evaluate to number

# Test sorting of downvalues
# Enter downvalues in wrong order. 
@ex fib(n_) := fib(n-1) + fib(n-2)
@ex fib(1) := 1
@ex fib(2) := 1
@testex fib(10) == 55
@ex ClearAll(fib)

## Condition on pattern

@ex g(1) := "cat"
@ex g(x_Integer) = "int"
@ex g(x_Float64) = "float"

@testex  g(1) == "cat"
@testex  g(2) == "int"
@testex  g(2.0) == "float"

@ex ClearAll(g,h,a,b)
@ex h(x_^2) := x
@testex  h((a+b)^2) == a + b
@ex ClearAll(h)

@ex h(x_,x_) := 1
@ex h(x_,y_) := 2
@testex h(a,a) == 1
@testex h(a,b) == 2
@ex ClearAll(h)

## Restrictions on patterns. Match head and/or "pattern test"

SJulia.@ex      ClearAll(stringgt4,g,gt5)
SJulia.@ex      stringgt4(x_) := StringLength(x) > 4
SJulia.@ex      gt5(x_) := x > 5
SJulia.@ex      g(x_Integer:?(EvenQ)) := x
SJulia.@ex      g(x_AbstractString:?(stringgt4)) = "Greater than 4"
SJulia.@ex      g(x_AbstractFloat:?(gt5)) = 1
SJulia.@testex  Head(g(3)) == g
SJulia.@testex  g(4) == 4
SJulia.@testex  Head(g(5)) == g
SJulia.@testex  Head(g("cat")) == g
SJulia.@testex  g("zebra") == "Greater than 4"
SJulia.@ex      ClearAll(a,b,stringgt4,g,gt5)

## UpValues

# All subtypes of Integer match. All subtypes of FloatingPoint match.
SJulia.@ex ClearAll(a,p)
SJulia.@ex a^3 ^= p
SJulia.@testex a^3 == p
SJulia.@testex Apply(List,a^2) == [a,2]
SJulia.@testex a^BI(3) == p
SJulia.@ex ClearAll(a,p)

SJulia.@ex ClearAll(a,p)
SJulia.@ex a^4.0 ^= p
# Fix this
SJulia.@testex (a^4 == p) != True
SJulia.@testex a^BF(4) == p
SJulia.@testex a^4.0 == p
SJulia.@ex ClearAll(a,p)

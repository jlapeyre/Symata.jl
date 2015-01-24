using Base.Test

## These are a few tests while we reorganize the code.

@ex ClearAll(fib)
@ex fib(1) := 1
@ex fib(2) := 1
@ex fib(n_) := fib(n-1) + fib(n-2)
@test @ex(fib(10)) == 55
@ex Clear(fib)   # does not remove definition
@test @ex(fib(11)) == @ex(fib(10)) + @ex(fib(9))
@ex ClearAll(fib)  # removes definition
@test @ex(Head(fib(11))) == getsym(:fib)


## SetDelay for SJSym
@ex Clear(a,b,c)
@ex (a = 1)
@ex (b = a)
@ex (c := a)
@ex (a = 2)
@test @ex(b) == 1
@test @ex(c) == 2
@ex Clear(a,b,c)

## Condition on pattern

@ex g(1) := "cat"
@ex g(x_Integer) = "int"
@ex g(x_Float64) = "float"

@test @ex(g(1)) == "cat"
@test @ex(g(2)) == "int"
@test @ex(g(2.0)) == "float"

# Test replacement
@ex ClearAll(a,b,c,g)

@ex cossinrule = Cos(x_)^2 + Sin(x_)^2 => 1

# Does not match
@test @ex(Replace( Cos(a+b)^2 + Sin(a+c)^2, cossinrule)) == @ex(Cos(a+b)^2 + Sin(a+c)^2)

# Does not match
@test @ex(Replace( Cos(a+b)^2 + Sin(a+b)^2, cossinrule)) == 1

# One level down. Does not match
@testex Replace( g(Cos(a+b)^2 + Sin(a+b)^2), cossinrule) == g(Cos(a + b)^2 + Sin(a + b)^2)

# Match at every level
@testex ReplaceAll( g(Cos(a+b)^2 + Sin(a+b)^2), cossinrule) == g(1)

@testex Replace( a , a => 1) == 1
@testex Replace( b , a => 1) == b
#@ex ClearAll(a,b)

## Set a Julia variable
@ex(SetJ(a,"cat"))
@test Main.a == "cat"

## Test compound expression

@ex Clear(a)
@ex res = ( a = 3, 4)
@testex a == 3
@testex res == 4

@ex ClearAll(f,a,b)
@ex f(x_) := ( a = 1, x + 1 )
@testex   f(b) == b + 1
@ex ClearAll(f,a,b)

## Special rule

@test @ex(Cos(ACos(x))) == getsym(:x)
@testex Cos(ACos(x)) == x

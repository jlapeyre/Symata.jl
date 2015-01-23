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

@ex g(1) := "cat"
@ex g(x_Integer) = "int"
@ex g(x_Float64) = "float"

@test @ex(g(1)) == "cat"
@test @ex(g(2)) == "int"
@test @ex(g(2.0)) == "float"

@test @ex(Cos(ACos(x))) == getsym(:x)

# Test replacement
@ex Clear(a,b)
@ex cossinrule = Cos(x_)^2 + Sin(x_)^2 => 1
@test @ex(Replace( Cos(a+b)^2 + Sin(a+c)^2, cossinrule)) == @ex(Cos(a+b)^2 + Sin(a+c)^2)
@test @ex(Replace( Cos(a+b)^2 + Sin(a+b)^2, cossinrule)) == 1
@test @ex(Replace( a , a => 1)) == 1
@test @ex(Replace( b , a => 1)) == @ex(b)


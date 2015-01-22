using Base.Test

## These are a few tests while we reorganize the code.

@ex ClearAll(fib)
@ex fib(1) := 1
@ex fib(2) := 1
@ex fib(n_) := fib(n-1) + fib(n-2)
@test @ex(fib(10)) == 55

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

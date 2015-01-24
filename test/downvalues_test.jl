@ex ClearAll(fib)
@ex fib(1) := 1
@ex fib(2) := 1
@ex fib(n_) := fib(n-1) + fib(n-2)
@test @ex(fib(10)) == 55
@ex Clear(fib)   # does not remove definition
@test @ex(fib(11)) == @ex(fib(10)) + @ex(fib(9))
@ex ClearAll(fib)  # removes definition
@test @ex(Head(fib(11))) == getsym(:fib)

## Condition on pattern

@ex g(1) := "cat"
@ex g(x_Integer) = "int"
@ex g(x_Float64) = "float"

@test @ex(g(1)) == "cat"
@test @ex(g(2)) == "int"
@test @ex(g(2.0)) == "float"

@ex ClearAll(g)

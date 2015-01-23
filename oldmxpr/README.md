Run

```julia
include("src/SJulia.jl")
include("test/run_tests.jl")
```

See the tests for examples.  To enter expressions, you can use the
same macros that the tests use. This also works with "sjulia" terminal
mode, which wraps input in macro.

An example of the canonical ordering

```julia
sjulia> x^3 * x^2 + 2 * x * x^4  + 1 + z
1 + 3 * x ^ 5 + z
```

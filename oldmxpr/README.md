### Experiment 2. Using type `Mxpr` and Julia `Symbol` and some Julia evaluation.

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

This code is in the directory `oldmxpr`. An input `Expr` is captured
by a macro and some things are rewritten and a `Mxpr` is constructed.
In this code, there is no single sequence for evaluation of input
expressions. I did not want to write a language in Julia, but
rather to extend Julia with a package. I was thinking of
Mathematica-like pattern matching for Julia. But you really need
to put, and maintain, expressions in a canonical form for this to work.
For this reason and others a separate type `Mxpr` is used. Parts of
the expression are any other Julia type. Undefined symbols that are
encountered are set to themselves, i.e. thereafter, they evaluate
to themselves. This is done if input is wrapped in a macro.
I also edited "src/interperter.c" to do this. And I defined
`+(a::Symbol,b)`, etc. So then, with no macro, you can do

```julia
julia> a + 1
```

and get an `Mxpr`. But, I abandoned this approach. (By the way, "make test"
passed with this modification, so there is no test for undefined symbols.
I did not try make test all.)


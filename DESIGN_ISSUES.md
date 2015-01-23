**  How does one best take advantage of multiple dispatch and
  type stability ?

Currently, there is `abstract AbstractMxpr` and subtypes
`Mxpr{:op}`, where `:op` is `:mplus`, `:f`, `:Cos`, etc. In this kind of
CAS, there is an evaluation function eg `meval`, similar to `eval`.
So we can dispatch `meval` on the subtypes of `AbstractMxpr`. But, the
contents of `Mxpr` are in an `Array{Any,1}`. Further dispatch is done by
dispatching on the type of each element or, often by conditional branching.
For example `Mxpr{:mplus}`, might have a numerical term of any type
`T <: Number`. This is not encoded in the subtype of `AbstractMxpr`.

In the current code, many functions are not type-stable. Often
`1`, `0`, or number is returned, but `one()`, etc. are never used.
We almost certainly want `6 * 1//3 --> 6`.  The numerical coefficient
of `x` is `1`, but what type of `1`? Currently, the default types
are `Int` and `Float64` with no overflow checking.

** Pattern matching similar to Mathematica's is the most important
feature to implement. Mathematica's design is about 30 years
old. There are a few old papers discussing it. How to
implement/integrate it ?  Concise, clear syntax would be great.

** Syntax and parsing. Can we get what we want without touching the Julia
parser ? Currently, an `Mxpr` is typically constructed by altering a
quoted `Expr`, so the syntax is must be valid Julia syntax. One could
copy and alter the scheme parser and invoke it optionally, via a macro
or terminal mode or something. But, this incurrs a big complexity penalty.

** Organizing function calls, simplifier, meval, rewriting with patterns.

## Symbolic manipulation code.

This code is a sketch that is not meant to demonstrate computations,
but rather to test implementing core features.  I am posting it to get
feedback on design decisions. The code is cleaner in some places than
others. But, I expect most of it to be rewritten. I don't want to
spend time with localized inefficiencies. I am concerned about
efficiency and expressiveness that is difficult to change without big
rewrites.

```julia
include("src/Mxpr.jl")
include("src/mxpr_test.jl")
```

I added a mode to the Julia repl to support this code.

https://github.com/jlapeyre/julia/tree/jl/symrepl

You enter and exit the sjulia mode with '.'
Here is an example.

```julia
sjulia> ClearAll(fib)
sjulia> fib(1) := 1
sjulia> fib(2) := 1
sjulia> fib(n_) := fib(n-1) + fib(n-2)
sjulia> fib(10)
55
sjulia> addone(x_) := (a = 1,  x + a)  # compound expression
sjulia> addone(y)
1 + y
```

Symbols that are associated with some functionality can be listed with
`BuiltIns()` at the sjulia prompt, or `@ex BuiltIns()` at the julia
prompt.

Here are a few commands (at the sjulia repl, or as a argument to the @ex macro).

* `SetJ(x,val)` set a Julia variable
* `Clear(), ClearAll()` clear a symbol's value, or all associated rules, too.
* `DownValues(x)` rules associated with x
* `Attributes(x)` attributes of symbol x
* `Dump(x)`
* `Cos(x)`
* `Length(x)`
* `TraceOn()`
* `TraceOff()`
* `Replace(expr,rule)`
* `ReplaceAll(expr,rule)`
* `a = b` assignment
* `a := b` delayed assignment
* `f(x_) := x` delayed rule assignment

There are three versions, or experiments. Each one has test files
that run and serve as examples. The instructions for loading
the code and running the tests,  are in the subdirs.

Expressions can be entered at the Julia repl. For the two most recent
versions, there is also a repl mode "sjulia" in my Julia fork in the branch
jl/symrepl.  You enter and exit the mode with ".". The only file
changed in this branch is REPL.jl. The only thing the new mode does is
wrap input lines in a macro. Of course, a better thing would be to
add a facility that works like: `newrepl(:name, :wrappermacro)`.

### Latest experiment. Using type `Mxpr` and a symbol type `SJSym`

Mixing Julia and symbolic manipulation via patterns and term rewriting was problematic.
So the next step amounts to writing another language. Symbols, and "functions" and
"variables" in "SJulia" (I don't have a name) are completely separate
from Julia symbols (But, I currently bind Julia symbols as well, as a hack, to get completion
in the repl. This can be removed later.)  I am using Mathematica as a model in order to
get something working and see how it interacts with Julia. But, the design could be
changed. Some of the things from the earlier experiments are ported, but not all.

#### Pattern matching.

This still uses the same pattern matcher, but some features need to be implemented differently.
The most important missing feature is matching objects in a commutative (Orderless) expression.
Eg, `a + b` should match the terms in `a + c + b`, etc.

Patterns are used in several places. Eg, you can make a replacement rule. Eg

```julia
sjulia> cossinrule = Cos(x_)^2 + Sin(x_)^2 => 1
sjulia> Replace( Cos(a+b)^2 + Sin(a+c)^2, cossinrule) == Cos(a+b)^2 + Sin(a+c)^2
```

A replacement rule is associated with the symbol `f` like this

```julia
sjulia> f(x_) := x^2

sjulia> f(a+b)
(a+b)^2
```

When `f` is encountered as the head of an expression, a list of such rules is
tried. The first that succeeds makes the transformation and that round of evaluation
is done. You should also be able to  associate automatic rules with `f` like this
`g(f(x_)) ^= x^2`. But, this is not done yet.

You can see the evaluation sequence in `loopmeval` and `meval` in the code.

#### Parsing

I use the Julia parser and reinterpret the results. Maybe there is an elegant enough
way to get everything you need this way. But, probably copying and altering the
parser would be better even though it adds more complication. Eg. Now, I can use curly
braces for literal construction of lists, but get a deprecation warning. So I use
square brackets. Once you change the parser, you can ask whether you want full Mma
syntax. OTOH, staying close to Julia (and everyone else's) syntax is also reasonable.
OTOOH, Julia tries to make it easy to come from matlab. So making it easy to
come from Mma might be good (if this ultimately will look like Mma).

#### Data and dispatch

Big question, how to use best Julia features, (multiple dispatch, and
others) with this language ? There are of course numerous other
questions.  Eg, how to handle integer overflow.

Another thing, Mma offers no way to write code except in Mma. It
should be possible to write user or quasi-user level code *easily* in
Julia.

Symbols are currently done this way

```julia
type SJSym{T}  <: AbstractSJSym
    val::Any
    attr::Dict{Symbol,Bool}
    downvalues::Array{Any,1}
end

symname{T}(s::SJSym{T}) = T
sjsym(s::Symbol) = SJSym{s}(s,Dict{Symbol,Bool}(),Array(Any,0))
```

Of course the attributes should probably be a bit field, or stored somewhere
else. The downvalues field is a list of definitions like `f(x_) := x`. I am not
yet using the symbol subtype (parameter) for dispatch.

Expressions are done like this

```julia
type Mxpr{T} <: AbstractMxpr
    head::SJSym
    args::Array{Any,1}
end
```

There are `Protected` (reserved) symbols, like `Cos`,
and `RuleDelayed`. Evaluation of these is dispatched by the subtype.

<!--  LocalWords:  julia src sjulia repl ClearAll SetJ DownValues jl
 -->
<!--  LocalWords:  TraceOn TraceOff expr ReplaceAll subdirs symrepl
 -->
<!--  LocalWords:  newrepl wrappermacro premxprcode Mathematica Eg
 -->
<!--  LocalWords:  matcher replaceall Mxpr oldmxpr SJSym SJulia meval
 -->
<!--  LocalWords:  canonicalizer Orderless cossinrule loopmeval Mma
 -->
<!--  LocalWords:  Fateman IIRC OTOOH else's matlab AbstractSJSym
 -->
<!--  LocalWords:  Bool symname sjsym downvalues subtype AbstractMxpr
 -->
<!--  LocalWords:  RuleDelayed
 -->

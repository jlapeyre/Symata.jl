## Symbolic manipulation code

This is a partial, experimental, implementation of a language for
symbolic computation.  It is largely based on pattern matching and an
evaluation sequence modeled on Mathematica, although this by no means
a fixed decision.

I am making it available to get feedback on design decisions.  The
focus now is not on implementing specific mathematical computation,
but rather on testing implmentations of core features. The most
important of these features are pattern matching and the evaluation
sequence.

The main features implemented so far are: canonical ordering of sums
and products; pattern matching based on structure, "Head", and
"pattern test". Controlling evaluation is not implemented uniformly.
Some important items not yet implemented are: conditions on the entire pattern,
matching of commutative and associative terms.


```julia
include("src/SJulia.jl")
include("src/run_tests.jl")
```

I added a mode to the Julia repl to support this code (but it is not necessary)

https://github.com/jlapeyre/julia/tree/jl/symrepl

You enter and exit the sjulia mode with '.'
Here are some examples. See the test directory for others

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
sjulia> g(x_) := Module([a,b],(a=1,b=3,a+b+x))  # lexically scoped local vars
sjulia> gt5(x_) := x > 5     # conditions on patterns
sjulia> g(x_FloatingPoint:?(gt5)) = 1   # only matches floating point numbers > 5
sjulia> h(x_^2) := x    # Structural matching
sjulia> h((a+b)^2)
a + b
```

Symbols that are associated with some functionality can be listed with
`BuiltIns()` at the sjulia prompt, or `@ex BuiltIns()` at the julia
prompt.

Documentation for many BuiltIn symbols can be found by entering
`?, SymName` at the `sjulia` prompt. Note the comma, which is
neccessary because limitations in the provisional parsing method.
If examples are printed with the documentation string, they can be
evaluated, that is run, by entering `Example(SymName)` at the `sjulia`
prompt. The input strings from the examples are pushed to the history
so that they can be recalled and edited and re-evaluated.

Here are a few commands (at the sjulia repl, or as a argument to the @ex macro).
There are many more commands available, but mostly to support experimenting with
evaluation.

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

There are also two older versions, or experiments. Each one has test files
that run and serve as examples. The instructions for loading
the code and running the tests, are in the subdirs. They have some
examples of rules that have not yet been ported to the latest version.

Expressions can be entered at the Julia repl. For the two most recent
versions, there is also a repl mode "sjulia" in my Julia fork in the branch
jl/symrepl.  You enter and exit the mode with ".". The only file
changed in this branch is REPL.jl. The only thing the new mode does is
wrap input lines in a macro. Of course, a better thing would be to
add a facility that works like: `newrepl(:name, :wrappermacro)`.

#### Evaluation

There are lines at the top of the file `src/mxpr.jl` to control evaluation. You can
choose infinite or single evaluation. The test suite relies on infinite evaluation being
enabled. Hashing and caching of expressions can also be chosen here.

#### Pattern matching.

An important feature not yet implemented is matching objects in a commutative (Orderless) expression.
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

Big question: how to use best Julia features, (multiple dispatch, and
others) with this language ? There are of course numerous other
questions.  Eg, how to handle integer overflow.

Another thing, Mma offers no way to write code except in Mma. It
should be possible to write user or quasi-user level code easily in
Julia.

Symbols are currently done this way (this is a bit outdated)

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
<!--  LocalWords:  RuleDelayed addone lexically FloatingPoint
 -->
<!--  LocalWords:  BuiltIns
 -->

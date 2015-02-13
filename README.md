## Symbolic manipulation code

This is a partial, experimental, implementation of a language for
symbolic computation.  It is largely based on pattern matching and an
evaluation sequence modeled on Mathematica, although this by no means
a fixed decision.

The focus now is not on implementing specific mathematical
computation, but rather on testing implmentations of core features and
subsystems. Some important features are pattern matching and the
evaluation sequence and data structures that support it.

### Installing

You can load and test SJulia like this

```julia
include("src/SJulia.jl")
include("src/run_tests.jl")
```

I added a mode to the Julia repl to support this code (but it is not necessary)
in this branch of a fork of Julia

https://github.com/jlapeyre/julia/tree/jl/symrepl

In fact, the only file changed in this branch is base/REPL.jl.  To use
this mode. Download the branch and build it and install it somewhere
as, say, sjulia. You enter and exit the SJulia mode with '.' Working
from this mode is similar to working from Mathematica or Maxima or
Maple. For the most part, the SJulia mode just wraps input in the macro
`ex`. So you can get the same thing by typing

```julia
julia> @ex some SJulia expression
julia> @ex(some expression that may look like two expressions)
```

### Finding Help and Examples

Symbols that are associated with some functionality can be listed with
`BuiltIns()` at the sjulia prompt, or `@ex BuiltIns()` at the julia
prompt.

Documentation for many BuiltIn symbols can be found by entering
`?, SymName` at the `sjulia` prompt. Note the comma, which is
neccessary because limitations in the provisional parsing method.
`Help(Symname)` prints the same documentation. This allows you
to type `@ex Help(SymName)` from Julia.
If examples are printed with the documentation string, they can be
evaluated, that is run, by entering `Example(SymName)` at the `sjulia`
prompt. The input strings from the examples are pushed to the history
so that they can be recalled and edited and re-evaluated.

There are many examples in the test directory.

### A few examples

Here are some examples of the SJulia mode.

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

Using the SJulia mode or the `@ex` macro is essentially using a language distinct
from Julia. In particular, the evaluation sequence, and binding of symbols to
storage is separate from Julia. But there is also some work done on allowing
use of SJulia direclty from Julia. For instance, these

```julia
julia> ex = :x + 1
1 + x
julia> m = Expand((:a+:b)*(:x+1)^2)
a + b + 2*a*x + 2*b*x + a*(x^2) + b*(x^2)
````

make Julia bindings of SJulia expressions to the symbols ex and m.

Here are a few commands (at the sjulia repl, or as a argument to the @ex macro).
There are many more commands available, mostly to support experimenting with
design of SJulia.

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

There are also two older experiments in this distribution. Each one
has test files that run and serve as examples. The instructions for
loading the code and running the tests, are in the subdirs.

#### Evaluation

There are lines at the top of the file `src/mxpr.jl` to control evaluation. You can
choose infinite or single evaluation. The test suite relies on infinite evaluation being
enabled. Hashing and caching of expressions can also be chosen here, but it is not very
useful at the moment.

#### Pattern matching.

Patterns are used in several places. Eg, you can make a replacement rule. Eg

```julia
sjulia> cossinrule = Cos(x_)^2 + Sin(x_)^2 => 1;

sjulia> Replace( Cos(a+b)^2 + Sin(a+c)^2, cossinrule)
(a+b)^2 + Sin(a+c)^2

sjulia> Replace( Cos(a+b)^2 + Sin(a+b)^2, cossinrule)
1
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

You can see the evaluation sequence in `infseval` and `meval` in the code.

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
type SSJSym{T}  <: AbstractSJSym
    val::Array{T,1}
    attr::Dict{Symbol,Bool}    # attributes associated with the symbol
    downvalues::Array{Any,1}  # These are transformation rules for the symbol
    age::UInt64             # Time stamp, last time assignment or other changes were made
end
```

Of course the attributes should probably be a bit field, or stored somewhere
else. The downvalues field is a list of definitions like `f(x_) := x`. I am not
yet using the symbol subtype (parameter) for dispatch.

Expressions are done like this

```julia
type Mxpr{T} <: AbstractMxpr
    head::SJSym
    args::MxprArgs
    fixed::Bool    # Does Mxpr evaluate to itself in current environment ?
    canon::Bool    # Is Mxpr in canonical form ?
    syms::FreeSyms # List of free symbols, whose timestamps we check
    age::UInt64    # Timestamp of this Mxpr
    key::UInt64    # Hash code
    typ::DataType  # Not used
end
```

At the moment, the field `head` and the parameter T are the same. Evaluation of
expressions is dispatched by functions with type annotations for each head, such
as `Mxpr{:Power}`.


<!--  LocalWords:  julia src sjulia repl ClearAll SetJ DownValues jl
 -->
<!--  LocalWords:  TraceOn TraceOff expr ReplaceAll subdirs symrepl
 -->
<!--  LocalWords:  newrepl wrappermacro premxprcode Mathematica Eg
 -->
<!--  LocalWords:  matcher replaceall Mxpr oldmxpr SJSym SJulia meval
 -->
<!--  LocalWords:  canonicalizer Orderless cossinrule  Mma
 -->
<!--  LocalWords:  Fateman IIRC OTOOH else's matlab AbstractSJSym
 -->
<!--  LocalWords:  Bool symname sjsym downvalues subtype AbstractMxpr
 -->
<!--  LocalWords:  RuleDelayed addone lexically FloatingPoint
 -->
<!--  LocalWords:  BuiltIns
 -->

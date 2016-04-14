## Symbolic manipulation code

SJulia is a a language for symbolic computation.  It is largely
modeled on the pattern matching and evaluation sequence of
Mathematica. Evaluation, pattern matching are written in Julia. Much
of the mathematics and symbolic manipulation is achieved by wrapping SymPy.

### Installing

SJulia is not a registered module, so it cannot be installed via `Pkg.add`.
Instead, it can be installed and used as follows

```julia
julia> Pkg.clone("https://github.com/jlapeyre/SJulia.git")
julia> using SJulia
sjulia> Help()    # type '=' alone on a line to enter sjulia mode
```

*Note*: `SJulia` depends on the Julia
[`PyCall`](https://github.com/stevengj/PyCall.jl) module, and [SymPy](http://www.sympy.org/en/index.html).

`SJulia` works with the v0.4 and v0.5 branches of Julia. It will probably not work with v0.3.

You can test  `Pkg.test("SJulia")`.

#### SJulia Repl

There is an SJulia command line (REPL) mode. You enter
the mode by typing `=` as the first character on a line. Type backspace to exit the SJulia
mode.  In this mode, the input is not interpreted as Julia code, but rather SJulia code.
You can do tab completion for builtin, Protected, symbols.

#### Some results.

Here is counting with patterns. The execution time is about the same as Mma 3.

```
sjulia> b = Range(10^5);   # [1,2,...,100000]
elapsed time: 0.001218053 seconds (2395840 bytes allocated)

sjulia> Count(b, 2)     # Count the number of 2's
elapsed time: 0.008307134 seconds (1608 bytes allocated)
1

sjulia> Count(b, _Integer)
elapsed time: 0.022361488 seconds (1594520 bytes allocated)
100000

sjulia> Count(b,_String)
elapsed time: 0.015774562 seconds (1594488 bytes allocated)
0

sjulia> Count(b, _:?(EvenQ))
elapsed time: 0.075666716 seconds (3984008 bytes allocated)
50000

sjulia> Count(b, _:?( :( x -> x > 5 ) ) )    # Use a Julia function as the test
elapsed time: 0.076713741 seconds (4780808 bytes allocated)
99995

sjulia> countprimes = Count(_:?(PrimeQ))   # Currying, like in Mma 10

sjulia> countprimes(b)
elapsed time: 0.167145648 seconds (16778920 bytes allocated)
9592

```

#### Evaluation

Like, Mma, SJulia does evaluation to a fixed point, always effectively re-evaluating in the
current environment. There are pros and cons to this approach. In Mma there are a host
of `HoldXXX` symbols to prevent evaluation, and in
[Maxima] (http://maxima.sourceforge.net/) and Maple a menagerie of `eval`
functions and options to force it.

Evaluation to a fixed point can be expensive. Here is an example in [Mathics](http://www.mathics.org/)

```
In[1]:= Timing[ m = Expand[(a+b)^1000];]
Out[1]= {6.282803, Null}

In[2]:= Timing[ m[[1]] ]
Out[2]= {3.203408, a ^ 1000}
```

Every time an element of `m` is accessed, the entire expression is reevaluated.
SJulia is about 700 times faster generating `m`, and 85000 times
faster retrieving a value. The latter time is mostly the difference between
an `O(1)` and `O(n)` algorithm. [SymPy](http://www.sympy.org/en/index.html)  does the expansion for Mathics, and it
is about 100 times slower than SJulia (this includes SJulia's fixed point evaluation)
Mathematica 3 is about two times faster than
SJulia generating `m`. (*Update*: SJulia now depends on SymPy as a backend, with a few
functions implemented.)

(There are caveats interpreting these results of course.
Just to name three; 1) Mma 3 did not yet use GMP numbers, and the expansion is
heavy on calculations with big integers. 2) SymPy `expand` handles more cases,
which may increase time complexity.
3) It seems that SymPy caches results, which consumes time upfront, but saves time later. Caching is currently
disabled in SJulia.)

Here is SJulia doing expansion.
We need to quickly evaluate an expression and track whether it needs to be re-evaluated:
```julia
sjulia> m = Expand((a+b)^BI(1000));   # expand with BigInt exponent
elapsed time: 0.008785756 seconds

sjulia> m[2]             # get a single value without re-evaluating
elapsed time: 3.7545e-5 seconds (992 bytes allocated)
1000*(a^999)*b

sjulia> a = 1;

sjulia> m[2]           # slower because we re-evaluate everything
elapsed time: 0.075710831
1000*b

sjulia> ClearAll(a)

sjulia> m[2]     # re-evaluate, it is still relatively slow.
elapsed time: 0.040376351 seconds (3723352 bytes allocated)
1000*(a^999)*b

sjulia> m[2]    # no evaluation, expression has been marked as fixed again.
elapsed time: 3.7984e-5 seconds (992 bytes allocated)
tryrule count: downvalue 0, upvalue 0
1000*(a^999)*b

sjulia> m[3]   # we can iterate over m without re-evaluating
elapsed time: 8.2968e-5 seconds (1544 bytes allocated)
499500*(a^998)*(b^2)
```

Quickly generate a large list of numbers and access a value.
```julia
sjulia> a = Range(10^5);
elapsed time: 0.001219758 seconds (2394728 bytes allocated)

sjulia> a[-1]
elapsed time: 3.5242e-5 seconds (976 bytes allocated)
100000

sjulia> a[-1] = d
elapsed time: 3.7307e-5 seconds (816 bytes allocated)
d

sjulia> Apply(Plus,a)
elapsed time: 0.005774109 seconds
4999950000 + d
```

#### Alternate way to evaluate SJulia expressions

Instead of working with the SJulia REPL (by typing `=`)
You can get the same thing by typing

```julia
julia> using SJulia
julia> @ex some SJulia expression
julia> @ex(some expression that may look like two expressions)
```

### Finding Help and Examples

Try `Help()`. Symbols that are associated with some functionality can be listed with
`BuiltIns()` at the sjulia prompt, or `@ex BuiltIns()` at the julia
prompt.

Documentation for for many BuiltIn symbols is reproduced at the end of
this document.

This documentation can be printed from within SJulia
by entering `?, SymName` at the `sjulia` prompt. Note the comma, which
is necessary because limitations in the provisional parsing method.
`Help(Symname)` prints the same documentation. This allows you to type
`@ex Help(SymName)` from Julia.

To print a list of all help topics, type `?,` or `Help()`.

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
use of SJulia directly from Julia. For instance, these

```julia
julia> ex = :x + 1
1 + x
julia> m = Expand((:a+:b)*(:x+1)^2)
a + b + 2*a*x + 2*b*x + a*(x^2) + b*(x^2)
````

make Julia bindings of SJulia expressions to the symbols ex and m.

Here are a few commands (or functions) (at the sjulia repl, or as a argument to the @ex macro).
About 300 functions (really 'Heads') are implemented, many of them wrapping SymPy functions.

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

#### More on Evaluation

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
is done.

#### Upvalues

```julia
sjulia> g(f(x_)) ^= x^2
sjulia > g(f(z))
 z ^ 2
```

You can see the evaluation sequence in `infseval` and `meval` in the code.

#### Parsing and Syntax

I use the Julia parser and reinterpret the results. Maybe there is an elegant enough
way to get everything you need this way. But, copying and altering the
parser might be better even though it adds more complication. Eg. Now, I can use curly
braces for literal construction of lists, but get a deprecation warning. So I use
square brackets.

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
<!--  LocalWords:  newrepl wrappermacro premxprcode Mathematica Eg di
 -->
<!--  LocalWords:  matcher replaceall Mxpr oldmxpr SJSym SJulia meval
 -->
<!--  LocalWords:  canonicalizer Orderless cossinrule  Mma BigInt arg
 -->
<!--  LocalWords:  Fateman IIRC OTOOH else's matlab AbstractSJSym eg
 -->
<!--  LocalWords:  Bool symname sjsym downvalues subtype AbstractMxpr
 -->
<!--  LocalWords:  RuleDelayed addone lexically FloatingPoint tryrule
 -->
<!--  LocalWords:  BuiltIns downvalue upvalue Maxima BuiltIn SymName
 -->
<!--  LocalWords:  infseval SSJSym timestamps Timestamp TimeOff AtomQ
 -->
<!--  LocalWords:  TimeOn TrDownOff TrDownOn TrUpOff TrUpOn EvenQ sym
 -->
<!--  LocalWords:  OddQ Builtin BigFloat BigInts builtin ByteCount tf
 -->
<!--  LocalWords:  UserSyms CompoundExpression DirtyQ timestamp HAge
 -->
<!--  LocalWords:  Syms imax imin SetDelayed UpSet UpValues DumpHold
 -->
<!--  LocalWords:  HoldPattern SomeHead FactorInteger incr Println gg
 -->
<!--  LocalWords:  FullForm tbranch fbranch IntegerDigits JVar Jxpr
 -->
<!--  LocalWords:  LeafCount NodeCount Dict's MatchQ myintq mx args
 -->
<!--  LocalWords:  zz ie StringInterpolation StringLength countprimes
 -->
<!--  LocalWords:  str HoldForm ToExpression ToString ToStringLength
 -->
<!--  LocalWords:  DownRules UpRules unsets Unprotect PrimeQ HoldXXX
 -->
<!--  LocalWords:  eval Mathics SymPy SJulia's GMP backend AddTo lim
 -->
LocalWords:  ConstantArray regex metadata tol PCRE DataType TimesBy
LocalWords:  PyCall Upvalues

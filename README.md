# SJulia.jl
### Symbolic mathematics language

Linux, OSX: [![Build Status](https://travis-ci.org/jlapeyre/SJulia.jl.svg)](https://travis-ci.org/jlapeyre/SJulia.jl)

[![Coverage Status](https://coveralls.io/repos/jlapeyre/SJulia.jl/badge.svg?branch=master&service=github)](https://coveralls.io/github/jlapeyre/SJulia.jl?branch=master)

[![codecov.io](http://codecov.io/github/jlapeyre/SJulia.jl/coverage.svg?branch=master)](http://codecov.io/github/jlapeyre/SJulia.jl?branch=master)

### What SJulia is

1. SJulia is a language for symbolic computations and mathematics, where, for
the most part, "mathematics" means what it typically
does for a scientist or engineer.

2. It is a language based mostly on expressions, on "evaluating" and
rewriting them, like Wolfram, Maple, or Maxima. It is neither a language,
nor an extension of a language, that is mostly procedural, or designed
around data types and functions, or a hierarchy of classes, etc.,
like C or Python or Java. Nor is it language like Sage;
that is, one meant to provide a unifying interface to a number of
mathematics languages with various programming models.

3. It is meant to be useful to people who do not like to program computers, as
well as those who do. The former includes people who prefer not to
think about classes, methods, objects, dispatch, stack traces, etc.

SJulia is largely modeled on the pattern matching and evaluation sequence of
Mathematica. Evaluation, pattern matching, flow control, etc. are
written in Julia. Much of the mathematics and symbolic manipulation is
achieved by wrapping SymPy. There are more than 300 functions
implemented, including integration, transformation of special
functions, expression manipulation, writing and reading expressions to
and from a file etc. The best places for examples of what works (and
what does not), are [the test directory](sjtest/), and, at the SJulia
prompt, the online help, `? Topic`, with TAB completion, and regex
search `h"word"`.

The core of SJulia is not complete. But, a large number of features are implemented;
enough to be useful. Here are [examples of pattern matching](sjtest/patterns_test.sj).

### Installing

SJulia depends on the [`PyCall`](https://github.com/stevengj/PyCall.jl) package and
the python [`sympy`](http://www.sympy.org/en/index.html) module.
The best way to install `sympy` is via `PyCall`, which will use [`Conda`](https://github.com/JuliaPy/Conda.jl),
which will install `python` and needed modules in your `Julia` directory.
However, `PyCall` must be configured to not use you system version of `python`.
If you do not have `PyCall` installed, do this

```julia
julia> ENV["PYTHON"]=""
julia> Pkg.add("PyCall")
```

If you *do* have `PyCall` installed, but it is configured to use your system `python`, reconfigure
it like this.

```julia
julia> ENV["PYTHON"]=""
julia> Pkg.build("PyCall")
```

If you use linux, you may have your distribution's `sympy` package installed and it may be
out of date. In this case, try the procedure above, and/or try removing your distribution's `sympy` package.

SJulia is not a registered module, so it cannot be installed via `Pkg.add`.
Instead, it can be installed and used as follows

```julia
julia> Pkg.clone("https://github.com/jlapeyre/SJulia.git")
julia> using SJulia
sjulia> Help()    # type '=' alone on a line to enter sjulia mode
```

#### note

`SymPy`, or `sympy`, here refers to the python [SymPy](http://www.sympy.org/en/index.html) distribution
(sometimes called sympy), *not* the Julia package `SymPy`. `SJulia` does not require the Julia package
[SymPy.jl](https://github.com/jverzani/SymPy.jl), which has a different goal.

You'll also need to install the `mpmath` package for python. This
should be automatically installed when installing `sympy` via
`PyCall`, which uses `Conda`.  The above should also work on OS
X. However, if you use `pip`, you should just be able to run `pip
install mpmath`.

`SJulia` works with the v0.4, v0.5, and v0.6 versions of Julia.

### SJulia REPL mode

There is an SJulia command line (REPL) mode.  To use the mode, there
is an executable [`sjulia`](sjulia) included in top level directory of this distribution. It is a (UNIX
sh) shell script that just starts julia and loads the module.

```sh
#
julia -i -e "using SJulia" $*
```

The REPL is also available simply by loading the module in a julia session via
`using SJulia`.

Toggle between Julia and SJulia modes by typing `=` as the first character on a line.
(If loading SJulia from the `julia` prompt via `using SJulia`, you use `=` and backspace.)
In SJulia mode, the input is not interpreted as Julia expressions, but rather SJulia expressions.
You can do tab completion to see a list of functions and symbols.

#### Finding Help and Examples

The best source of examples is [the test directory](sjtest/).
The documentation can be printed from within SJulia by entering `? SymName`
at the `sjulia` prompt.  `Help(Symname)` prints the same
documentation. For many SJulia functions, the SymPy docstring is
printed along with the SJulia documentation.

Try `Help()`. Type `h"topic"` to search for items containing the
string `"topic"`.  Hit TAB at the command line REPL for a list of all
builtin symbols. (i.e. variables and functions) Symbols that are
associated with some functionality can be listed with
`BuiltIns()`. Type `Example()` to see a list of topics with examples.
Type `Example(topic)` to run the examples. (But, far more examples are
in [the test directory](sjtest/) ). The input strings from the examples are pushed
to the history so that they can be recalled and edited and
re-evaluated.

##### Tests

Run the test suite from the `sjulia` prompt with `Tests()`.
This runs tests in [the directory sjtest.](sjtest/)

`Pkg.test("SJulia")` runs the older test suite. It is not being updated and is less complete
than the suite described above. For reasons explained in the code, the
tests in [sjtest](sjtest/) are preferred over those in [test](test/).

#### A few examples

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
sjulia> g(x_Float:?(gt5)) = 1   # only matches floating point numbers > 5
sjulia> h(x_^2) := x    # Structural matching
sjulia> h((a+b)^2)
a + b
```

#### SymPy

Functions that call SymPy work as follows (many more examples are in the test directory). This SymPy call
```python
integrate(exp(-t) * t**(a-1), (t,0,oo), conds='none')
```
corresponds to this SJulia expression
```julia
Integrate( Exp(-t)*t^(a-1),[t,0,Infinity], conds => "none")
```

#### Pattern matching.

Patterns are used in several places. For example you can make a replacement rule like this

```julia
sjulia> cossinrule = Cos(x_)^2 + Sin(x_)^2 => 1;

sjulia> Replace( Cos(a+b)^2 + Sin(a+c)^2, cossinrule)
(a+b)^2 + Sin(a+c)^2

sjulia> Replace( Cos(a+b)^2 + Sin(a+b)^2, cossinrule)
1
```

But, in practice, you would use `TrigSimp` or `Simplify`.

`Functions` are really replacement rules.  A replacement rule is associated with the symbol `f` like this
```julia
sjulia> f(x_) := x^2

sjulia> f(a+b)
(a+b)^2
```

When `f` is encountered as the head of an expression, a list of such rules is
tried. The first that succeeds makes the transformation and that round of evaluation
is done.

#### Miscellaneous Tips

To recall previous lines of input, use `O`, `OO`, etc. Or use `Out(n)` for the `n`th output.
Type `? HistoryLength` to see how to control saving output.

`BigIntInput(True)` enables making all input integers arbitrary precision.

You can use `Save` and `Get` to write and read SJulia code.

##### Rules and Assignment

You can use the symbols `:>` for `RuleDelayed`, `=>` for `Rule`, `^:=` for `UpSetDelayed`,
`=` for `Set`, `:=` for `SetDelayed`, and `^=` for `UpSet`.


#### Upvalues

"Upvalues" are (partially) implemented:
```julia
sjulia> g(f(x_)) ^= x^2
sjulia > g(f(z))
 z ^ 2
```

#### Results on evaluation and efficiency

*Note* Some of the times given below are slower with more recent
versions of SJulia. This is because the pattern matching code has been
rewritten to include more features, but has not been optimized. In
particular, `PatternTest` is 5 or 6 times slower because it is no
longer "compiled", but is reevaluated every time the pattern is
used. It probably won't be compiled again until the pattern matching
code is nearly completed.

*Note* As with everything in Julia, the following are not the times you get the first time you run
these commands, as Julia uses JIT compiling.

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

#### Fixed-point evaluation

SJulia does evaluation to a fixed point, always effectively re-evaluating in the
current environment.

Here is SJulia doing expansion with the native `ExpandA`. `ExpandA` is
only for demonstration; The much more capable `Expand` is a frontend
to a SymPy function. However the results following the first line
below, work as well if you call `Expand`.  We need to quickly evaluate
an expression and track whether it needs to be re-evaluated:

```julia
sjulia> m = ExpandA((a+b)^BI(1000));   # expand with BigInt exponent
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

#### Using SJulia language features directly from Julia

As an experiment, a few functions can be called directly from Julia

```julia
julia> Cos(3*Pi)
-1

julia> ex = Expand( (:x+:y)^3)
x^3 + y^3 + 3x*(y^2) + 3y*(x^2)

julia> Factor(ex)
(x + y)^3

julia> Integrate(:x^2, [:x,0,1])
1/3

julia> Integrate(:x^2, :x)
(1/3)*(x^3)

julia> Integrate(Cos(:x^2), :x)
(1/8)*(2^(1/2))*(Pi^(1/2))*(Gamma(5/4)^(-1))*FresnelC(x*(2^(1/2))*(Pi^(-1/2)))*Gamma(1/4)
```


<!--  LocalWords:  Mathematica SymPy julia sjulia PyCall Mma src REPL
 -->
<!--  LocalWords:  EvenQ countprimes PrimeQ HoldXXX Maxima eval regex
 -->
<!--  LocalWords:  Mathics SJulia's backend ExpandA BigInt ClearAll
 -->
<!--  LocalWords:  tryrule downvalue upvalue BuiltIns BuiltIn SymName
 -->
<!--  LocalWords:  Symname addone lexically FloatingPoint cossinrule
 -->
<!--  LocalWords:  TrigSimp Upvalues SJulia sjtest docstring builtin
 -->
<!--  LocalWords:  oo conds th HistoryLength BigIntInput RuleDelayed
 -->
<!--  LocalWords:  UpSetDelayed SetDelayed UpSet frontend FresnelC
 -->

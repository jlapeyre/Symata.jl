# Symata.jl

*Symbolic mathematics language*

Linux, OSX: [![Build Status](https://travis-ci.org/jlapeyre/Symata.jl.svg)](https://travis-ci.org/jlapeyre/Symata.jl)
&nbsp;
Windows: [![Build Status](https://ci.appveyor.com/api/projects/status/github/jlapeyre/Symata.jl?branch=master&svg=true)](https://ci.appveyor.com/project/jlapeyre/symata-jl)
&nbsp; &nbsp; &nbsp;
[![Coverage Status](https://coveralls.io/repos/github/jlapeyre/Symata.jl/badge.svg?branch=master)](https://coveralls.io/github/jlapeyre/Symata.jl?branch=master)
[![codecov.io](http://codecov.io/github/jlapeyre/Symata.jl/coverage.svg?branch=master)](http://codecov.io/github/jlapeyre/Symata.jl?branch=master)

[![Symata](http://pkg.julialang.org/badges/Symata_0.4.svg)](http://pkg.julialang.org/?pkg=Symata&ver=0.4)
[![Symata](http://pkg.julialang.org/badges/Symata_0.5.svg)](http://pkg.julialang.org/?pkg=Symata&ver=0.5)
[![Symata](http://pkg.julialang.org/badges/Symata_0.6.svg)](http://pkg.julialang.org/?pkg=Symata&ver=0.6)

### Symata is

- a language for symbolic computations and mathematics, where, for
the most part, "mathematics" means what it typically
does for a scientist or engineer.

- a language based mostly on expressions, on "evaluating" and
rewriting them, like Wolfram, Maple, or Maxima. It is neither a language,
nor an extension of a language, that is mostly procedural, or designed
around data types and functions, or a hierarchy of classes, etc.,
like C or Python or Java. Nor is it language like Sage;
that is, one meant to provide a unifying interface to a number of
mathematics languages with various programming models.

- meant to be useful to people who do not like to program computers, as
well as those who do. The former includes people who prefer not to
think about classes, methods, objects, dispatch, stack traces, etc.

Symata is largely modeled on the pattern matching and evaluation sequence of
Mathematica. Evaluation, pattern matching, flow control, etc. are
written in Julia. Much of the mathematics and symbolic manipulation is
achieved by wrapping SymPy. There are more than 300 functions
implemented, including integration, transformation of special
functions, expression manipulation, writing and reading expressions to
and from a file etc. The best places for examples of what works (and
what does not), are [the test directory](sjtest/), and, at the Symata
prompt, the online help, `? Topic`, with TAB completion, and regex
search `h"word"`.

The core of Symata is not complete. But, a large number of features are implemented;
enough to be useful. Here are [examples of pattern matching](sjtest/patterns_test.sj).

### IJulia Notebooks

Tutorial notebooks are in [the example directory](examples/). (The typeset math will look better
in the IJulia notebook, which renders it with higher quality than github.)

### Installing

`Symata` can be installed on Linux, OSX, and Windows, and Julia v0.4, v0.5, and v0.6.
It depends on the [`PyCall`](https://github.com/stevengj/PyCall.jl) package and
the python [`sympy`](http://www.sympy.org/en/index.html) module.
When you load `Symata` with `using Symata`, `sympy` is automatically via `PyCall`, which uses [`Conda`](https://github.com/JuliaPy/Conda.jl),
which in turn installs `python` and needed modules in your `Julia` directory.
However, to do this, `PyCall` must be configured to not use you system version of `python`.
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

Symata is a registered module. It can be installed like this

```julia
julia> Pkg.update()
julia> Pkg.add("Symata")
julia> using Symata
symata> Help()    # type '=' alone on a line to enter symata mode
```

#### note

`SymPy`, or `sympy`, here refers to the python [SymPy](http://www.sympy.org/en/index.html) distribution
(sometimes called sympy), *not* the Julia package `SymPy`. `Symata` does not require the Julia package
[SymPy.jl](https://github.com/jverzani/SymPy.jl), which has a different goal.

Symata requires `mpmath` package for python. This
should be automatically installed when installing `sympy` via
`PyCall` as described above. This also works on OSX.
However, if you use `pip`, you should just be able to run `pip
install mpmath`.


### Running Symata

Three environments for running `Symata` are supported: the `Julia` REPL, `Jupyter`, and a dumb terminal

### Symata REPL mode

A `Symata` mode is added to the `Julia` REPL. Enter the mode by typing `=` as the first character. Exit
the mode by typing `backspace` as the first character.

```julia
julia> using Symata

symata 1>     # after entering `=`
```

There is also an executable [`symata`](symata) included in top level directory of this distribution. It is a (UNIX
sh) shell script that just starts julia and loads the module.

```sh
#
julia -i -e "using Symata" $*
```

Upon running this script, `Symata` mode is entered automatically.
Toggle between Julia and Symata modes by typing `=` as the first character on a line.
(If loading Symata from the `julia` prompt via `using Symata`, you use `=` and backspace.)

In Symata mode, the input is not interpreted as Julia expressions, but rather Symata expressions.
You can do tab completion to see a list of functions and symbols.

### Jupyter / IJulia

<!-- You need to use the development version of `Symata` in order to use `Jupyter`, which is provided by
`IJulia.jl`. Switch to the development version with `Pkg.checkout("Symata")`. (Later, you can return to the latest versioned
branch with `Pkg.free("Symata")`.)
-->

Versions v1.3.0 through v1.3.2 of `IJulia.jl` are supported.

<!-- Once the development branch of `Symata` is selected, start a `Jupyter` notebook session. Then do the following.
-->

```julia
In [1]:  using Symata

In [2]:  isymata()  # enter Symata mode

In [3]:  Expand((a+b)^2)

Out[3]:  a^2 + 2a*b + b^2

In [4]:  Julia()   # return to Julia mode
```

In `Jupyter`, the `Symata` expressions `In(n)` and `Out(n)` reevaluate the input and output cells. TAB completion
works in `Jupyter`. To see a list of all possible completions, type `*[TAB]`.

### Dumb terminal

If you do `using Symata` in a dumb terminal, the `Symata` prompt should appear automatically.

### Help, examples, tests

#### Plotting

<!-- The development branch of Symata supports some plotting via `Plot.jl`.  How to switch branches
is described above.
-->

Some plotting is supported.

```
Plot( :(sin) , Table(Pi * RandomReal(), [100]), color => red)

Plot( [:(sin), :(cos)] , Table(Pi * RandomReal(), [100]), color => red, xlabel => "x")

Plot using a `Julia` function.

Other examples.

Plot([1,2,3])

Plot([1,2,3], [3,1,2])
```

Try `? Plot` or `Help(Plot)`.

#### Finding Help and Examples

The best source of examples is [the test directory](sjtest/).
The documentation can be printed from within Symata by entering `? SymName`
at the `symata` prompt.  `Help(Symname)` prints the same
documentation. For many Symata functions, the SymPy docstring is
printed along with the Symata documentation.

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

Run the test suite from the `symata` prompt with `Tests()`.
This runs tests in [the directory sjtest.](sjtest/)
`Pkg.test("Symata")` runs the same test suite from `Julia`.

#### A few examples

Here are some examples of Symata.

```julia
symata> ClearAll(fib)
symata> fib(1) := 1
symata> fib(2) := 1
symata> fib(n_) := fib(n-1) + fib(n-2)
symata> fib(10)
55
symata> addone(x_) := (a = 1,  x + a)  # compound expression
symata> addone(y)
1 + y
symata> g(x_) := Module([a,b],(a=1,b=3,a+b+x))  # lexically scoped local vars
symata> gt5(x_) := x > 5     # conditions on patterns
symata> g(x_Float:?(gt5)) = 1   # only matches floating point numbers > 5
symata> h(x_^2) := x    # Structural matching
symata> h((a+b)^2)
a + b
```

#### SymPy

Functions that call SymPy work as follows (many more examples are in the test directory). This SymPy call
```python
integrate(exp(-t) * t**(a-1), (t,0,oo), conds='none')
```
corresponds to this Symata expression
```julia
Integrate( Exp(-t)*t^(a-1),[t,0,Infinity], conds => "none")
```

#### Pattern matching.

Patterns are used in several places. For example you can make a replacement rule like this

```julia
symata> cossinrule = Cos(x_)^2 + Sin(x_)^2 => 1;

symata> Replace( Cos(a+b)^2 + Sin(a+c)^2, cossinrule)
(a+b)^2 + Sin(a+c)^2

symata> Replace( Cos(a+b)^2 + Sin(a+b)^2, cossinrule)
1
```

But, in practice, you would use `TrigSimp` or `Simplify`.

`Functions` are really replacement rules.  A replacement rule is associated with the symbol `f` like this
```julia
symata> f(x_) := x^2

symata> f(a+b)
(a+b)^2
```

When `f` is encountered as the head of an expression, a list of such rules is
tried. The first that succeeds makes the transformation and that round of evaluation
is done.

#### Miscellaneous Tips

To recall previous lines of input, use `O`, `OO`, etc. Or use `Out(n)` for the `n`th output.
Type `? HistoryLength` to see how to control saving output.

`BigIntInput(True)` enables making all input integers arbitrary precision.

You can use `Save` and `Get` to write and read Symata code.

##### Rules and Assignment

You can use the symbols `:>` for `RuleDelayed`, `=>` for `Rule`, `^:=` for `UpSetDelayed`,
`=` for `Set`, `:=` for `SetDelayed`, and `^=` for `UpSet`.


#### Upvalues

"Upvalues" are (partially) implemented:
```julia
symata> g(f(x_)) ^= x^2
symata > g(f(z))
 z ^ 2
```

#### Results on evaluation and efficiency

*Note* Some of the times given below are slower with more recent
versions of Symata. This is because the pattern matching code has been
rewritten to include more features, but has not been optimized. In
particular, `PatternTest` is 5 or 6 times slower because it is no
longer "compiled", but is reevaluated every time the pattern is
used. It probably won't be compiled again until the pattern matching
code is nearly completed.

*Note* As with everything in Julia, the following are not the times you get the first time you run
these commands, as Julia uses JIT compiling.

Here is counting with patterns. The execution time is about the same as Mma 3.

```
symata> b = Range(10^5);   # [1,2,...,100000]
elapsed time: 0.001218053 seconds (2395840 bytes allocated)

symata> Count(b, 2)     # Count the number of 2's
elapsed time: 0.008307134 seconds (1608 bytes allocated)
1

symata> Count(b, _Integer)
elapsed time: 0.022361488 seconds (1594520 bytes allocated)
100000

symata> Count(b,_String)
elapsed time: 0.015774562 seconds (1594488 bytes allocated)
0

symata> Count(b, _:?(EvenQ))
elapsed time: 0.075666716 seconds (3984008 bytes allocated)
50000

symata> Count(b, _:?( :( x -> x > 5 ) ) )    # Use a Julia function as the test
elapsed time: 0.076713741 seconds (4780808 bytes allocated)
99995

symata> countprimes = Count(_:?(PrimeQ))   # Currying, like in Mma 10

symata> countprimes(b)
elapsed time: 0.167145648 seconds (16778920 bytes allocated)
9592

```

#### Fixed-point evaluation

Symata does evaluation to a fixed point, always effectively re-evaluating in the
current environment.

Here is Symata doing expansion with the native `ExpandA`. `ExpandA` is
only for demonstration; The much more capable `Expand` is a frontend
to a SymPy function. However the results following the first line
below, work as well if you call `Expand`.  We need to quickly evaluate
an expression and track whether it needs to be re-evaluated:

```julia
symata> m = ExpandA((a+b)^BI(1000));   # expand with BigInt exponent
elapsed time: 0.008785756 seconds

symata> m[2]             # get a single value without re-evaluating
elapsed time: 3.7545e-5 seconds (992 bytes allocated)
1000*(a^999)*b

symata> a = 1;

symata> m[2]           # slower because we re-evaluate everything
elapsed time: 0.075710831
1000*b

symata> ClearAll(a)

symata> m[2]     # re-evaluate, it is still relatively slow.
elapsed time: 0.040376351 seconds (3723352 bytes allocated)
1000*(a^999)*b

symata> m[2]    # no evaluation, expression has been marked as fixed again.
elapsed time: 3.7984e-5 seconds (992 bytes allocated)
tryrule count: downvalue 0, upvalue 0
1000*(a^999)*b

symata> m[3]   # we can iterate over m without re-evaluating
elapsed time: 8.2968e-5 seconds (1544 bytes allocated)
499500*(a^998)*(b^2)
```

Quickly generate a large list of numbers and access a value.
```julia
symata> a = Range(10^5);
elapsed time: 0.001219758 seconds (2394728 bytes allocated)

symata> a[-1]
elapsed time: 3.5242e-5 seconds (976 bytes allocated)
100000

symata> a[-1] = d
elapsed time: 3.7307e-5 seconds (816 bytes allocated)
d

symata> Apply(Plus,a)
elapsed time: 0.005774109 seconds
4999950000 + d
```

#### Using Symata language features directly from Julia

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


<!--  LocalWords:  Mathematica SymPy julia symata PyCall Mma src REPL
 -->
<!--  LocalWords:  EvenQ countprimes PrimeQ HoldXXX Maxima eval regex
 -->
<!--  LocalWords:  Mathics Symata's backend ExpandA BigInt ClearAll
 -->
<!--  LocalWords:  tryrule downvalue upvalue BuiltIns BuiltIn SymName
 -->
<!--  LocalWords:  Symname addone lexically FloatingPoint cossinrule
 -->
<!--  LocalWords:  TrigSimp Upvalues Symata sjtest docstring builtin
 -->
<!--  LocalWords:  oo conds th HistoryLength BigIntInput RuleDelayed
 -->
<!--  LocalWords:  UpSetDelayed SetDelayed UpSet frontend FresnelC
 -->

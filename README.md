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
achieved by wrapping SymPy. There are more than 500 functions
implemented, including integration, transformation of special
functions, expression manipulation, writing and reading expressions to
and from a file etc. These are the best places for examples and help:

-  [static snapshots of Symata tutorial notebooks](http://nbviewer.jupyter.org/github/jlapeyre/Symata.jl/tree/master/examples/).
   (If you can't get to the nbviewer link, here is [the example directory](examples/) at github.
-  Symata functions written in Symata are [here](symsrc/autoloaded.sj).
-  examples are in [the test directory](sjtest/)  (note this is `sjtest`, not `test`)
-  when running Symata
 * `TAB` completion
 * `? Topic` (with completion)
 * `h"word"` regular expression search

### IJulia Notebooks

A few tutorial notebooks are in [the example directory](examples/). These are snapshots, not live, so you can
view them immediately with your browswer. The typeset math will look better
in the IJulia notebook, which renders it with higher quality than github does.

### Installing

Symata is a registered module. It can be installed like this

```julia
julia> Pkg.update()
julia> Pkg.add("Symata")
julia> using Symata
symata> Help()    # type '=' alone on a line to enter symata mode
```

`Symata` can be installed on Linux, OSX, and Windows, and Julia v0.4, v0.5, and v0.6. (NB: `Symata` is
broken on `v0.4`. It won't take much time to fix, but its not a priority. If you want to use `Symata` with v0.4,
please open an issue. Or better yet, make a PR!)

`Symata` depends on the [`PyCall`](https://github.com/stevengj/PyCall.jl) package and
the python [`sympy`](http://www.sympy.org/en/index.html) module.
When you load `Symata` with `using Symata`, `sympy` is installed automatically via `PyCall`, which uses [`Conda`](https://github.com/JuliaPy/Conda.jl).
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


```julia
In [1]:  using Symata

In [2]:  Expand((a+b)^2)

Out[2]:  a^2 + 2a*b + b^2

In [3]:  Julia()   # return to Julia mode
```

In `Jupyter`, the `Symata` expressions `In(n)` and `Out(n)` reevaluate the input and output cells. TAB completion
works in `Jupyter`. To see a list of all possible completions, type `*[TAB]`.

### Dumb terminal

If you do `using Symata` in a dumb terminal, the `Symata` prompt should appear automatically.

### Help, examples, tests


The best source of examples is [the test directory](sjtest/).
The documentation can be printed from within Symata by entering `? SymName`
at the `symata` prompt.  `Help(Symname)` prints the same
documentation. For many Symata functions, the SymPy docstring is
printed along with the Symata documentation.

Try `Help()`. Type `h"topic"` to search for items containing the
string `"topic"`.  Hit `TAB` at the command line REPL for a list of all
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

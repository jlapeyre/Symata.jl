# Symata.jl

*Symbolic mathematics language*

Linux, OSX: [![Build Status](https://travis-ci.org/jlapeyre/Symata.jl.svg)](https://travis-ci.org/jlapeyre/Symata.jl)
&nbsp;
Windows: [![Build Status](https://ci.appveyor.com/api/projects/status/github/jlapeyre/Symata.jl?branch=master&svg=true)](https://ci.appveyor.com/project/jlapeyre/symata-jl)
&nbsp; &nbsp; &nbsp;
[![Coverage Status](https://coveralls.io/repos/github/jlapeyre/Symata.jl/badge.svg?branch=master)](https://coveralls.io/github/jlapeyre/Symata.jl?branch=master)
[![codecov.io](http://codecov.io/github/jlapeyre/Symata.jl/coverage.svg?branch=master)](http://codecov.io/github/jlapeyre/Symata.jl?branch=master)

[![Symata](http://pkg.julialang.org/badges/Symata_0.6.svg)](http://pkg.julialang.org/?pkg=Symata&ver=0.6)
[![Symata](http://pkg.julialang.org/badges/Symata_1.0.svg)](http://pkg.julialang.org/?pkg=Symata&ver=1.0)

### Examples and help

-  [Static snapshots of Symata tutorial notebooks](http://nbviewer.jupyter.org/github/jlapeyre/Symata.jl/tree/master/TutorialNotebooks/)
   at `nbviewer.jupyter.org`. These are the same notebooks found in the [TutorialNotebooks](TutorialNotebooks/) directory
   in this repositoy. But the rendering at `nbviewer` is better.
-  [Symata-language test directory](symata_test/) (note this is `symata_test`, not `test`)
-  [Symata functions written in Symata](symsrc/autoloaded.sj).
-  When running Symata
   * `TAB` completion
   * `? Topic` (with completion)
   * `h"word"` regular expression search
   * `Help()` and `Help(topic)`
-  [NumericalMethodsforEngineers.jl](https://github.com/PtFEM/NumericalMethodsforEngineers.jl) uses
   Symata. Example code is found in
   * [examples/ch04/](https://github.com/PtFEM/NumericalMethodsforEngineers.jl/tree/master/examples/ch04)
   * [Ex.7.13.function.jl](https://github.com/PtFEM/NumericalMethodsforEngineers.jl/blob/master/examples/ch07/WRM/Ex.7.13.function.jl)
   * Several files in the [test directory](https://github.com/PtFEM/NumericalMethodsforEngineers.jl/tree/master/test)   
-  If you have a question or a request, or want to contribute,
   please [open an issue](https://github.com/jlapeyre/Symata.jl/issues) here on github.

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
achieved by wrapping SymPy. There are more than 600 functions
implemented, including integration, transformation of special
functions, expression manipulation, writing and reading expressions to
and from a file etc.

<!--  ### Mathematica syntax.

You can use Symata with Mathematica syntax in addition to the usual Julia-like syntax. To use Mathematica syntax,
install the [SymataSyntax.jl package](https://github.com/jlapeyre/SymataSyntax.jl).
-->

### Installing

Symata is a registered module. It can be installed like this

```julia
(v0.7) pkg> add Symata
julia> using Symata
symata> Help()    # type '=' alone on a line to enter symata mode
```

`Symata` can be installed on Linux, OSX, and Windows.

`Symata` depends on the [`PyCall`](https://github.com/stevengj/PyCall.jl) package and
the python [SymPy](http://www.sympy.org/en/index.html) module. You can install SymPy
via `pip install sympy`. Symata is compatible with SymPy v1.0 and v1.2 (and probably v1.1).

Alternatively, you may install SymPy via `Conda.jl`.
When you load `Symata` with `using Symata`, `sympy` is installed automatically via `PyCall`, which uses [`Conda`](https://github.com/JuliaPy/Conda.jl). However, to do this, `PyCall` must be configured to not use you system version of `python`.
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

Three environments for running `Symata` are supported: the `Julia` REPL, `Jupyter`, and a dumb terminal.

### Symata REPL mode

A `Symata` mode is added to the `Julia` REPL. Enter the mode by typing `=` as the first character. Exit
the mode by typing `backspace` as the first character.

```julia
julia> using Symata

symata 1>     # after entering `=`
```

Under some circumstances, e.g. when using `PackageCompiler`, the `Symata` repl is not initialized after the module is loaded.
You can initialize it with the exported Julia command `run_repl`. After this, the repl is entered with the `=` key.
An executable [`symata`](symata) is included in top level directory of this distribution. It is a (UNIX
sh) shell script that just starts julia, loads the module, and enters `Symata` mode.
Switch between `Julia` and `Symata` modes by typing `=`, or backspace, as the first character on a line.
You can do tab completion to see a list of functions and symbols.

### Jupyter / IJulia

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

### sympy shell

From the julia prompt, type `isympy()` to enter the sympy shell.

### Tests

Run the test suite from the `symata` prompt with `Tests()`.
This runs tests in the [symata_test directory](symata_test/)
`Pkg.test("Symata")` runs the same test suite from `Julia` and
some Julia-level unit tests, as well.

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
<!--  LocalWords:  TrigSimp Upvalues Symata symata_test docstring builtin
 -->
<!--  LocalWords:  oo conds th HistoryLength BigIntInput RuleDelayed
 -->
<!--  LocalWords:  UpSetDelayed SetDelayed UpSet frontend FresnelC jl
 -->
<!--  LocalWords:  OSX nbsp codecov io jv PatternTest nbviewer github
 -->
<!--  LocalWords:  symsrc SymataSyntax IJulia sympy Conda ENV linux
 -->
<!--  LocalWords:  mpmath Jupyter isympy
 -->

## Symbolic manipulation language

SJulia is a language for symbolic computation.  It is largely
modeled on the pattern matching and evaluation sequence of
Mathematica. Evaluation, pattern matching, flow control, etc. are
written in Julia. Much of the mathematics and symbolic manipulation is
achieved by wrapping SymPy. There are more than 300 functions
implemented, including integration, tranformation of special
functions, expression manipulation, writing and reading expressions to
and from a file etc. The best places for examples of what works (and
what does not), are [the test directory](test/), and, at the SJulia
prompt, the online help, `? Topic`, with TAB completion, and regex search `h"word"`.

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

You can test it with `Pkg.test("SJulia")`.

#### SJulia REPL mode

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
In SJulia mode, the input is not interpreted as Julia code, but rather SJulia code.
You can do tab completion to see a list of functions and symbols.

#### SymPy

Functions that call SymPy work like this. This SymPy call
```python
integrate(exp(-t) * t**(a-1), (t,0,oo), conds='none')
```
corresponds to this SJulia expression
```julia
Integrate( Exp(-t)*t^(a-1),[t,0,Infinity], conds => "none")
```

For many SJulia functions, the SymPy docstring is printed along with the SJulia documentation.

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

Here is SJulia doing expansion (with the native `ExpandA`. The more capable `Expand` is a frontend to a
SymPy function)
We need to quickly evaluate an expression and track whether it needs to be re-evaluated:
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

#### Alternate way to evaluate SJulia expressions

Instead of working with the SJulia REPL (by typing `=`)
You can get the same thing by typing

```julia
julia> using SJulia
julia> @ex some SJulia expression
julia> @ex(some expression that may look like two expressions)
```

### Finding Help and Examples

Try `Help()`. Type `h"topic"` to search for items containing the
string `"topic"`.  Hit TAB at the command line REPL for a list of all
builtin symbols (i.e. variables and functions) Symbols that are
associated with some functionality can be listed with
`BuiltIns()`. Type `Example()` to see a list of topics with examples.
Type `Example(topic)` to run the examples. The input strings from the
examples are pushed to the history so that they can be recalled and
edited and re-evaluated.

This documentation can be printed from within SJulia by entering `? SymName`
at the `sjulia` prompt.  `Help(Symname)` prints the same
documentation. This allows you to type `@ex Help(SymName)` from Julia.

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
storage is separate from Julia. But, some features allow
using SJulia directly from Julia, and vice versa. For instance, these commands

```julia
julia> using SJulia
julia> ex = :x + 1
1 + x
julia> m = Expand((:a+:b)*(:x+1)^2)
a + b + 2*a*x + 2*b*x + a*(x^2) + b*(x^2)
````

make Julia bindings of SJulia expressions to the symbols ex and m.
(Only Expand works this way, but other functions can be added easily.)

#### More on Evaluation

There are lines at the top of the file `src/mxpr.jl` to control evaluation. You can
choose infinite or single evaluation. The test suite relies on infinite evaluation being
enabled. Hashing and caching of expressions can also be chosen here, but it is not very
useful at the moment.

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

#### Upvalues

"Upvalues" are (partially) implemented:
```julia
sjulia> g(f(x_)) ^= x^2
sjulia > g(f(z))
 z ^ 2
```

<!--  LocalWords:  Mathematica SymPy julia sjulia PyCall Mma src
 -->
<!--  LocalWords:  EvenQ countprimes PrimeQ HoldXXX Maxima eval
 -->
<!--  LocalWords:  Mathics SJulia's backend ExpandA BigInt ClearAll
 -->
<!--  LocalWords:  tryrule downvalue upvalue BuiltIns BuiltIn SymName
 -->
<!--  LocalWords:  Symname addone lexically FloatingPoint cossinrule
 -->
<!--  LocalWords:  TrigSimp Upvalues
 -->

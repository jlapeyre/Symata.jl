## Symbolic manipulation code

SJulia is a partial implementation of a language for symbolic
computation.  It is largely modeled on the pattern matching and
evaluation sequence of Mathematica.

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

#### What is implemented

The focus at present is not on implementing specific mathematical
computation. However, much of this is supplied (`Integrate`, `D`,
`Limit`, `Together`, `Apart`, `Factor`) mostly by using
[SymPy](http://www.sympy.org/en/index.html) as a backend. The
emphasis is rather on testing implementations of core features and
subsystems that are efficient or can be made efficient. These include
core data structures, pattern matching, and the evaluation sequence.

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
There are [many more listed below](#documented-functions-and-symbols), mostly to support experimenting with
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

### Documented Functions and Symbols

The following is generated from SJulia's online help.

##### Abs

Abs(z) represents the absolute value of z.



##### AddTo

AddTo(a,b), or a += b, sets a to a + b and returns the new value. This is currently
faster than a = a + b for numbers.



##### All

All is a symbol used in options.



##### Allocated

Allocated(expr) evaluates expr and returns a list of the memory allocated
and the result of the evaluation.

See also TimeOff, TimeOn, Timing, TrDownOff, TrDownOn, TrUpOff, and TrUpOn.


##### Apart

Together(product) rewrites a product as a sum of terms with minimal denominators.



##### Apply

Apply(f,expr) replaces the Head of expr with f. This also works for some
Julia objects. Eg. Apply(Plus, :( [1:10] )) returns 55. Apply can be used
in operator form. For example m = Apply(Plus),  m(f(a,b,c)).



##### AtomQ

AtomQ(expr), in principle, returns true if expr has no parts accessible with Part.
However, currently, Julia Arrays can be accessed with Part, and return true under AtomQ.

See also EvenQ and OddQ.


##### Attributes

Attributes(s) returns attributes associated with symbol s. Builtin symbols have
the attribute Protected, and may have others.



##### BF

BF(n) converts the number, or string n to a BigFloat. SJulia currently neither
detects overflow, nor automatically promotes types from fixed to arbitrary precision.

See also BI and Big.


##### BI

BI(n) converts the number n to a BigInt. SJulia currently neither
detects integer overflow, nor automatically promote integers to BigInts.

See also BF and Big.


##### Big

Convert a number to a maximum precision representation (typically
'BigInt' or 'BigFloat')

See also BF and BI.


##### BuiltIns

BuiltIns() returns a List of all "builtin" symbols. These are in fact all symbols that
have the Protected attribute.



##### ByteCount

ByteCount(expr) gives number of bytes in expr.



##### Clear

Clear(x,y,z) removes the values associated with x,y,z. It does not remove
their DownValues.

See also ClearAll.


##### ClearAll

ClearAll(x,y,z) removes all values and DownValues associated with x,y,z. The
symbols are removed from the symbol table and will not appear in the list returned
by UserSyms().

See also Clear.


##### Comparison

Comparison(expr1,c1,expr2,c2,expr3,...) performs or represents a
chain of comparisons. Comparison expressions are usually input and
displayed using infix notation.


Examples

```
sjulia> Clear(a,b,c)

sjulia> a == a
true

sjulia> a == b
false

sjulia> a < b <= c
a < b <= c

sjulia> (a=1,b=2,c=2)
2

sjulia> a < b <= c
true
```


##### Complex

Complex(a,b) returns a complex number when a and b are Reals. This is done when the
expression is parsed, so it is much faster than 'a + I*b'.



##### CompoundExpression

CompoundExpression(expr1,expr2,...) or (expr1,expr2,...) evaluates each expression in turn and
returns the result of only the final evaluation.



##### ConstantArray

ConstantArray(expr,n) creates a list of n copies of expr.



##### Count

Count(expr,pattern) returns the number of arguments in expr than match pattern.
Only matching on one level is supported. This is for testing the performance
of pattern matching. Count(pattern) can be used as the head of an expression,
as an operator. For instance cop = Count(_^2) defines a function that counts
the number of arguments that have the form of a square. Count also works when
expr is a Julia Dict.


Examples

```
sjulia> Count(Range(10), _Integer)
10

sjulia> Count(_Integer)(Range(10))
10

sjulia> Count(Range(10), 2)
1
```


##### D

D(expr, x) gives the partial derivative of expr with respect to x.
D(expr,[x,n]) gives the nth partial derivative.
D(expr,[x,n1],y,[z,n2]) gives the mixed derivative.



##### Depth

Depth(expr) gives the maximum number of indices required to specify
any part of expr, plus 1.



##### DirtyQ

DirtyQ(m) returns true if the timestamp of any symbol that m depends on
is more recent than the timestamp of m. This is for diagnostics.

See also Age, Fixed, HAge, Syms, and Unfix.


##### Do

Do(expr,[imax]) evaluates expr imax times.
Do(expr,[i,imax]) evaluates expr imax times with i localized taking values from 1 through
  imax in increments of 1.
Do(expr,[i,imin,imax]) evaluates expr with i taking values from imin to imax with increment 1.
  imin and imax may be symbolic.
Do(expr,[i,imin,imax,di]) evaluates expr with i taking values from imin to imax with increment di.
  imin, imax, and di may be symbolic.
Do(expr,[i,[i1,i2,...]) evaluates expr with i taking values from a list.



##### DownValues

DownValues(s) returns a List of DownValues associated with symbol s. These are values
that are typically set with the declarative "function definition".

See also Set, SetDelayed, UpSet, and UpValues.

Examples

```
sjulia> ClearAll(f)

sjulia> f(x_) := x^2

sjulia> DownValues(f)
[HoldPattern(f(x_))->(x^2)]
```


##### Dump

Dump(expr) prints an internal representation of expr. This is similar to
Julia `dump'.

See also DumpHold.


##### DumpHold

DumpHold(expr) prints an internal representation of expr. This is similar to
Julia `dump'. In contrast to `Dump', expr is not evaluated before it's internal
representation is printed.

See also Dump.


##### E

E is the base of the natural logarithm



##### EvenQ

EvenQ(expr) returns true if expr is an even integer.

See also AtomQ and OddQ.


##### Example

Example(s) runs (evaluates) the first example for the symbol s, which is typically
a BuiltIn symbol. The input, output, and comments are displayed. The input strings
for the example are pushed onto the terminal history so they can be retrieved and 
edited and re-evaluated. Example(s,n) runs the nth example for symbol s. When viewing
documentation strings via ? SomeHead, the examples are printed along with the
documentation string, but are not evaluated.



##### Expand

Expand(expr) expands products in expr. This is only partially implemented,
mostly to test the efficiency of evaluation and evaluation control.



##### Factor

Factor(expr) factors expr. This function calls SymPy.



##### FactorInteger

FactorInteger(n) gives a list of prime factors of n and their multiplicities.



##### Fixed

Fixed(expr) returns the status of the fixed point bit, which tells whether expr
is expected to evaluate to itself in the current environment. This is partially
implemented.

See also Age, DirtyQ, HAge, Syms, and Unfix.


##### For

For(start,test,incr,body) is a for loop. Eg. For(i=1,i<=3, Increment[i] , Println(i))
Using Increment[i] is currently much faster than i = i + 1. There is no special syntax yet for
Increment.



##### FullForm

FullForm(expr) prints expr and all sub-expressions as
Head(arg1,arg2,...). Normal output may use infix notation instead.


Examples

```
sjulia> Clear(a,b)

sjulia> a+b
a+b

sjulia> FullForm(a+b)
Plus(a,b)
```


##### HAge

HAge(s) returns the timestamp for the expression or symbol s.
Using this timestamp to avoid unnecessary evaluation is a partially
implemented feature.

See also Age, DirtyQ, Fixed, Syms, and Unfix.


##### Head

Head(expr) returns the head of expr, which may be an SJulia expression or object of any
Julia type. The head of a Julia expression is Expr, eg.
Head( :( :( a = 1) )) returns Expr. Note we have to quote twice, because one level of
a quoted Julia expression is evaluated so that we can embed Julia code.



##### Help

Help(sym) or Help("sym") prints documentation for the symbol sym. Eg: Help(Expand).
Help() lists all documented symbols. Due to parsing restrictions at the repl, for some
topics, the input must be a string. The same help can be accessed with
h"topic", which is implemented as a Julia special string literal.
Help(regex) prints a list of topics whose documentation text matches the
regular expression regex.
Help(All -> true) prints all of the documentation.



##### I

I is the imaginary unit



##### If

If(test,tbranch,fbranch) evaluates test and if the result is true, evaluates tbranch, otherwise fbranch



##### Increment

Increment(n) increments the value of n by 1 and returns the old value.



##### IntegerDigits

IntegerDigits(n,[, base][, pad]) Returns an array of the digits of "n" in the given base,
optionally padded with zeros to a specified size. In contrast to Julia, more significant
digits are at lower indexes.



##### Integrate

Integrate(expr, x) gives the indefinite integral of expr with respect to x.
Integrate(expr, [x,a,b]) gives the definite integral.



##### JVar

JVar(x) returns the Julia value of the Symbol that x evaluates to. For example,
if a = 1 in Julia and b = a in SJulia, then JVar(b) evaluates to 1.

See also Jxpr.


##### Jxpr

Jxpr allows embedding Julia expressions.
A Jxpr is entered like this :( expr ) . expr is interpreted as a Julia expression and
it is wrapped expression with head Jxpr, which is then evaluated when
Jxpr is evaluated. You never see the head Jxpr. For example
 m = :( [1:10] )  creates a Julia array and binds it to the SJulia symbol m

See also JVar.

Examples

```
This creates a Julia Array{Int,1} and "binds" it to the SJulia symbol m.
sjulia> m = :( [1:3] )
3-element Array{Int64,1}:
 1
 2
 3
```

```
Call a Julia function
sjulia> tf = :( time )

sjulia> tf()
1.424287593897437e9
```


##### Keys

Keys(d) returns a list of the keys in Dict d



##### LeafCount

LeafCount(expr) gives the number of indivisible (Part can't be taken) elements in expr.
This amounts to counting all the Heads and all of the arguments that are not of type Mxpr.
A more accurate name is NodeCount.



##### Length

Length(expr) prints the length of SJulia expressions and Julia objects. For
SJulia expressions, the length is the number or arguments. For scalar Julia
types, the length is zero. For Array's and Dict's the length is the same as
Julia `length'.



##### Limit

Limit(expr, var => lim) gives the limit of expr as var approaches to lim.



##### Map

Map(f,expr) returns f applied to each element in a copy of expr.
f can be an SJulia object or a Julia function. Map can be used in
an operator form. For example Map(f)(expr).



##### MatchQ

MatchQ(expr,pattern) returns true if expr matches pattern. MatchQ can
be used in operator form. For example, myintq = MatchQ(_Integer).


Examples

```
sjulia> MatchQ( 1, _Integer)
true

sjulia> ClearAll(gg,xx,b)

sjulia> MatchQ( gg(xx) , _gg)
true

sjulia> MatchQ( b^2, _^2)
true
```


##### Module

Module creates a lexical scope block for variables. Warning, this is broken
in the sense that nested calls to a Module are not supported.


Examples

```
sjulia> ClearAll(f,a)

sjulia> f(x_) := Module([a],(a=1, a+x))

sjulia> f(3)
4

sjulia> a
a
```


##### N

N(expr) tries to give a the numerical value of expr.
N(expr,p) tries to give p decimal digits of precision.



##### Numerator

Numerator(expr) returns the numerator of expr.



##### OddQ

OddQ(expr) returns true if expr is an odd integer.

See also AtomQ and EvenQ.


##### Pack

Pack(mx) packs the args of the SJulia expression mx into a typed Julia array.
The type of the array is the same as the first element in mx.

See also Unpack.

Examples

```
This returns a Julia array of element type Int [1,2,3].
sjulia> ClearAll(f)

sjulia> Pack(f(1,2,3))
3-element Array{Int64,1}: [1,2,3]
```


##### Part

Part(expr,n) or expr[n], returns the nth element of expression expr.
Part(expr,n1,n2,...) or expr[n1,n2,...] returns a nested part.
The same can be achieved less efficiently with expr[n1][n2]...
expr[n] = val sets the nth part of expr to val. n and val are evaluated
normally. expr is evaluated once.
expr[n] also returns the nth element of instances of several
Julia types such as Array, or the element with key 'n' for Dict's.



##### Permutations

Permutations(expr) give a list of all permutations of elements in expr.



##### Pi

Pi is the trigonometric constant π.



##### Position

Position(expr,x) returns a list of part specifications of positions in
expr at which x occurs. Only literal values for x are supported, not
patterns



##### Precision

Precision(x) gets the precision of a floating point number x, as defined by the
effective number of bits in the mantissa.



##### Primes

Primes(n) returns a collection of the prime numbers <= "n"



##### Println

Println(expr1,expr2,...) prints the expressions and a newline.



##### Push!

Push!(a,val) pushes val onto the expression that symbol a evaluates to.
Push! is outside the Mma programming model, which requires immutable
expressions. Re-evaluation, and updating metadata is not implemented.
Re-evaluation can be forced with Unfix(a).


Examples

```
sjulia> ClearAll(a,b)

sjulia> a = []

sjulia> For(i=1, i < 1000, Increment(i), Push!(a,Symbol("b$i")))
```


##### Range

Range(n) returns the List of integers from 1 through n.
Range(n1,n2) returns the List of numbers from n1 through n2.
Range(n1,n2,di) returns the List of numbers from n1 through n2 in steps of di
di may be negative. Floats and some symbolic arguments are supported.
You can get also get SJulia lists like using Unpack(:([1.0:10^5])).
This uses embedded Julia to create a typed Array and then unpacks it to a List.



##### Rational

Rational(a,b), or a//b, returns a Rational for Integers a and b.  This is done when the
expression is parsed, so it is much faster than 'a/b'.



##### Rationalize

Rationalize(x) returns a Rational approximation of x.
Rationalize(x,tol) returns an approximation differing from x by no more than tol.



##### Regex

r"expr" creates a regular expression (PCRE). This is a Julia DataType.



##### Replace

Replace(expr,rule) replaces parts in expr according to Rule rule.

See also ReplaceAll.

Examples

```
sjulia> Clear(a,b,c)

# This expression does not match the pattern.
sjulia> Replace( Cos(a+b)^2 + Sin(a+c)^2, Cos(x_)^2 + Sin(x_)^2 => 1)
Cos(a + b)^2 + Sin(a + c)^2

# This expression does match the pattern.
sjulia> Replace( Cos(a+b)^2 + Sin(a+b)^2, Cos(x_)^2 + Sin(x_)^2 => 1)
1
```


##### ReplaceAll

ReplaceAll(expr,rule) replaces parts at all levels in expr according to Rule rule.
ReplaceAll(expr,List(rule1,rule2,...)) replaces parts at all levels in expr according to the
list or rules. If given explicitly, the rules must be given as List(...) rather than
[...] because of a parsing error.

See also Replace.

Examples

```
sjulia> ClearAll(zz,b,c)

sjulia> zz = 10 * b^2 * (c+d)
zz = 10 * b^2 * (c+d)

sjulia> ReplaceAll(zz, List(c => 3,d => 2) )
50*b^2
```


##### Reverse

Reverse(expr) reverses the order of the arguments in expr.    



##### Set

Set(a,b), a = b
Sets the value of a to b. b is evaluated only once, when `a=b' is evaluated.
obj[i,j,...] = val sets a part of obj to val. obj can be an SJulia expression
or a Julia object, such as an Array or Dict.

See also DownValues, SetDelayed, UpSet, and UpValues.

Examples

```
sjulia> Clear(a,b,c)

sjulia> b = a
a

sjulia> a = 1
1

sjulia> c = a
1

sjulia> a = 2
2

sjulia> b
2

sjulia> c
1
```


##### SetDelayed

SetDelayed(a,b), a := b
Whenever a is evaluated, b is evaluated and the result is assigned to a.
So a is not set to the value of b at the time a := b is evaluated, but
rather to the current value of b every time a is evaluated.

See also DownValues, Set, UpSet, and UpValues.


##### SetJ

SetJ(x,val) sets the Julia symbol x to val. Variables and functions in SJulia
are separate from those in Julia, ie, their table of bindings to symbols are separate.



##### Span

Span(a,b) or a:b represents elements a through b.
Span(a,b,c) or a:b:c represents elements a through b in steps of c.
expr(a:b) returns elements a through b of expr, with the same head as expr.



##### StringInterpolation

 " string1  $a string2 " interpolates the value of 'a' into the string.



##### StringLength

StringLength(s) returns the length of the string s.



##### Symbol

Symbol(str) converts the string str to a symbol. For example if a is 1,
then Symbol("a") returns 1.



##### Syms

Syms(m) returns a List of the symbols that the expression m 'depends' on. The
list is wrapped in HoldForm in order to prevent evaluation of the symbols.

See also Age, DirtyQ, Fixed, HAge, and Unfix.


##### Table

Table(expr,[imax]) returns a list of imax copies of expr.
Table(expr,[i,imax]) returns a list of expr evaluated imax times with
i set successively to 1 through imax.

Unusual examples:
This calls an anonymous Julia function. It is currently very slow
Table( (:((x)->(x^2))(i) ),[i,10])
This is much faster
f = :( g(x) = x^2 )
Table( f(i), [i,10])



##### TimeOff

TimeOff() disables printing CPU time consumed and memory allocated
after each evaluation of command line input.

See also Allocated, TimeOn, Timing, TrDownOff, TrDownOn, TrUpOff, and TrUpOn.


##### TimeOn

TimeOn() enables printing CPU time consumed and memory allocated
after each evaluation of command line input.

See also Allocated, TimeOff, Timing, TrDownOff, TrDownOn, TrUpOff, and TrUpOn.


##### TimesBy

TimesBy(a,b), or a *= b, sets a to a * b and returns the new value. This is currently
faster than a = a * b for numbers.



##### Timing

Timing(expr) evaluates expr and returns a list of the elapsed CPU time
and the result.

See also Allocated, TimeOff, TimeOn, TrDownOff, TrDownOn, TrUpOff, and TrUpOn.


##### ToExpression

ToExpression(str) converts string str to an expression.



##### ToString

ToStringLength(expr) returns the string of the printed form or expr.



##### Together

Together(sum) rewrites a sum of terms as a product.



##### TrDownOff

TrDownOff() disables tracing attempted applications of DownRules.

See also Allocated, TimeOff, TimeOn, Timing, TrDownOn, TrUpOff, and TrUpOn.


##### TrDownOn

TrDownOn() enables tracing attempted applications of DownRules.

See also Allocated, TimeOff, TimeOn, Timing, TrDownOff, TrUpOff, and TrUpOn.


##### TrUpOff

TrUpOff() disables tracing attempted applications of UpRules.

See also Allocated, TimeOff, TimeOn, Timing, TrDownOff, TrDownOn, and TrUpOn.


##### TrUpOn

TrUpOn() enables tracing attempted applications of UpRules.

See also Allocated, TimeOff, TimeOn, Timing, TrDownOff, TrDownOn, and TrUpOff.


##### TraceOff

TraceOff() turns off the tracing of SJulia evaluation.

See also TraceOn.


##### TraceOn

TraceOn() turns on the tracing of SJulia evaluation.

See also TraceOff.


##### Unfix

Unfix(expr) unsets the fixed flag on expr, causing it to be evaluated.
This is a workaround for bugs that cause an expression to be marked fixed
before it is completely evaluated.

See also Age, DirtyQ, Fixed, HAge, and Syms.


##### Unpack

Unpack(a) unpacks a Julia typed array into an SJulia List expression.
Only 1-d is supported. If a is a Julia Dict, then a list of lists of
key,value pairs is returned.

See also Pack.

Examples

```
This creates a List of three random Float64's.
sjulia> Unpack( :(rand(3)) )
[0.5548766917324894,0.034964001133465095,0.9122052258982192]
```


##### Unprotect

Unprotect(z1,z2,...) removes the Protected attribute from the symbols z1, z2, ...



##### UpSet

UpSet(a(g(x_)),b), or a(g(x_)) ^= b  associates the transformation rule with g.

See also DownValues, Set, SetDelayed, and UpValues.


##### UpValues

UpValues(s) returns a List of UpValues associated with symbol s. These are values
that are typically set with UpSet.

See also DownValues, Set, SetDelayed, and UpSet.


##### UserSyms

UserSyms() returns a List of non-Protected symbols, which is approximately
all user defined symbols.



##### Values

Values(d) returns a list of the values in Dict d



##### While

While(test,body) evaluates test then body in a loop until test does not return true.



##### macros

If SJulia encounters a macro call in input, it first Julia-evaluates all the
arguments then Julia-evaluates the macro and inserts it into the SJulia expression
tree. For instance, big numbers and regular expressions are constructed this way.


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

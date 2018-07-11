# Many of these are redundant. They should be replaced in the rest of the code by `isa`

is_SSJSym(x) = isa(x,SSJSym)
is_SJSym(x) = isa(x,SJSym)

is_Mxpr(x) = isa(x,Mxpr)

## I think we should prefer isa(Mxpr{:Sym}) here
is_Mxpr(mx::Mxpr{T},s::Symbol) where {T} = T == s
is_Mxpr(x,s::Symbol) = false

## Ruleq or ruleq ? need lc somewhere to signify it is lower level
ruleq(x::Mxpr{:Rule}) = true
ruleq(x) = false

# we could generate some of these with a loop and eval
Base.isempty(mx::Mxpr) = isempty(margs(mx))

"""
    mxpr_head_freeq(mx::Mxpr)

return true if any element at level 1 of mx is an Mxpr with head `head`.
"""
mxpr_head_freeq(mx::Mxpr, head) = ! any(t -> isa(t,Mxpr{head}), margs(mx))

## probably should not use these. isa signals that we are interested in Julia type rather
## than a paraphyletic class of numbers.
is_Number(x) = isa(x,Number)
is_Real(x) = isa(x,Real)
is_Complex(x) = isa(x,Complex)
is_Float(x) = isa(x,AbstractFloat)

## TODO julia has some function like this. We should use, if possible:
## isinteger
## isapprox
## isassigned, for arrays
## isequal
## isempty (instead of length == 0)
is_imaginary_integer(z::Complex{T}) where {T<:Integer} = real(z) == 0
is_imaginary_integer(x) = false

atomq(x) = ! isa(x,Mxpr)

is_Indeterminate(x::Symbol) = x == Indeterminate
is_Indeterminate(x) = false

is_Infintity(x) = false
is_Infintity(mx::Mxpr{:DirectedInfinity}) = ! isempty(mx) && mx[1] == 1 ? true : false

is_ComplexInfinity(x) = false
is_ComplexInfinity(mx::Mxpr{:DirectedInfinity}) =  isempty(mx) ? true : false

# use hasConstant
# is_Constant(x::Symbol) = get_attribute(x, :Constant)
# is_Constant(x) = false

# use hasProtected instead
#is_protected(sj::SJSym) = get_attribute(sj,:Protected)

is_blankxxx(x) = isa(x,BlankXXX)

## question: shold we do isxxx, is_xxx, or xxxq ?
## Julia does isxxx (or is_xxx in a few cases). Mma uses XxxQ uniformly (almost)
## Answer:
## Base.isinteger(1.0) -> true
## So we should do xxxq consistently because we want a clear distinction from Julia
## Julia's isinteger makes perfect sense within Julia. But, the name would collide
## isinteger for us.

## These are equivalent to a single isa, but they can be used directly as predicate functions to
## supply as arguments
listq(x::Mxpr{:List}) = true
listq(x) = false
integerq(x::Integer) = true
integerq(x) = false

listofpredq(x::Mxpr{:List},pred) = isempty(x) ? false : all(pred,x)
listofpredq(x,pred) = false
listoflistsq(x::Mxpr{:List}) = listofpredq(x,listq)

function listoflistsofsamelengthq(x::Mxpr{:List})
    listoflistsq(x) || return false
    n = symlength(x[1])
    all( a -> symlength(a) == n, x)
end
listoflistsofsamelengthq(x) = false

listofintegersq(x::Mxpr{:List}) = listofpredq(x,integerq)  ## maybe we should not define this

listoflistsofpredq(x::Mxpr{:List},pred) = all(a -> listofpredq(a,pred), x)
listoflistsofpredq(x) = false # maybe not necessary, but maybe the compiler needs help ?

"""
    listoflistsofsamelengthpredq(x,pred)

true if x is List of Lists of same length with predq giving true on all elements.
"""
function listoflistsofsamelengthpredq(x::Mxpr{:List},pred)
    listoflistsofsamelengthq(x) && listoflistsofpredq(x,pred)
end
listoflistsofsamelengthpredq(x) = false

####  Symata Predicates

### ConstantQ

@mkapprule ConstantQ :nargs => 1

@sjdoc ConstantQ """
    ConstantQ(x)

return `True` if `x` is a numerical constant.
"""
do_ConstantQ(mx::Mxpr{:ConstantQ}, x) = isConstant(x)

### AtomQ

@mkapprule AtomQ  :nargs => 1
@sjdoc AtomQ """
    AtomQ(expr)

return true if `expr` has no parts accessible with `Part`.

However, currently, Julia `Array`s can be accessed with `Part`, and return `True` under `AtomQ`.
"""
@doap AtomQ(x) = atomq(x)

### OddQ

@mkapprule OddQ  :nargs => 1
@doap OddQ(x) = isa(x, Integer) && ! iseven(x)
@sjdoc OddQ """
    OddQ(expr)

return `True` if `expr` is an odd integer.
"""

### EvenQ

@mkapprule EvenQ  :nargs => 1
@doap EvenQ(x) = isa(x, Integer) && ! iseven(x)
@sjdoc EvenQ """
    EvenQ(expr)

return `True` if `expr` is an even integer.
"""

@sjseealso_group(AtomQ, EvenQ, OddQ)

### DirtyQ

apprules(mx::Mxpr{:DirtyQ}) = checkdirtysyms(mx[1])

@sjdoc DirtyQ """
    DirtyQ(m)

return `True `if the timestamp of any symbol that `m` depends on
is more recent than the timestamp of `m`. This is for diagnostics.
"""
do_syms(mx::Mxpr) = mxpr(:List,listsyms(mx)...)
do_syms(s) = mxpr(:List,)

### NumericQ

@mkapprule NumericQ  :nargs => 1

@sjdoc NumericQ """
    NumericQ(expr)

return true if `N(expr)` would return a number.
"""
do_NumericQ(mx::Mxpr{:NumericQ}, x) = is_Numeric(x)
is_Numeric(x) = false
is_Numeric(x::Number) = true
is_Numeric(x::Symbol) = isConstant(x)
is_Numeric(x::Mxpr) = get_attribute(x,:NumericFunction)  && all( t -> is_Numeric(t), margs(x))

### NumberQ

@mkapprule NumberQ :nargs => 1

@sjdoc NumberQ """
    NumberQ(x)

return true if `x` is an explicit number. i.e. it is a subtype of Julia type `Number`.
"""
@doap NumberQ(x) = isa(x,Number)

### MachineNumberQ

@mkapprule MachineNumberQ :nargs => 1
# Should we check for smaller floats ?

@sjdoc MachineNumberQ """
    MachineNumberQ(x)

return `True` if `x` is a machine-precision floating point number.
"""
@doap MachineNumberQ(x::Float64) = true
@doap MachineNumberQ(x::Complex{Float64}) = true
@doap MachineNumberQ(x) = false

### InexactNumberQ

@mkapprule InexactNumberQ :nargs => 1

@sjdoc InexactNumberQ """
    InexactNumberQ(x)

return `True` if `x` is an inexact number. Inexact numbers are floating
point numbers and floating point complex numbers.
"""
@doap InexactNumberQ(x::AbstractFloat) = true
@doap InexactNumberQ(x::Complex{<:AbstractFloat}) = true
@doap InexactNumberQ(x) = false

### IntegerQ

@mkapprule IntegerQ :nargs => 1
@sjdoc IntegerQ """
    IntegerQ(x)

return true if `x` is an `Integer`.
"""
@doap IntegerQ(x) = isa(x,Integer)

### ListQ

@mkapprule ListQ :nargs => 1
@sjdoc ListQ """
    ListQ(x)

return `True` if the `Head` of `x` is `List`.
"""
@doap ListQ(x::Mxpr{:List}) = true
@doap ListQ(x) = false

### Positive

@mkapprule Positive :nargs => 1
@sjdoc Positive """
    Positive(x)

return `True` if `x` is a positive number. return `Null` if `x` is a `String`.
Otherwise, return the input expression.
"""
@doap Positive(x::Real) = x > 0
@doap Positive(x::Number) = false
@doap Positive(x::String) = nothing

### PermuationQ

@mkapprule PermutationQ :nargs => 1
@sjdoc PermutationQ """
    PermutationQ(list)

return `True` if and only if `list` is a permuation of the integers from `1` through `Length(list)`.
"""
do_PermutationQ(mx::Mxpr{:PermutationQ}, lst::Mxpr{:List}) =  all(t -> typeof(t) <: Union{Integer,AbstractFloat}, margs(lst)) && isperm(margs(lst))

### VectorQ

@mkapprule VectorQ :nargs => 1:2

@sjdoc VectorQ """
    VectorQ(x)

return `True` if `x` is a `List`, none of whose elements is a `List`.

    VectorQ(x,test)

return `True` if `x` is a vector, all of whose elements satisfy the predicate `test`.
For example, `VectorQ(expr, IntegerQ)` returns `True` if `x` is a vector of `Integers`.
"""
vectorq(x::Mxpr{:List}) = all( t -> (! isa(t,Mxpr{:List})), margs(x))
vectorq(x::Mxpr{:List}, test) = isa(test,Function) ? all(t -> test(t), margs(x)) : all(t ->doeval(mxpr(test,t)), margs(x))
@doap VectorQ(x) = false
@doap VectorQ(x::Mxpr{:List}) = vectorq(x)
@doap VectorQ(x::Mxpr{:List}, test) = vectorq(x,test)

### MatrixQ

@mkapprule MatrixQ :nargs => 1:2  :nodefault => true
@sjdoc MatrixQ """
    MatrixQ(m)

return `True` if `m` has the form of a matrix.
`m` has the form of a matrix if it is a `List`, each
of whose elements is a `List` of the same length,
none of whose elements is a `List`.

    MatrixQ(m,test)

return `True` if `test` applied to each element of the matrix `m`.
"""
@doap MatrixQ(args...) = matrixq(args...)

function matrixq(x::Mxpr{:List})
    a = margs(x)
    length(a) < 1 && return false
    len = symlength(a[1])
    all(t -> ( symlength(t) == len && vectorq(t) ), a)
end

function matrixq(x::Mxpr{:List},test)
    a = margs(x)
    length(a) < 1 && return false
    len = symlength(a[1])
    all(t -> ( symlength(t) == len && vectorq(t,test) ), a)
end
matrixq(x) = false
matrixq(x,test) = false

### TrueQ

@mkapprule TrueQ :nargs => 1
@doap TrueQ(x) = trueq(x)

trueq(x::Bool) = x
trueq(x) = false

### Boole

@mkapprule Boole
@doap Boole(x::Bool) = x ? 1 : 0

### BooleanQ

@mkapprule BooleanQ :nodefault => true
@doap BooleanQ(x::Bool) = true
@doap BooleanQ(x...) = false

### FreeQ

## FIXME: Need Heads => True

# don't know how to put this in the macro string
# const levelstring = """
# - `n`       levels `0` through `n`.
# - `[n]`     level `n` only.
# - `[n1,n2]` levels `n1` through `n2`
# """

@mkapprule FreeQ :nargs => 1:3
@doap FreeQ(expr, pattern) = freeq(LevelSpecAll(),expr,pattern)
@doap FreeQ(expr, pattern, inlevelspec)  = freeq(make_level_specification(expr, inlevelspec), expr, pattern)

@curry_second FreeQ
@sjdoc FreeQ """
    FreeQ(expr, pattern)

return `True` if no subexpression of `expr` matches `pattern`.

    FreeQ(expr, pattern, levelspec)

return `True` if `expr` does not match `pattern` on levels specified by `levelspec`.

- `n`       levels `1` through `n`.
- `[n]`     level `n` only.
- `[n1,n2]` levels `n1` through `n2`

    FreeQ(pattern)

operator form.
"""

mutable struct FreeQData
    pattern
    gotmatch::Bool
end

freeq(expr, pat) = freeq(LevelSpecAll(), expr, pat)

function freeq(levelspec::LevelSpec, expr, pat)
    data = FreeQData(pat,false)
    action = LevelAction(data,
                         function (data, expr)
                             (gotmatch,cap) = match_and_capt(expr,patterntoBlank(data.pattern))
                             if gotmatch
                               data.gotmatch = true
                               action.levelbreak = true
                             end
                         end)
    if has_level_zero(levelspec)  # Do level zero separately
        (gotmatch,cap) = match_and_capt(expr,patterntoBlank(data.pattern))
        gotmatch && return false
    end
    traverse_levels!(action,levelspec,expr)
    data.gotmatch && return false
    return true
end

### Element

## We may want to do this with a dictionary. As it is the Julia compiler
## may have a chance to reason.

@sjdoc Element """
    Element(x,dom),  x ∈ dom

asserts that `x` is an element of the domain `dom`.
"""
@mkapprule Element :nargs => 2

@doap function Element(x::Integer,sym::Symbol)
    if sym == :Integers || sym == :Reals || sym == :Rationals || sym == :Complexes ||
        sym == :Algebraics
        return true
    end
    sym == :Primes && return isprime(x)
    sym == :Booleans && return false
    mx
end

@doap function Element(x::AbstractFloat,sym::Symbol)
    sym == :Integers && return round(x) == x ? mx : false
    sym == :Rationals && return mx
    sym == :Complexes && return true
    sym == :Booleans && return false
    sym == :Reals && return true
    sym == :Algebraics && return mx
    if sym == :Primes
        if round(x) == x
            return isprime(convert(Integer,x))
        else
            return false
        end
    end
    mx
end

@doap function Element(x::Complex{T},sym::Symbol) where T
    sym == :Integers && return false
    sym == :Rationals && return false
    sym == :Reals && return false
    sym == :Booleans && return false
    if sym == :Algebraics
        T <: Integer && return true
        return mx
    end
    sym == :Complexes && return true
    mx
end

@doap function Element(x::Bool,sym::Symbol)
    sym == :Booleans && return true
    sym == :Integers && return false
    sym == :Rationals && return false
    sym == :Reals && return false
    sym == :Complexes && return false
    sym == :Algebraics && return false
    mx
end

@doap function Element(x::Rational,sym::Symbol)
    sym == :Booleans && return false
    sym == :Integers && return false
    sym == :Rationals && return true
    sym == :Reals && return true
    sym == :Complexes && return true
    sym == :Algebraics && return true
    mx
end

@doap function Element(x::Symbol,sym::Symbol)
    if x == :Pi || x == :E
        (sym == :Integers || sym == :Rationals || sym == :Booleans || sym == :Algebraics || sym == :Primes) && return false
        (sym == :Reals || sym == :Complexes)  && return true
    end
    if x == :GoldenRatio
        sym == :Algebraics && return true
        sym == :Reals && return true
        sym == :Rationals && return false
        sym == :Integers && return false
        sym == :Complexes && return true
    end
    if x == :Catalan
        sym == :Reals && return true
        sym == :Integers && return false
        sym == :Complexes && return true
    end
    mx
end

## This will return unevaluated some inputs for which the answer is known, eg, powers involving Pi
@doap Element(x::PowerT, sym::Symbol) = _element_power(mx,x,margs(x)...,sym)
function _element_power(mx,x,b::Number,expt::Number,sym)
    (sym == :Integers || sym == :Rationals || sym == :Booleans || sym == :Primes) && return false
    (sym == :Reals || sym == :Complexes || sym == :Algebraics)  && return true
    x
end
_element_power(mx,args...) = mx


## This will probably give the wrong answer for some inputs
@doap function Element(x::TimesT, sym::Symbol)
    if is_Numeric(x) && freeq(LevelSpecAll(),x, mxpr(:Blank,:Symbol))
        (sym == :Integers || sym == :Rationals || sym == :Booleans || sym == :Primes) && return false
        (sym == :Reals || sym == :Complexes || sym == :Algebraics)  && return true
    end
    mx
end

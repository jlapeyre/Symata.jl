# Many of these are redundant. They should be replaced in the rest of the code by `isa`

is_SSJSym(x) = isa(x,SSJSym)
is_SJSym(x) = isa(x,SJSym)

#is_Mxpr{T<:Mxpr}(mx::T) = true
#is_Mxpr(x) = false
is_Mxpr(x) = isa(x,Mxpr)

is_Mxpr{T}(mx::Mxpr{T},s::Symbol) = T == s
is_Mxpr(x,s::Symbol) = false

"""
    mxpr_head_freeq(mx::Mxpr)

return true if any element at level 1 of mx is an Mxpr with head `head`.
"""
mxpr_head_freeq(mx::Mxpr, head) = ! any(t -> isa(t,Mxpr{head}), margs(mx))

is_Number(x) = isa(x,Number)
is_Real(x) = isa(x,Real)
is_Complex(x) = isa(x,Complex)
is_Float(x) = isa(x,AbstractFloat)

is_imaginary_integer{T<:Integer}(z::Complex{T}) = real(z) == 0
is_imaginary_integer(x) = false

atomq(x) = ! isa(x,Mxpr)

is_Indeterminate(x::Symbol) = x == Indeterminate
is_Indeterminate(x) = false

is_Infintity(x) = false
is_Infintity(mx::Mxpr{:DirectedInfinity}) = length(mx) > 0 && mx[1] == 1 ? true : false

is_ComplexInfinity(x) = false
is_ComplexInfinity(mx::Mxpr{:DirectedInfinity}) = length(mx) == 0 ? true : false

# This attribute checking should be encapsulated !
is_Constant(x::SSJSym) = haskey(x.atrr,:Constant)
is_Constant(x::Symbol) = haskey(getssym(x).attr, :Constant)
is_Constant(x) = false

# encapsulate!
is_protected(sj::SJSym) = get(getssym(sj).attr,:Protected,false)

is_blankxxx(x) = isa(x,BlankXXX)

####  Symata Predicates

### ConstantQ

@mkapprule ConstantQ :nargs => 1

@sjdoc ConstantQ """
    ConstantQ(x)
 
return `True` if `x` is a numerical constant.
"""
do_ConstantQ(mx::Mxpr{:ConstantQ}, x) = is_Constant(x)

### AtomQ

@mkapprule AtomQ  :nargs => 1

@sjdoc AtomQ """
    AtomQ(expr)

return true if `expr` has no parts accessible with `Part`.

However, currently, Julia `Array`s can be accessed with `Part`, and return `True` under `AtomQ`.
"""
@doap AtomQ(x) = atomq(x)

### EvenQ

apprules(mx::Mxpr{:EvenQ}) = isa(mx[1],Integer) && iseven(mx[1])

@sjdoc EvenQ """
    EvenQ(expr)

return `True` if `expr` is an even integer.
"""

### OddQ

apprules(mx::Mxpr{:OddQ}) = isa(mx[1],Integer) &&  ! iseven(mx[1])

@sjdoc OddQ """
    OddQ(expr)

return `True` if `expr` is an odd integer.
"""

@sjseealso_group(AtomQ,EvenQ,OddQ)

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
is_Numeric{T<:Number}(x::T) = true
is_Numeric(x::Symbol) = is_Constant(x)
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
@doap InexactNumberQ{T<:AbstractFloat}(x::Complex{T}) = true
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
do_PermutationQ(mx::Mxpr{:PermutationQ}, lst::Mxpr{:List}) =  all( t -> typeof(t) <: Union{Integer,AbstractFloat}, margs(lst)) && isperm(margs(lst))

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

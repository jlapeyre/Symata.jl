#### predicates

is_type(x,t::DataType) = typeof(x) == t
is_type(x,t::Union) = typeof(x) == t

is_type_less(x,t::DataType) = typeof(x) <: t
is_type_less{T}(x::T,t::Union) = T <: t

is_SSJSym(s::SSJSym) = true
is_SSJSym(x) = false

is_SJSym(s::SJSym) = true
is_SJSym(x) = false

is_Mxpr{T<:Mxpr}(mx::T) = true
is_Mxpr(x) = false
is_Mxpr{T}(mx::Mxpr{T},s::Symbol) = T == s
is_Mxpr(x,s::Symbol) = false

is_Number{T<:Number}(mx::T) = true
is_Number(x) = false

is_Real{T<:Real}(mx::T) = true
is_Real(x) = false

is_Complex{T<:Real}(x::Complex{T}) = true
is_Complex(x) = false

is_Float{T<:AbstractFloat}(x::T) = true
is_Float(x) = false

is_imaginary_integer{T<:Integer}(z::Complex{T}) = real(z) == 0
is_imaginary_integer(x) = false

atomq{T<:Mxpr}(x::T) = false
atomq(x) = true

is_Indeterminate(x::Symbol) = x == Indeterminate
is_Indeterminate(x) = false

is_Infintity(x) = false
is_Infintity(mx::Mxpr{:DirectedInfinity}) = length(mx) > 0 && mx[1] == 1 ? true : false

is_ComplexInfinity(x) = false
is_ComplexInfinity(mx::Mxpr{:DirectedInfinity}) = length(mx) == 0 ? true : false

is_Constant(x::SSJSym) = haskey(x.atrr,:Constant)

function is_Constant(x::Symbol)
    sjsym = getssym(x)
    haskey(sjsym.attr,:Constant)
end
is_Constant(x) = false

is_protected(sj::SJSym) = get(getssym(sj).attr,:Protected,false)

# BlankXXX defined in sjuliaconstants.jl
is_blankxxx{T<:BlankXXX}(mx::T) = true
is_blankxxx{T<:Mxpr}(x::T) = false

####  SJulia Predicates

@mkapprule ConstantQ :nargs => 1

do_ConstantQ(mx::Mxpr{:ConstantQ}, s::Symbol) = is_Constant(s)
do_ConstantQ(mx::Mxpr{:ConstantQ}, x) = false

@sjdoc AtomQ "
AtomQ(expr), in principle, returns true if expr has no parts accessible with Part.
However, currently, Julia Arrays can be accessed with Part, and return true under AtomQ.
"

@mkapprule AtomQ  :nargs => 1

@doap AtomQ(x) = atomq(x)

@sjdoc EvenQ "
EvenQ(expr) returns true if expr is an even integer.
"
@sjdoc OddQ "
OddQ(expr) returns true if expr is an odd integer.
"

@sjseealso_group(AtomQ,EvenQ,OddQ)
apprules(mx::Mxpr{:EvenQ}) = is_type_less(mx[1],Integer) && iseven(mx[1])
apprules(mx::Mxpr{:OddQ}) = is_type_less(mx[1],Integer) &&  ! iseven(mx[1])

@sjdoc DirtyQ "
DirtyQ(m) returns true if the timestamp of any symbol that m depends on
is more recent than the timestamp of m. This is for diagnostics.
"
apprules(mx::Mxpr{:DirtyQ}) = checkdirtysyms(mx[1])
do_syms(mx::Mxpr) = mxpr(:List,listsyms(mx)...)
do_syms(s) = mxpr(:List,)

#### NumericQ

@mkapprule NumericQ  :nargs => 1
@sjdoc NumericQ "
NumericQ(expr) returns true if N(expr) would return a number.
"
do_NumericQ(mx::Mxpr{:NumericQ}, x) = is_Numeric(x)
is_Numeric(x) = false
is_Numeric{T<:Number}(x::T) = true
is_Numeric(x::Symbol) = is_Constant(x)
function is_Numeric{T<:Mxpr}(x::T)
    get_attribute(x,:NumericFunction) || return false
    for i in 1:length(x)
        is_Numeric(x[i]) || return false
    end
    return true
end

#### IntegerQ
@mkapprule IntegerQ :nargs => 1
do_IntegerQ{T<:Integer}(mx::Mxpr{:IntegerQ}, x::T) = true
do_IntegerQ(mx::Mxpr{:IntegerQ}, x) = false


#### PermuationQ

@mkapprule PermutationQ :nargs => 1

@sjdoc PermutationQ "
PermutationQ(list) returns true if and only if list is a permuation of the integers from 1 through Length(list).
"

function do_PermutationQ(mx::Mxpr{:PermutationQ}, lst::Mxpr{:List})
    args = margs(lst)
    for arg in args
        ! (typeof(arg) <: Union{Integer,AbstractFloat}) && return false
    end
    isperm(args)
end

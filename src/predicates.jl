## predicates

is_type(x,t::DataType) = typeof(x) == t
is_type_less(x,t::DataType) = typeof(x) <: t
is_type(x,t::UnionType) = typeof(x) == t
is_type_less(x,t::UnionType) = typeof(x) <: t

#is_SJSym{T<:SJSym}(s::T) = true
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
is_Complex(x::Complex) = true
is_Complex(x) = false

is_imaginary_integer{T<:Integer}(z::Complex{T}) = real(z) == 0
is_imaginary_integer(x) = false

atomq(x::Mxpr) = false
atomq(x) = true

#function is_indexable(x)
#end

typealias BlankXXX Union(Mxpr{:Blank},Mxpr{:BlankSequence},Mxpr{:BlankNullSequence})
is_blankxxx(mx::BlankXXX) = true
is_blankxxx(x::Mxpr) = false

###  SJulia Predicates

@sjdoc AtomQ "
AtomQ(expr), in principle, returns true if expr has no parts accesible with Part.
However, currently, Julia Arrays can be accessed with Part, and return true under AtomQ.
"
apprules(mx::Mxpr{:AtomQ}) = atomq(mx[1])

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

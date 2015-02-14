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
is_Complex(x::Complex) = true
is_Complex(x) = false

atomq(x::Mxpr) = false
atomq(x) = true

#function is_indexable(x)
#end

typealias BlankXXX Union(Mxpr{:Blank},Mxpr{:BlankSequence},Mxpr{:BlankNullSequence})
is_blankxxx(mx::BlankXXX) = true
is_blankxxx(x::Mxpr) = false

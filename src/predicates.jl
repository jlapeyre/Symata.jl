## predicates

is_type(x,t::DataType) = typeof(x) == t
is_type_less(x,t::DataType) = typeof(x) <: t
is_type(x,t::UnionType) = typeof(x) == t
is_type_less(x,t::UnionType) = typeof(x) <: t

is_SJSym{T<:SJSym}(s::T) = true
is_SJSym(x) = false
is_Mxpr{T<:Mxpr}(mx::T) = true
is_Mxpr(x) = false
is_Number{T<:Number}(mx::T) = true
is_Number(x) = false

atomq(x::Mxpr) = false
atomq(x) = true

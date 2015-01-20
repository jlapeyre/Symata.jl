type SJSym
    name::Symbol
    val::Any
    HoldFirst::Bool
end

const SYMTAB = Dict{Symbol,SJSym}()

abstract AbstractMxpr
type Mxpr{T} <: AbstractMxpr
    head::SJSym
    args::Array{Any,1}
end

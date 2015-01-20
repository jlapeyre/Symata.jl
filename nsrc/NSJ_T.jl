type SJSym
    name::Symbol
    val::Any
    HoldFirst::Bool
    HoldAll::Bool    
    Protected::Bool    
end

abstract AbstractMxpr
type Mxpr{T} <: AbstractMxpr
    head::SJSym
    args::Array{Any,1}
end

const SYMTAB = Dict{Symbol,SJSym}()

function getsym(s::Symbol)
    if haskey(SYMTAB,s)
        return SYMTAB[s]
    else
        ns = sjsym(s)
        SYMTAB[s] = ns
        return ns
    end
end

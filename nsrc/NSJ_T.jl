type SJSym
    name::Symbol
    val::Any
    attr::Dict{Symbol,Bool}
    downvalues::Dict{Symbol,Any}  # need Mxpr here instead of Any
end

symname(s::SJSym) = s.name
symval(s::SJSym) = s.val
setsymval(s::SJSym,val) = (s.val = val)

abstract AbstractMxpr
type Mxpr{T} <: AbstractMxpr
    head::SJSym
    args::Array{Any,1}
end

const SYMTAB = Dict{Symbol,SJSym}()

## Retrieve or create new symbol
sjsym(s::Symbol) = SJSym(s,s,Dict{Symbol,Bool}(),Dict{Symbol,Any}())
function getsym(s::Symbol)
    if haskey(SYMTAB,s)
        return SYMTAB[s]
    else
        ns = sjsym(s)
        SYMTAB[s] = ns
        return ns
    end
end
getsym(ss::String) = getsym(symbol(ss))

getsymval(s::Symbol) = getsym(s).val

get_attribute(s::Symbol, a::Symbol) = get_attribute(getsym(s),a)
set_attribute(s::Symbol, a::Symbol) = set_attribute(getsym(s),a)
unset_attribute(s::Symbol, a::Symbol) = unset_attribute(getsym(s),a)

function get_attribute(sj::SJSym, a::Symbol)
    get(sj.attr,a,false)
end

function set_attribute(sj::SJSym, a::Symbol)
    sj.attr[a] = true
end

function unset_attribute(sj::SJSym, a::Symbol)
    sj.attr[a] = false
end

abstract AbstractTSym
type TSym{T} <: AbstractTSym
end

tsymname{T}(t::TSym{T}) = T

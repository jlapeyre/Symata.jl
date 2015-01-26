abstract AbstractSJSym
type SJSym{T}  <: AbstractSJSym
    val::Any
    attr::Dict{Symbol,Bool}
    downvalues::Array{Any,1}
end

symname{T}(s::SJSym{T}) = T
symval(s::SJSym) = getsym(s).val  # This is the key: Look up the copy in the table
function setsymval(s::SJSym,val)
    (getsym(s).val = val)  # maybe not necessary
end

import Base:  ==

Base.isless{T,S}(t::SJSym{T}, s::SJSym{S}) = T < S
Base.isless(t::SJSym,s::SJSym) = symname(t)  < symname(s)
Base.isless(s::SJSym,t) = symname(s) < t
Base.isless(t,s::SJSym) = t  < symname(s)
==(s::SJSym,t::SJSym) = symname(s) == symname(t) # but, we sometimes have rogue copies

function push_downvalue(s::SJSym,val)
    dv = s.downvalues
    isnewrule = true
    for i in 1:length(dv)
        if val[1] == dv[i][1]
            dv[i] = val
            isnewrule = false
            break
        end
    end
    isnewrule && push!(s.downvalues,val)
    sort!(s.downvalues,lt=isless_patterns)
end
    
clear_downvalues(s::SJSym) = (s.downvalues = Array(Any,0))

abstract AbstractMxpr
type Mxpr{T} <: AbstractMxpr
    head::SJSym
    args::Array{Any,1}
end

function mxpr(s::SJSym,iargs...)
    args = newargs()
    for x in iargs push!(args,x) end
    Mxpr{symname(s)}(s,args)
end
mxpr(s::Symbol,args...) = mxpr(getsym(s),args...)

margs(mx::Mxpr) = mx.args

newargs() = Array(Any,0)

#setindex!(mx::Mxpr, val, k::Int) = k == 0 ? sethead(mx,val) : (margs(mx)[k] = val)
# No, this should be fast
setindex!(mx::Mxpr, val, k::Int) = (margs(mx)[k] = val)

getindex(mx::Mxpr, k::Int) = margs(mx)[k]
Base.length(mx::Mxpr) = length(margs(mx))
Base.length(s::SJSym) = 0
Base.endof(mx::Mxpr) = length(mx)

# We need to think about copying in the following. Support both refs and copies ?
function getindex(mx::Mxpr, r::UnitRange)
    if r.start == 0
        return mxpr(mx[0],margs(mx)[1:r.stop]...)
    else
        return margs(mx)[r]
    end
end

function getindex(mx::Mxpr, r::StepRange)
    if r.start == 0
        return mxpr(mx[0],margs(mx)[0+r.step:r.step:r.stop]...)
    elseif r.stop == 0 && r.step < 0
        return mxpr(mx[r.start],margs(mx)[r.start-1:r.step:1]...,mx[0])
    else
        return margs(mx)[r]
    end
end


#downvalues(s::SJSym) = s.downvalues
# FIXME!
# workaround for mysterious bug, SJSym is copied somewhere and downvalues are altered.
# So we look up the orginal SJSym from the name
downvalues(s::SJSym) = getsym(symname(s)).downvalues
downvalues(s::Symbol) = downvalues(getsym(s))
listdownvalues(s::SJSym) = mxpr(:List,s.downvalues...)

const SYMTAB = Dict{Symbol,SJSym}()

## Retrieve or create new symbol
#sjsym(s::Symbol) = SJSym{s}(s,s,Dict{Symbol,Bool}(),Array(Any,0))
sjsym(s::Symbol) = SJSym{s}(s,Dict{Symbol,Bool}(),Array(Any,0))
function getsym(s::Symbol)
    if haskey(SYMTAB,s)
        return SYMTAB[s]
    else
        ns = sjsym(s)
        SYMTAB[s] = ns
        # pollute Julia just so we get repl completion. remove this later.        
        !isdefined(s) && eval(:($s = true)) 
        return ns
    end
end
getsym(ss::String) = getsym(symbol(ss))

# refresh a copy that is not in the symbol table. how does this happen?
getsym(sjs::SJSym) = getsym(symname(sjs))

getsymval(s::Symbol) = getsym(s).val

get_attribute(s::Symbol, a::Symbol) = get_attribute(getsym(s),a)
set_attribute(s::Symbol, a::Symbol) = set_attribute(getsym(s),a)
unset_attribute(s::Symbol, a::Symbol) = unset_attribute(getsym(s),a)


function protectedsymbols()
    args = newargs()
    for s in keys(SYMTAB)
        if get_attribute(s,:Protected) push!(args,getsym(s)) end
    end
    mxpr(:List, sort!(args)...)
end

function get_attribute(sj::SJSym, a::Symbol)
    get(sj.attr,a,false)
end

function set_attribute(sj::SJSym, a::Symbol)
    sj.attr[a] = true
end

function unset_attribute(sj::SJSym, a::Symbol)
    sj.attr[a] = false
end

function Base.copy(mx::Mxpr)
    args = copy(mx.args)
    mxpr(mx.head,args...)
end

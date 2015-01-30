abstract AbstractSJSym
type SJSym{T}  <: AbstractSJSym
    val::Any
    attr::Dict{Symbol,Bool}
    downvalues::Array{Any,1}
    age::UInt64    
end

## Retrieve or create new symbol
#sjsym(s::Symbol) = SJSym{s}(s,s,Dict{Symbol,Bool}(),Array(Any,0))
#sjsym(s::Symbol) = SJSym{s}(s,Dict{Symbol,Bool}(),Array(Any,0),Dict{Symbol,UInt64}())
sjsym(s::Symbol) = SJSym{s}(s,Dict{Symbol,Bool}(),Array(Any,0),0)

symname{T}(s::SJSym{T}) = T
symval(s::SJSym) = getsym(s).val  # This is the key: Look up the copy in the table
function setsymval(s::SJSym,val)
    (getsym(s).val = val)  # maybe not necessary to get symbol from table
    s.age += 1
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

typealias MxprArgs Array{Any,1}

abstract AbstractMxpr
type Mxpr{T} <: AbstractMxpr
    head::SJSym
    args::MxprArgs
    fixed::Bool
    canon::Bool
    syms::Dict{Symbol,UInt64}    
end

typealias Symbolic Union(Mxpr,SJSym)

function mxpr(s::SJSym,iargs...)
    args = newargs()
    for x in iargs push!(args,x) end
    Mxpr{symname(s)}(s,args,false,false,Dict{Symbol,UInt64}())
end

function mxpr(s::SJSym,args::MxprArgs)
    Mxpr{symname(s)}(s,args,false,false,Dict{Symbol,UInt64}())
end

function mxprcf(s::SJSym,iargs...)
    args = newargs()
    for x in iargs push!(args,x) end
    Mxpr{symname(s)}(s,args,true,true,Dict{Symbol,UInt64}())
end

function mxprcf(s::SJSym,args::MxprArgs)
    Mxpr{symname(s)}(s,args,true,true,Dict{Symbol,UInt64}())
end

mxpr(s::Symbol,args...) = mxpr(getsym(s),args...)
mxprcf(s::Symbol,args...) = mxprcf(getsym(s),args...)
margs(mx::Mxpr) = mx.args

# is_canon(mx::Mxpr) = mx.canon
# is_fixed(mx::Mxpr) = mx.canon
# is_fixed{T}(s::SJSym{T}) = symval(s) == T
# setcanon(mx::Mxpr) = mx.canon = true
# setfixed(mx::Mxpr) = mx.canon = true
# unsetcanon(mx::Mxpr) = mx.canon = false
# unsetfixed(mx::Mxpr) = mx.canon = false

is_canon(x) = false
setcanon(x) = false
unsetcanon(x) = false
is_fixed(x) = false
setfixed(x) = false
unsetfixed(x) = false

newargs() = Array(Any,0)
newargs(n::Integer) = Array(Any,n)

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
# We may have fixed this with change in getsymval.
# We look up the orginal SJSym from the name
# because altered, spurious, copies of downvalues were being used.
downvalues(s::SJSym) = getsym(symname(s)).downvalues
downvalues(s::Symbol) = downvalues(getsym(s))
listdownvalues(s::SJSym) = mxpr(:List,s.downvalues...)

const SYMTAB = Dict{Symbol,SJSym}()

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

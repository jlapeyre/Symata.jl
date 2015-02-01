type Evalage
    t::UInt64
end

const evalage = Evalage(0)

increvalage() = evalage.t += 1
getevalage() = evalage.t

typealias SJSym Symbol 

abstract AbstractSJSym
type SSJSym{T}  <: AbstractSJSym
    val::Any
    attr::Dict{Symbol,Bool}
    downvalues::Array{Any,1}
    age::UInt64
end

#sjsym(s::Symbol) = SJSym{s}(s,s,Dict{Symbol,Bool}(),Array(Any,0))
#sjsym(s::Symbol) = SJSym{s}(s,Dict{Symbol,Bool}(),Array(Any,0),Dict{Symbol,UInt64}())
ssjsym(s::Symbol) = SSJSym{s}(s,Dict{Symbol,Bool}(),Array(Any,0),0)

#symname{T}(s::SJSym{T}) = T
symname(s::SJSym) = s
symname(s::String) = symbol(s)
getsym(s) = s
symval(s::SJSym) = getssym(s).val
function setsymval(s::SJSym,val)
    (getssym(s).val = val)
#    getssym(s).age += 1
    getssym(s).age = increvalage()
end

sjset(s::SJSym,val) = setsymval(s,val)

symage(s::SJSym) = getssym(s).age

import Base:  ==

function push_downvalue(ins::SJSym,val)
    s = getssym(ins)
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
    
clear_downvalues(s::SJSym) = (getssym(s).downvalues = Array(Any,0))

typealias MxprArgs Array{Any,1}

abstract AbstractMxpr
type Mxpr{T} <: AbstractMxpr
    head::SJSym
    args::MxprArgs
    fixed::Bool
    canon::Bool
    syms::Dict{Symbol,UInt64}
    age::UInt64  # Not used yet.
end

function Base.hash(mx::Mxpr, h::UInt64)
    hout = hash(mx.head,h)
    hout = hash(mx.args,hout)
end

typealias Symbolic Union(Mxpr,SJSym)

function mxpr(s::SJSym,iargs...)
    args = newargs()
    for x in iargs push!(args,x) end
    Mxpr{symname(s)}(s,args,false,false,Dict{Symbol,UInt64}(),0)
end

function mxpr(s::SJSym,args::MxprArgs)
    Mxpr{symname(s)}(s,args,false,false,Dict{Symbol,UInt64}(),0)
end

# set fixed point and clean bits
function mxprcf(s::SJSym,iargs...)
    args = newargs()
    for x in iargs push!(args,x) end
    Mxpr{symname(s)}(s,args,true,true,Dict{Symbol,UInt64}(),0)
end

function mxprcf(s::SJSym,args::MxprArgs)
    Mxpr{symname(s)}(s,args,true,true,Dict{Symbol,UInt64}(),0)
end

# stack overflow
#mxpr(s::Symbol,args...) = mxpr(getsym(s),args...)
#mxprcf(s::Symbol,args...) = mxprcf(getsym(s),args...)
margs(mx::Mxpr) = mx.args

function mergesyms(mx::Mxpr, a::Mxpr)
    mxs = mx.syms
    for (sym,age) in a.syms
        if haskey(mxs,sym) && age  < mxs[sym]
            mxs[sym] = age
        else
            mxs[sym] = age
        end
    end
end

# Existing record of SJSym a must be older
function mergesyms(mx::Mxpr, a::SJSym)
#    println("merging $a")
    if ! haskey(mx.syms, a)
#        println("setting age $an, $(a.age)")
        (mx.syms)[a] = getssym(a).age
    end
#    dump(mx.syms)
end

function checkdirtysyms(mx::Mxpr)
    length(mx.syms) == 0 && return true  # assume re-eval is necessary
    for (sym,age) in mx.syms
        symage(sym) > age && return true
    end
    return false  # no symbols in mx have been set since mx was constructed
end
checkdirtysyms(x) = false

#function setcleansyms(mx::Mxpr)
#    for (sym,age) in mx.syms
#end

is_canon(mx::Mxpr) = mx.canon
is_fixed(mx::Mxpr) = mx.fixed
is_fixed(s::SJSym) = symval(s) == s
#is_fixed{T}(s::SJSym{T}) = symval(s) == T
setcanon(mx::Mxpr) = mx.canon = true
setfixed(mx::Mxpr) = mx.fixed = true
unsetcanon(mx::Mxpr) = mx.canon = false
unsetfixed(mx::Mxpr) = mx.fixed = false

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
downvalues(s::SJSym) = getssym(s).downvalues
listdownvalues(s::SJSym) = mxpr(:List,downvalues(s)...)

const SYMTAB = Dict{Symbol,SSJSym}()

## Retrieve or create new symbol
function getssym(s::Symbol)
    if haskey(SYMTAB,s)
        return SYMTAB[s]
    else
        ns = ssjsym(s)
        SYMTAB[s] = ns
        # pollute Julia just so we get repl completion. remove this later.        
        !isdefined(s) && eval(:($s = true)) 
        return ns
    end
end
getssym(ss::String) = getssym(symbol(ss))

# refresh a copy that is not in the symbol table. how does this happen?
#getsym(sjs::SJSym) = getsym(symname(sjs))

# was never used
#getsymval(s::Symbol) = getsym(s).val

#get_attribute(s::Symbol, a::Symbol) = get_attribute(getsym(s),a)
#set_attribute(s::Symbol, a::Symbol) = set_attribute(getsym(s),a)
#unset_attribute(s::Symbol, a::Symbol) = unset_attribute(getsym(s),a)

function protectedsymbols()
    args = newargs()
    for s in keys(SYMTAB)
        if get_attribute(s,:Protected) push!(args,getsym(s)) end
    end
    mxpr(:List, sort!(args)...)
end

function get_attribute(sj::SJSym, a::Symbol)
    get(getssym(sj).attr,a,false)
end

function set_attribute(sj::SJSym, a::Symbol)
    getssym(sj).attr[a] = true
end

function unset_attribute(sj::SJSym, a::Symbol)
    getssym(sj).attr[a] = false
end

function Base.copy(mx::Mxpr)
#    println("copying $mx")
    args = copy(mx.args)
    mxpr(mx.head,args)
end

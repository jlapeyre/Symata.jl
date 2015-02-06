type Evalage
    t::UInt64
end

const evalage = Evalage(0)

# These are probably all inlined anyway

@inline increvalage() = evalage.t += 1
@inline getevalage() = evalage.t

typealias SJSym Symbol 

abstract AbstractSJSym
type SSJSym{T}  <: AbstractSJSym
    val::Any
    attr::Dict{Symbol,Bool}  # attributes
    downvalues::Array{Any,1}
    age::UInt64
end

#sjsym(s::Symbol) = SJSym{s}(s,s,Dict{Symbol,Bool}(),Array(Any,0))
#sjsym(s::Symbol) = SJSym{s}(s,Dict{Symbol,Bool}(),Array(Any,0),Dict{Symbol,UInt64}())
@inline ssjsym(s::Symbol) = SSJSym{s}(s,Dict{Symbol,Bool}(),Array(Any,0),0)

@inline symname{T}(s::SSJSym{T}) = T
@inline symname(s::SJSym) = s
@inline symname(s::String) = symbol(s)
@inline symattr(s::SJSym) = getssym(s).attr
@inline getsym(s) = s  # careful, this is not getssym
@inline symval(s::SJSym) = getssym(s).val
@inline symval(s::SSJSym) = s.val
sjval(s::SJSym) = getssym(s).val  # intended to be used from within Julia, or quoted julia
@inline function setsymval(s::SJSym,val)
    (getssym(s).val = val)
    getssym(s).age = increvalage()
end

@inline sjset(s::SJSym,val) = setsymval(s,val)

@inline symage(s::SJSym) = getssym(s).age
@inline getage(s::SJSym) = symage(s)  # should only use one of these

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
    
@inline clear_downvalues(s::SJSym) = (getssym(s).downvalues = Array(Any,0))

typealias MxprArgs Array{Any,1}

abstract AbstractMxpr
type Mxpr{T} <: AbstractMxpr
    head::SJSym
    args::MxprArgs
    fixed::Bool
    canon::Bool
    syms::Dict{Symbol,Bool}
    age::UInt64
    key::UInt64
    typ::DataType
end

mxprtype{T}(mx::Mxpr{T}) = T

# Important that we do not hash any meta data. We take the
# cached version
function Base.hash(mx::Mxpr, h::UInt64)
    dohash(mx,h)
end

# Hmm almost works
function Base.hash(mx::Mxpr)
    mx.key != 0 && return mx.key
    hout = hash(mx.head)
    for a in mx.args
        hout = hash(a,hout)
    end
    hout    
end

@inline function dohash(mx::Mxpr, h::UInt64)
    hout = hash(mx.head,h)
    for a in mx.args
        hout = hash(a,hout)
    end
    hout
end
                  
const EXPRDICT = Dict{UInt64,Mxpr}()

global gotit = 0

# Slows operations down by factor of 2 to 5 or more or less
@inline function checkhash(mx::Mxpr)
    mx.key != 0 && return mx
    k = hash(mx)
    if haskey(EXPRDICT,k)
        return EXPRDICT[k]
    end
    mx.key = k
    EXPRDICT[k] = mx
    mx
end
@inline checkhash(x) = x

typealias Symbolic Union(Mxpr,SJSym)

function mxpr(s::SJSym,iargs...)
    args = newargs()
    for x in iargs push!(args,x) end
    mx = Mxpr{symname(s)}(s,args,false,false,Dict{Symbol,UInt64}(),0,0,Any)
    setage(mx)
#    checkhash(mx)
    mx
end

@inline function mxpr(s::SJSym,args::MxprArgs)
    mx =Mxpr{symname(s)}(s,args,false,false,Dict{Symbol,UInt64}(),0,0,Any)
    setage(mx)
#    checkhash(mx)    
    mx
end

# set fixed point and clean bits
@inline function mxprcf(s::SJSym,iargs...)
    args = newargs()
    for x in iargs push!(args,x) end
    mx = Mxpr{symname(s)}(s,args,true,true,Dict{Symbol,UInt64}(),0,0,Any)
#    checkhash(mx)
    mx
end

@inline function mxprcf(s::SJSym,args::MxprArgs)
    mx = Mxpr{symname(s)}(s,args,true,true,Dict{Symbol,UInt64}(),0,0,Any)
#    checkhash(mx)
    mx
end

@inline setage(mx::Mxpr) = mx.age = increvalage()
@inline getage(mx::Mxpr) = mx.age

# For Symbol. Better to avoid calling on Symbols. They should not be update
# just for evaluating to themselves
#setage(x) = true 

@inline margs(mx::Mxpr) = mx.args

# record dependence of mx on symbols that a depends on
@inline function mergesyms(mx::Mxpr, a::Mxpr)
    mxs = mx.syms
#    println("INMERGE MXPR $mx: $a")
    for sym in keys(a.syms)
#        println("** INMERGE MXPR $mx: $sym")
        mxs[sym] = true
    end
end

# record dependence of mx on symbol a
@inline function mergesyms(mx::Mxpr, a::SJSym)
    (mx.syms)[a] = true    
end

# should we detect type and not call these ?
@inline mergesyms(x,y) = true
@inline setfixed(x) = true

@inline function checkdirtysyms(mx::Mxpr)
    length(mx.syms) == 0 && return true   # assume re-eval is necessary if there are no syms
    mxage = mx.age
    for (sym,age) in mx.syms
        symage(sym) > mxage && return true        
    end
    return false  # no symbols in mx have been set since mx age was updated
end
@inline checkdirtysyms(x) = false

#function setcleansyms(mx::Mxpr)
#    for (sym,age) in mx.syms
#end

@inline is_canon(mx::Mxpr) = mx.canon
@inline is_fixed(mx::Mxpr) = mx.fixed
#is_fixed(s::SJSym) = symval(s) == s
#is_fixed{T}(s::SJSym{T}) = symval(s) == T
@inline setcanon(mx::Mxpr) = mx.canon = true
@inline setfixed(mx::Mxpr) = mx.fixed = true
@inline unsetcanon(mx::Mxpr) = mx.canon = false
@inline unsetfixed(mx::Mxpr) = mx.fixed = false

@inline is_canon(x) = false
@inline setcanon(x) = false
@inline unsetcanon(x) = false
#is_fixed(x) = false
#setfixed(x) = false
#unsetfixed(x) = false

@inline newargs() = Array(Any,0)
@inline newargs(n::Integer) = Array(Any,n)

#setindex!(mx::Mxpr, val, k::Int) = k == 0 ? sethead(mx,val) : (margs(mx)[k] = val)
# No, this should be fast
@inline setindex!(mx::Mxpr, val, k::Int) = (margs(mx)[k] = val)

@inline getindex(mx::Mxpr, k::Int) = margs(mx)[k]
@inline Base.length(mx::Mxpr) = length(margs(mx))
@inline Base.length(s::SJSym) = 0
@inline Base.endof(mx::Mxpr) = length(mx)

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
@inline function getssym(s::Symbol)
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
@inline getssym(ss::String) = getssym(symbol(ss))

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

@inline function Base.copy(mx::Mxpr)
#    println("copying $mx")
    args = copy(mx.args)
    mxpr(mx.head,args)
end

typealias Orderless Union(Mxpr{:Plus},Mxpr{:Times})
typealias Blanks Union(Mxpr{:Blank},Mxpr{:BlankSequence},Mxpr{:BlankNullSequence})

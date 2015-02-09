type Evalage
    t::UInt64
end

const evalage = Evalage(0)

# These are probably all inlined anyway

@inline increvalage() = evalage.t += 1
@inline getevalage() = evalage.t

#####################################################################
# SSJSym                                                            #
# data associated with SJulia symbols are instances of type SSJSym  #
# SJSym is just Symbol. It is an older abstraction                  #
# Implementing SJulia symbols is still in flux                      #
#####################################################################
typealias SJSym Symbol 

# Almost all symbols use Any for parameter T.
# We experiented a bit with a value of Int for some symbols
# It may be better to have no parameter, or that it means
# something else.
# The name of the SJulia symbol is a Symbol. The symbol
# table maps Symbol to SSJSym.
abstract AbstractSJSym
type SSJSym{T}  <: AbstractSJSym
#    val::Any
    val::T
    attr::Dict{Symbol,Bool}  # attributes
    downvalues::Array{Any,1}
    age::UInt64
end

newattributes() = Dict{Symbol,Bool}()
newdownvalues() = Array(Any,0)

# We have a choice to carry the symbol name in the type parameter or a a field,
# in which case the value of the symbol is typed
# Form of these functions depend on whether the symbol name is a type parameter
# or a field
@inline ssjsym(s::Symbol) = SSJSym{Any}(s,newattributes(),newdownvalues(),0)
#@inline ssjsym(s::Symbol) = SSJSym{s}(s,Dict{Symbol,Bool}(),Array(Any,0),0)
#@inline symname{T}(s::SSJSym{T}) = T

# not for current implementation
#@inline symname{T}(s::SSJSym{T}) = s.val

## Typed SJ Symbols. Only experimental
ssjsym(s::Symbol,T::DataType) = SSJSym{T}(zero(T),newattributes(),newdownvalues(),0)

@inline symname(s::SJSym) = s
#@inline symname(s::String) = symbol(s)
@inline symattr(s::SJSym) = getssym(s).attr
@inline getsym(s) = s  # careful, this is not getssym
sjval(s::SJSym) = getssym(s).val  # intended to be used from within Julia, or quoted julia. not used anywhere in code

@inline symval(s::SJSym) = getssym(s).val
@inline symval(s::SSJSym) = s.val
@inline function setsymval(s::SJSym,val)
    (getssym(s).val = val)
    getssym(s).age = increvalage()
end

# Try storing values in a Dict instead of a field. Not much difference.
# @inline symval(s::SJSym) = return haskey(SYMVALTAB,s) ? SYMVALTAB[s] : s
# @inline function setsymval(s::SJSym,val)
# #     (getssym(s).val = val)        
#      SYMVALTAB[s] = val
#      getssym(s).age = increvalage()
# end
# #@inline symval(s::SSJSym) = s.val

@inline sjset(s::SJSym,val) = setsymval(s,val)

@inline symage(s::SJSym) = getssym(s).age
@inline getage(s::SJSym) = symage(s)  # should only use one of these

## symbol table for SJulia symbols
const SYMTAB = Dict{Symbol,SSJSym}()
const SYMVALTAB = Dict{Symbol,Any}()  # experiment with keep values elsewhere

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
    s.age = increvalage()    
end
    
@inline clear_downvalues(s::SJSym) = (getssym(s).downvalues = Array(Any,0))

##################################################################
# Mxpr                                                           #
# all SJulia expressions are represented by instances of Mxpr    #
##################################################################
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
typealias Symbolic Union(Mxpr,SJSym)
newargs() = Array(Any,0)
newargs(n::Integer) = Array(Any,n)
newsymsdict() = Dict{Symbol,Bool}()  # create dict for field syms of Mxpr
mhead(mx::Mxpr) = mx.head
margs(mx::Mxpr) = mx.args
margs(mx::Mxpr,n::Int) = mx.args[n]  # need to get rid of this, use getindex, setindex
@inline setage(mx::Mxpr) = mx.age = increvalage()
@inline getage(mx::Mxpr) = mx.age

# These should be fast: In the SJulia language, mx[0] gets the head, but not here.
@inline setindex!(mx::Mxpr, val, k::Int) = (margs(mx)[k] = val)
@inline getindex(mx::Mxpr, k::Int) = margs(mx)[k]
@inline Base.length(mx::Mxpr) = length(margs(mx))
@inline Base.length(s::SJSym) = 0
@inline Base.endof(mx::Mxpr) = length(mx)
mxprtype{T}(mx::Mxpr{T}) = T

# Table for storing Mxpr indexed by hash code.
# Not using this at the moment.
const EXPRDICT = Dict{UInt64,Mxpr}()
global gotit = 0   # non constant global, only for testing

# hash function for expressions.
# Mma and Maple claim to use hash functions for all expressions. But, we find
# this this is very expensive.
#
# Important that we do not hash any meta data, eg two expressions with
# different timestamps, that are otherwise the same should map to the same key.
function Base.hash(mx::Mxpr, h::UInt64)
    dohash(mx,h)
end

# Hmm almost works
function Base.hash(mx::Mxpr)
    mx.key != 0 && return mx.key
    hout = hash(mhead(mx))
    for a in margs(mx)
        hout = hash(a,hout)
    end
    hout    
end

function dohash(mx::Mxpr, h::UInt64)
    hout = hash(mhead(mx),h)
    for a in margs(mx)
        hout = hash(a,hout)
    end
    hout
end

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

function mxpr(s::SJSym,iargs...)
    args = newargs()
    for x in iargs push!(args,x) end
    mx = Mxpr{symname(s)}(s,args,false,false,newsymsdict(),0,0,Any)
    setage(mx)
#    checkhash(mx)
    mx
end

@inline function mxpr(s::SJSym,args::MxprArgs)
    mx =Mxpr{symname(s)}(s,args,false,false,newsymsdict(),0,0,Any)
    setage(mx)
#    checkhash(mx)    
    mx
end

# set fixed point and clean bits
@inline function mxprcf(s::SJSym,iargs...)
    args = newargs()
    for x in iargs push!(args,x) end
    mx = Mxpr{symname(s)}(s,args,true,true,newsymsdict(),0,0,Any)
#    checkhash(mx)
    mx
end

@inline function mxprcf(s::SJSym,args::MxprArgs)
    mx = Mxpr{symname(s)}(s,args,true,true,newsymsdict(),0,0,Any)
#    checkhash(mx)
    mx
end

# For Symbol. Better to avoid calling on Symbols. They should not be update
# just for evaluating to themselves
#setage(x) = true 

# record dependence of mx on symbols that a depends on.
# Reject protected symbols ?
@inline function mergesyms(mx::Mxpr, a::Mxpr)
    mxs = mx.syms
    for sym in keys(a.syms)
        mxs[sym] = true
    end
    h = mhead(a)
    if ! is_protected(h)
        mxs[h] = true
    end
end

#
function mergesyms(mxs::Dict, a::Mxpr)
    for sym in keys(a.syms)
#        println("m1: $sym")
        mxs[sym] = true
    end
    h = mhead(a)
    if ! is_protected(h)
        mxs[h] = true
    end
end

function mergesyms(mxs::Dict, a::SJSym)
#    println("m2: $a")    
    mxs[a] = true
end

mergesyms(x,y) = nothing

# Copy contents of symlists of mx[i] to symlist of mx
function mergeargs(mx::Mxpr)
    for i in 1:length(mx)
#        println("mergeargs $i: ", listsyms(mx[i]))
        mergesyms(mx,mx[i])
    end
end

mergeargs(x) = nothing

# record dependence of mx on symbol a
@inline function mergesyms(mx::Mxpr, a::SJSym)
    (mx.syms)[a] = true    
end

function clearsyms(mx::Mxpr)
    mx.syms = newsymsdict()
end

# should we detect type and not call these ?

@inline setfixed(x) = true

@inline function checkdirtysyms(mx::Mxpr)
    length(mx.syms) == 0 && return true   # assume re-eval is necessary if there are no syms
    mxage = mx.age
    for (sym,age) in mx.syms
        symage(sym) > mxage && return true        
    end
#    symage(mhead(mx)) > mxage && return true
    return false  # no symbols in mx have been set since mx age was updated
end
@inline checkdirtysyms(x) = false

function add_nothing_if_no_syms(mx::Mxpr)
    if isempty(mx.syms) mx.syms[:nothing] = true end
end
checkemptysyms(x) = nothing

listsyms(mx::Mxpr) = sort!(collect(keys(mx.syms)))
listsyms(x) = nothing

#function setcleansyms(mx::Mxpr)
#    for (sym,age) in mx.syms
#end

@inline is_canon(mx::Mxpr) = mx.canon
@inline is_fixed(mx::Mxpr) = mx.fixed
is_fixed(s::SJSym) = symval(s) == s
#is_fixed{T}(s::SJSym{T}) = symval(s) == T
@inline setcanon(mx::Mxpr) = mx.canon = true
@inline setfixed(mx::Mxpr) = (mx.fixed = true; setage(mx))
@inline unsetcanon(mx::Mxpr) = mx.canon = false
@inline unsetfixed(mx::Mxpr) = mx.fixed = false

@inline is_canon(x) = false
@inline setcanon(x) = false
@inline unsetcanon(x) = false
#is_fixed(x) = false
#setfixed(x) = false
#unsetfixed(x) = false

# We need to think about copying in the following. Support both refs and copies ?
function getindex(mx::Mxpr, r::UnitRange)
    if r.start == 0
        return mxpr(mhead(mx),margs(mx)[1:r.stop]...)
    else
        return margs(mx)[r]
    end
end

function getindex(mx::Mxpr, r::StepRange)
    if r.start == 0
        return mxpr(mhead(mx),margs(mx)[0+r.step:r.step:r.stop]...)
    elseif r.stop == 0 && r.step < 0
        return mxpr(mx[r.start],margs(mx)[r.start-1:r.step:1]...,mhead(mx))
    else
        return margs(mx)[r]
    end
end

#downvalues(s::SJSym) = s.downvalues
downvalues(s::SJSym) = getssym(s).downvalues
listdownvalues(s::SJSym) = mxpr(:List,downvalues(s)...)

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

function createssym(s::Symbol,T::DataType)
    ns = ssjsym(s,T)
    SYMTAB[s] = ns
    return ns
end

function removesym(s::Symbol)
    delete!(SYMTAB,s)
    nothing
end

# refresh a copy that is not in the symbol table. how does this happen?
#getsym(sjs::SJSym) = getsym(symname(sjs))

#get_attribute(s::Symbol, a::Symbol) = get_attribute(getsym(s),a)
#set_attribute(s::Symbol, a::Symbol) = set_attribute(getsym(s),a)
#unset_attribute(s::Symbol, a::Symbol) = unset_attribute(getsym(s),a)

function protectedsymbols()
    args = newargs()
    for s in keys(SYMTAB)
        if get_attribute(s,:Protected) push!(args,getsym(s)) end
    end
    mx = mxpr(:List, sort!(args)...)
end

function usersymbols()
    args = newargs()
    for s in keys(SYMTAB)
        if ! get_attribute(s,:Protected) push!(args,getsym(s)) end
    end
    mx = mxpr(:List, sort!(args)...)
    setcanon(mx)
    setfixed(mx)
    mx
end


function get_attribute(sj::SJSym, a::Symbol)
    get(getssym(sj).attr,a,false)
end

function get_attribute{T}(mx::Mxpr{T}, a::Symbol)
    get_attribute(T,a)
end

function is_protected(sj::SJSym)
    get(getssym(sj).attr,:Protected,false)
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
    mxpr(mhead(mx),args)
end

typealias Orderless Union(Mxpr{:Plus},Mxpr{:Times})
typealias Blanks Union(Mxpr{:Blank},Mxpr{:BlankSequence},Mxpr{:BlankNullSequence})

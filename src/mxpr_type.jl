## Types SSJSym and Mxpr
# SSJSym are SJulia symbols
# Mxpr are SJulia expressions

## Counter for timestamps ("age") of instances of SSJSym and Mxpr
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
# There is only one element in val::Array{T,1}. It is much faster to set this
# value, than to set a field val::T.
abstract AbstractSJSym
type SSJSym{T}  <: AbstractSJSym
#    val::Any
    val::Array{T,1}
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
@inline ssjsym(s::Symbol) = SSJSym{Any}(Any[s],newattributes(),newdownvalues(),0)
#@inline symname{T}(s::SSJSym{T}) = T
# Hmm. Careful, this only is the name if the symbol evaluates to itself
@inline symname{T}(s::SSJSym{T}) = s.val[1]
## Typed SJ Symbols. Only experimental
ssjsym(s::Symbol,T::DataType) = SSJSym{T}(zero(T),newattributes(),newdownvalues(),0)
# intended to be used from within Julia, or quoted julia. not used anywhere in code
sjval(s::SJSym) = getssym(s).val[1]  
@inline symval(s::SJSym) = getssym(s).val[1]
@inline symval(s::SSJSym) = s.val[1]

@inline function setsymval(s::SJSym,val)
    getssym(s).val[1] = val
    getssym(s).age = increvalage()
end

@inline function setsymval(s::SSJSym,val)
    s.val[1] = val
    s.age = increvalage()
end

@inline function fastsetsymval(s::SJSym,val)
    getssym(s).val[1] = val
end

@inline function fastsetsymval(s::SSJSym,val)
    s.val[1] = val
end

# Any and all direct access to the val field in SSJSym occurs above this line.
# No other file accesses it directly.
###################################

@inline symname(s::SJSym) = s
#@inline symname(s::String) = symbol(s)
@inline symattr(s::SJSym) = getssym(s).attr
@inline getsym(s) = s  # careful, this is not getssym

# Try storing values in a Dict instead of a field. Not much difference.
# @inline symval(s::SJSym) = return haskey(SYMVALTAB,s) ? SYMVALTAB[s] : s
# @inline function setsymval(s::SJSym,val)
# #     (getssym(s).val = val)        
#      SYMVALTAB[s] = val
#      getssym(s).age = increvalage()
# end
# #@inline symval(s::SSJSym) = s.val

@inline symage(s::SJSym) = getssym(s).age
# Julia way is use getage for both SJSym and Mxpr
# But symage better signals intent.
#@inline getage(s::SJSym) = symage(s)  # should only use one of these

## symbol table for SJulia symbols
const SYMTAB = Dict{Symbol,SSJSym}()
const SYMVALTAB = Dict{Symbol,Any}()  # experiment with keep values elsewhere

import Base:  ==

function push_downvalue(ins::SJSym,val)
    s = getssym(ins)
    dv = s.downvalues
    isnewrule = true
    @inbounds for i in 1:length(dv)
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

##################################################################
# Mxpr                                                           #
# all SJulia expressions are represented by instances of Mxpr    #
##################################################################
typealias MxprArgs Array{Any,1}
typealias FreeSyms Dict{Symbol,Bool}


abstract AbstractMxpr
type Mxpr{T} <: AbstractMxpr
    head::SJSym
    args::MxprArgs
    fixed::Bool
    canon::Bool
    syms::FreeSyms
    age::UInt64
    key::UInt64
    typ::DataType
end
typealias Symbolic Union(Mxpr,SJSym)
newargs() = Array(Any,0)
newargs(n::Integer) = Array(Any,n)
newsymsdict() = FreeSyms() # Dict{Symbol,Bool}()  # create dict for field syms of Mxpr
mhead(mx::Mxpr) = mx.head
margs(mx::Mxpr) = mx.args
margs(mx::Mxpr,n::Int) = mx.args[n]  # need to get rid of this, use getindex, setindex
@inline setage(mx::Mxpr) = mx.age = increvalage()
@inline getage(mx::Mxpr) = mx.age

# These should be fast: In the SJulia language, mx[0] gets the head, but not here.
# TODO: iterator for mx that iterates over args would be useful
@inline setindex!(mx::Mxpr, val, k::Int) = (margs(mx)[k] = val)
@inline getindex(mx::Mxpr, k::Int) = margs(mx)[k]
@inline Base.length(mx::Mxpr) = length(margs(mx))
@inline Base.length(s::SJSym) = 0
@inline Base.endof(mx::Mxpr) = length(mx)
# Using this iterator is probably less efficient than iterating directly over the args
# Base.start(mx::Mxpr) = margs(mx)[1] # could use start here too
# Base.next(mx::Mxpr,state) = (state,next(state,margs(mx)))
# Base.done(mx::Mxpr,state) = done(margs(mx),state)
mxprtype{T}(mx::Mxpr{T}) = T

# Table for storing Mxpr indexed by hash code.
# Not using this at the moment.
const EXPRDICT = Dict{UInt64,Mxpr}()
global gotit = 0   # non constant global, only for testing

@inline function Base.copy(mx::Mxpr)
#    println("copying $mx")
    args = copy(mx.args)
    mxpr(mhead(mx),args)
end

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

# Input is Mxpr, output is the unique "copy" (can't really be a copy if it is unique)
# 1. Check if mx already has a hash key, then it is good one, return
# 2. Compute hash code of mx, look it up. Return unique copy, or make mx unique copy
#  if none exists.
# Slows down some code by factor of 2 to 5 or more or less
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

# Create a new Mxpr from list of args
function mxpr(s::SJSym,iargs...)
    args = newargs()
    for x in iargs push!(args,x) end
    mx = Mxpr{symname(s)}(s,args,false,false,newsymsdict(),0,0,Any)
    setage(mx)
#    checkhash(mx)
    mx
end

# Create a new Mxpr from Array of args
@inline function mxpr(s::SJSym,args::MxprArgs)
    mx =Mxpr{symname(s)}(s,args,false,false,newsymsdict(),0,0,Any)
    setage(mx)
#    checkhash(mx)    
    mx
end

# set fixed point and clean bits
# not used much
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

######  Manage lists of free symbols

# Copy list of free (bound to self) symbols in a to free symbols in mx.
@inline function mergesyms(mx::Mxpr, a::Mxpr)
    mxs = mx.syms
    for sym in keys(a.syms) # mxs is a Dict
        mxs[sym] = true
    end
    h = mhead(a)
    if ! is_protected(h)
        mxs[h] = true
    end
end

# Copy list of free (bound to self) symbols in a to a collection (Dict) of free symbols
function mergesyms(mxs::FreeSyms, a::Mxpr)
    for sym in keys(a.syms)
#        println("m1: $sym")
        mxs[sym] = true
    end
    h = mhead(a)
#    println("Checking head $h")    
    if ! is_protected(h)
        mxs[h] = true
    end
end

# Add Symbol a to list of free symbols syms
function mergesyms(syms::FreeSyms, a::SJSym)
#    println("m2: $a")    
    syms[a] = true
end

# Add Symbol a to list of free symbols in mx
@inline function mergesyms(mx::Mxpr, a::SJSym)
    (mx.syms)[a] = true    
end

mergesyms(x,y) = nothing

# Copy lists of free symbols in subexpressions of mx to
# list of free symbols of mx. Only descend one level.
# We also merge the head of mx. Maybe its better to
# separate this function. Maybe not.
function mergeargs(mx::Mxpr)
    h = mhead(mx)
    if ! is_protected(h)
        mergesyms(mx,h)
    end    
    @inbounds for i in 1:length(mx)
#        println("mergeargs $i: ", listsyms(mx[i]))
        mergesyms(mx,mx[i])
    end
end

mergeargs(x) = nothing

## clear list of free symbols in mx.
# is it cheaper to delete keys, or throw it away ?
function clearsyms(mx::Mxpr)
    mx.syms = newsymsdict()
end

# return true if a free symbol in mx has a more recent timestamp than mx
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


# If mx has an empty list of free symbols, put :nothing in the list.
# Prevents calling mergeargs if the list is empty. Eg when args to mx are numbers.
function add_nothing_if_no_syms(mx::Mxpr)
    if isempty(mx.syms) mx.syms[:nothing] = true end
end
checkemptysyms(x) = nothing

listsyms(mx::Mxpr) = sort!(collect(keys(mx.syms)))
listsyms(x) = nothing

## Check, set, and unset fixed point status and canonicalized status of Mxpr

@inline is_canon(mx::Mxpr) = mx.canon
@inline is_fixed(mx::Mxpr) = mx.fixed
is_fixed(s::SJSym) = symval(s) == s
#is_fixed{T}(s::SJSym{T}) = symval(s) == T
@inline setcanon(mx::Mxpr) = mx.canon = true
@inline setfixed(mx::Mxpr) = (mx.fixed = true; setage(mx))
@inline setfixed(x) = true
@inline unsetcanon(mx::Mxpr) = mx.canon = false
@inline unsetfixed(mx::Mxpr) = mx.fixed = false

@inline is_canon(x) = false
@inline setcanon(x) = false
@inline unsetcanon(x) = false
#is_fixed(x) = false
#setfixed(x) = false
unsetfixed(x) = false  # sometimes we have a Julia object

# We need to think about copying in the following. Support both refs and copies ?
# where is this used ?
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

# Return true if sj has attribute attr
function get_attribute(sj::SJSym, attr::Symbol)
    get(getssym(sj).attr,attr,false)
end

# Return true if head of mx has attribute attr
function get_attribute{T}(mx::Mxpr{T}, attr::Symbol)
    get_attribute(T,attr)
end

function is_protected(sj::SJSym)
    get(getssym(sj).attr,:Protected,false)
end

unprotect(sj::SJSym) = unset_attribute(sj,:Protected)
protect(sj::SJSym) = set_attribute(sj,:Protected)

function set_attribute(sj::SJSym, attr::Symbol)
    getssym(sj).attr[attr] = true
end

function unset_attribute(sj::SJSym, attr::Symbol)
    getssym(sj).attr[attr] = false
end


## Some types of Heads of Mxpr's

typealias Orderless Union(Mxpr{:Plus},Mxpr{:Times})
typealias Blanks Union(Mxpr{:Blank},Mxpr{:BlankSequence},Mxpr{:BlankNullSequence})

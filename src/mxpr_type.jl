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

typealias SJSymAttrs Dict{Symbol,Bool}
typealias SJSymDVs Array{Any,1}
typealias SJSymuVs Array{Any,1}
@inline newattributes() = SJSymAttrs()
@inline newdownvalues() = Array(Any,0)
@inline newupvalues() = Array(Any,0)

# TODO
#type DownValueT
#end

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
    attr::SJSymAttrs
    downvalues::SJSymDVs
    upvalues::SJSymDVs
    age::UInt64
end

# We have a choice to carry the symbol name in the type parameter or a a field,
# in which case the value of the symbol is typed
# Form of these functions depend on whether the symbol name is a type parameter
# or a field
@inline ssjsym(s::Symbol) = SSJSym{Any}(Any[s],newattributes(),newdownvalues(),newupvalues(),0)
#@inline symname{T}(s::SSJSym{T}) = T
# Hmm. Careful, this only is the name if the symbol evaluates to itself
@inline symname{T}(s::SSJSym{T}) = s.val[1]
## Typed SJ Symbols. Only experimental
@inline ssjsym(s::Symbol,T::DataType) = SSJSym{T}(zero(T),newattributes(),newdownvalues(),newupvalues(),0)
# intended to be used from within Julia, or quoted julia. not used anywhere in code
@inline sjval(s::SJSym) = getssym(s).val[1]  
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

# 
#symname(s::AbstractString) = symbol(s)

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

# This does not work. We need to compare things like
# HoldPattern(f(1.0)) HoldPattern(f(1))
# The best solution  is probably to make a hash key of the lhs's
# We need to use a type like DownValueT above
downvalue_lhs_equal(x,y) = x == y
downvalue_lhs_equal{T<:Number,V<:Number}(x::T,y::V) = x === y  #  f(1.0) is not f(1)

function push_downvalue(ins::SJSym,val)
    s = getssym(ins)
    dvs = s.downvalues
    isnewrule = true
    @inbounds for i in 1:length(dvs)
        if downvalue_lhs_equal(val[1], dvs[i][1])
            dvs[i] = val
            isnewrule = false
            break
        end
    end
    isnewrule && push!(s.downvalues,val)
    sort!(s.downvalues,lt=isless_patterns)
    s.age = increvalage()    
end

clear_downvalues(s::SJSym) = (getssym(s).downvalues = Array(Any,0))

@inline downvalues(s::SJSym) = getssym(s).downvalues
@inline listdownvalues(s::SJSym) = mxpr(:List,downvalues(s)...)

function push_upvalue(ins::SJSym,val)
    s = getssym(ins)
    uv = s.upvalues
    isnewrule = true
    @inbounds for i in 1:length(uv)
        if val[1] == uv[i][1] # need more sophistication than '=='
            uv[i] = val
            isnewrule = false
            break
        end
    end
    isnewrule && push!(s.upvalues,val)
#    sort!(s.downvalues,lt=isless_patterns)
    s.age = increvalage()    
end

@inline upvalues(s::SJSym) = getssym(s).upvalues
@inline listupvalues(s::SJSym) = mxpr(:List,upvalues(s)...)
@inline has_upvalues(s::SJSym) = length(upvalues(s)) > 0

## Retrieve or create new symbol
@inline function getssym(s::Symbol)
    if haskey(SYMTAB,s)
        return SYMTAB[s]
    else
        ns = ssjsym(s)
        SYMTAB[s] = ns
        # pollute Julia just so we get repl completion. remove this later.
        # Note, this may slow things like pattern matching. Because
        # Julia bindings of symbols are sometimes checked and then evaluated.
        # But, the pattern test file shows no difference in speed.
        # Remove this for two reasons
        # 1. We now use a module so Symbols are now SJulia.x and we don't get completion anyway (maybe could be fixed)
        # 2. We sometimes want to do a Julia binding of a symbol to a Julia function. But, this fails if it already
        #  is bound as a variable.
#        !isdefined(s) && eval(:($s = true))
        return ns
    end
end
@inline getssym(ss::String) = getssym(symbol(ss))

@inline function createssym(s::Symbol,T::DataType)
    ns = ssjsym(s,T)
    SYMTAB[s] = ns
    return ns
end

@inline function removesym(s::Symbol)
    delete!(SYMTAB,s)
    nothing
end

##################################################################
# Mxpr                                                           #
# all SJulia expressions are represented by instances of Mxpr    #
##################################################################
typealias MxprArgs Array{Any,1}
typealias FreeSyms Dict{Symbol,Bool}

# Creating an Array of these Dicts and using them is slower than
# Just creating them one at a time. So this is disabled.
# type Freesymsind
#     ind::Int
# end
# const Freesymspoolsize = 10^6
# const Freesymspool = Array(FreeSyms,Freesymspoolsize)
# const freesymsind = Freesymsind(Freesymspoolsize)
# function disable_newsymsdict()
#     if freesymsind.ind < Freesymspoolsize - 1
#         freesymsind.ind += 1
#         return Freesymspool[freesymsind.ind]
#     else
#         for i in 1:Freesymspoolsize
#             Freesymspool[i] = Dict{Symbol,Bool}()
#         end
#         freesymsind.ind = 1
#         return Freesymspool[freesymsind.ind]
#     end
# end


# This is T in Mxpr{T} for any Head that is not a Symbol
# We have duplicate information about the Head in a field, as well.
type GenHead
end

abstract AbstractMxpr
type Mxpr{T} <: AbstractMxpr
    head::Any  # making this Any instead of Symbol slows things a bit
    args::MxprArgs
    fixed::Bool
    canon::Bool
    syms::FreeSyms
    age::UInt64
    key::UInt64
    typ::DataType
end
typealias Symbolic Union(Mxpr,SJSym)
@inline newargs() = Array(Any,0)
@inline newargs(n::Integer) = Array(Any,n)
@inline newargs(m::Mxpr) = newargs(length(m))
@inline newargs(a::Array) = newargs(length(a))
@inline newsymsdict() = FreeSyms() # Dict{Symbol,Bool}()  # create dict for field syms of Mxpr
@inline mhead(mx::Mxpr) = mx.head
@inline margs(mx::Mxpr) = mx.args
@inline margs(mx::Mxpr,n::Int) = mx.args[n]  # need to get rid of this, use getindex, setindex

# Everything that is not an Mxpr
mhead(x) = typeof(x)
# This allows, in some cases, SJulia code to operate directly on a Dict.
# Eg, it works with Count.
# If we always access via iterators, then we don't need to 'collect' the values
# Probably not slower, either.
margs(d::Dict) = collect(values(d))

@inline setage(mx::Mxpr) = mx.age = increvalage()
@inline getage(mx::Mxpr) = mx.age
getfreesyms(mx::Mxpr) = mx.syms
setfreesyms(mx::Mxpr, syms::FreeSyms) = (mx.syms = syms)

# These should be fast: In the SJulia language, mx[0] gets the head, but not here.
# TODO: iterator for mx that iterates over args would be useful
@inline setindex!(mx::Mxpr, val, k::Int) = (margs(mx)[k] = val)
@inline getindex(mx::Mxpr, k::Int) = margs(mx)[k]
@inline Base.length(mx::Mxpr) = length(margs(mx))
@inline Base.length(s::SJSym) = 0
# We are claiming a lot of space here. But in SJulia,
# Most things should have length zero.
Base.length(x) = 0
@inline Base.endof(mx::Mxpr) = length(mx)
# Using this iterator is probably less efficient than iterating directly over the args
# Base.start(mx::Mxpr) = margs(mx)[1] # could use start here too
# Base.next(mx::Mxpr,state) = (state,next(state,margs(mx)))
# Base.done(mx::Mxpr,state) = done(margs(mx),state)
@inline mxprtype{T}(mx::Mxpr{T}) = T

# Table for storing Mxpr indexed by hash code.
# Not using this at the moment.
const EXPRDICT = Dict{UInt64,Mxpr}()
global gotit = 0   # non constant global, only for testing

@inline function Base.copy(mx::Mxpr)
#    println("copying $mx")
    args = copy(mx.args)
    mxpr(mhead(mx),args)
end

function Base.push!(mx::Mxpr,item)
    push!(margs(mx),item)
    mx
end

## This belongs more with SSJsym above, but Mxpr is not yet defined
@inline upvalues(m::Mxpr) = upvalues(mhead(m))
@inline downvalues(m::Mxpr) = downvalues(mhead(m))

# Allow any Head; Integer, anything.
@inline downvalues(x) = newdownvalues()

@inline function has_downvalues(mx::Mxpr)
    return ! isempty(downvalues(mhead(mx)))
end
@inline has_downvalues(x) = false

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

# We are not using this now. This is what Maple did when memory was scarce.
# But, Mma and Maple still say they compute a hash of everything.
# Input is Mxpr, output is the unique "copy" (can't really be a copy if it is unique)
# 1. Check if mx already has a hash key, then it is good one, return
# 2. Compute hash code of mx, look it up. Return unique copy, or make mx unique copy
#  if none exists.
#  Slows down some code by factor of 2 to 5 or more or less if we do it with all expressions
function checkhash(mx::Mxpr)
    mx.key != 0 && return mx
    k = hash(mx)
    if haskey(EXPRDICT,k)
        return EXPRDICT[k]
    end
    mx.key = k
    EXPRDICT[k] = mx
    mx
end
checkhash(x) = x

# function checknewhash(mx::Mxpr)
#     mx.key != 0 && return mx
#     k = hash(mx)
#     if haskey(EXPRDICT,k)
#         return EXPRDICT[k]
#     end
#     mx.key = k
#     EXPRDICT[k] = mx
#     mx
# end
# checknewhash(x) = (x,true)

##### Create Mxpr

# Create a new Mxpr from list of args
@inline function mxpr(s::SJSym,iargs...)
    n = length(iargs)
    args = newargs(n)
    for i in 1:n
        args[i] = iargs[i]
    end
    # args = newargs()  for reference. this appears to be no slower.
    # for x in iargs
    #     push!(args,x)
    # end
    return mxpr(s,args)
end

# Create a new Mxpr from Array of args
@inline function mxpr(s::SJSym,args::MxprArgs)
    mx = Mxpr{symname(s)}(s,args,false,false,newsymsdict(),0,0,Any)
    setage(mx)
#    checkhash(mx)    
    mx
end

# Non-symbolic Heads have type GenHead, for now
function mxpr(s,args::MxprArgs)
    mx = Mxpr{GenHead}(s,args,false,false,newsymsdict(),0,0,Any)
    setage(mx)
    mx
end

# Non-symbolic Heads have type GenHead, for now
function mxpr(s,iargs...)
    len = length(iargs)
    args = newargs(len)
    for i in 1:len
        args[i] = iargs[i]
    end
    mxpr(s,args)
end

# set fixed point and clean bits
# not used much
@inline function mxprcf(s::SJSym,iargs...)
    args = newargs()
    for x in iargs push!(args,x) end
    mxprcf(s,args)
end

@inline function mxprcf(s::SJSym,args::MxprArgs)
    mx = Mxpr{symname(s)}(s,args,true,true,newsymsdict(),0,0,Any)
#    checkhash(mx)
    mx
end

######  Manage lists of free symbols

# Sometimes protectd symbols need to be merged, somewhere.
#is_sym_mergeable(s) = ! is_protected(s)
@inline is_sym_mergeable(s) = true

# Copy list of free (bound to self) symbols in a to free symbols in mx.
@inline function mergesyms(mx::Mxpr, a::Mxpr)
    mxs = mx.syms
    for sym in keys(a.syms) # mxs is a Dict
        mxs[sym] = true
    end
    h = mhead(a)
    if is_sym_mergeable(h)
        mxs[h] = true
    end
end

# Copy list of free (bound to self) symbols in a to a collection (Dict) of free symbols
@inline function mergesyms(mxs::FreeSyms, a::Mxpr)
    for sym in keys(a.syms)
#        println("m1: $sym")
        mxs[sym] = true
    end
    h = mhead(a)
#    println("Checking head $h")    
    if is_sym_mergeable(h)
        mxs[h] = true
    end
end

# Add Symbol a to list of free symbols syms
@inline function mergesyms(syms::FreeSyms, a::SJSym)
#    println("m2: $a")    
    syms[a] = true
end

# Add Symbol a to list of free symbols in mx
@inline function mergesyms(mx::Mxpr, a::SJSym)
    (mx.syms)[a] = true    
end

@inline mergesyms(x,y) = nothing

# Copy lists of free symbols in subexpressions of mx to
# list of free symbols of mx. Only descend one level.
# We also merge the head of mx. Maybe its better to
# separate this function. Maybe not.
function mergeargs(mx::Mxpr)
    h = mhead(mx)
    if is_sym_mergeable(h)
        mergesyms(mx,h)
    end    
    @inbounds for i in 1:length(mx)
#        println("mergeargs $i: ", listsyms(mx[i]))
        mergesyms(mx,mx[i])
    end
end

@inline mergeargs(x) = nothing

## clear list of free symbols in mx.
# is it cheaper to delete keys, or throw it away ?
function clearsyms(mx::Mxpr)
    mx.syms = newsymsdict()
end

# return true if a free symbol in mx has a more recent timestamp than mx
@inline function checkdirtysyms(mx::Mxpr)
    length(mx.syms) == 0 && return true   # assume re-eval is necessary if there are no syms
    mxage = mx.age
#    if mxage > getevalage() # && return false # check if *any* user symbol has changed.
#        return false        # in test suite, this branch is never taken.
#    end
    for sym in keys(mx.syms) # is there a better data structure for this ?
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
@inline setcanon(mx::Mxpr) = (mx.canon = true; mx)
@inline setfixed(mx::Mxpr) = (mx.fixed = true; setage(mx); mx)
@inline setfixed(x) = x
@inline unsetcanon(mx::Mxpr) = (mx.canon = false; mx)
@inline unsetfixed(mx::Mxpr) = (mx.fixed = false ; mx)

@inline is_canon(x) = false
@inline setcanon(x) = false
@inline unsetcanon(x) = false
#is_fixed(x) = false
#setfixed(x) = false
@inline unsetfixed(x) = false  # sometimes we have a Julia object

# We need to think about copying in the following. Support both refs and copies ?
# where is this used ?
@inline function getindex(mx::Mxpr, r::UnitRange)
    if r.start == 0
        return mxpr(mhead(mx),margs(mx)[1:r.stop]...)
    else
        return margs(mx)[r]
    end
end

@inline function getindex(mx::Mxpr, r::StepRange)
    if r.start == 0
        return mxpr(mhead(mx),margs(mx)[0+r.step:r.step:r.stop]...)
    elseif r.stop == 0 && r.step < 0
        return mxpr(mx[r.start],margs(mx)[r.start-1:r.step:1]...,mhead(mx))
    else
        return margs(mx)[r]
    end
end

function protectedsymbols()
    args = newargs()
    for s in keys(SYMTAB)
        if get_attribute(s,:Protected) && s != :ans
            push!(args,getsym(s)) end
    end
    mx = mxpr(:List, sort!(args))
end

# Should we be constructing Mxpr's in this file ?
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


# For Heads that are not symbols
get_attribute(args...) = false

# Return true if sj has attribute attr
@inline function get_attribute(sj::SJSym, attr::Symbol)
    get(getssym(sj).attr,attr,false)
end

# Return true if head of mx has attribute attr
@inline function get_attribute{T}(mx::Mxpr{T}, attr::Symbol)
    get_attribute(T,attr)
end

@inline function is_protected(sj::SJSym)
    get(getssym(sj).attr,:Protected,false)
end

unprotect(sj::SJSym) = unset_attribute(sj,:Protected)
protect(sj::SJSym) = set_attribute(sj,:Protected)

@inline function set_attribute(sj::SJSym, attr::Symbol)
    getssym(sj).attr[attr] = true
end

@inline function unset_attribute(sj::SJSym, attr::Symbol)
    getssym(sj).attr[attr] = false
end


## Some types of Heads of Mxpr's

typealias Orderless Union(Mxpr{:Plus},Mxpr{:Times})
typealias Blanks Union(Mxpr{:Blank},Mxpr{:BlankSequence},Mxpr{:BlankNullSequence})

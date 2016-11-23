typealias SJSymbol Union{SJSym,Qsym}

# This is T in Mxpr{T} for any Head that is not a Symbol
# We have duplicate information about the Head in a field, as well.
type GenHead
end

# We have a choice to carry the symbol name in the type parameter or a a field,
# in which case the value of the symbol is typed
# Form of these functions depend on whether the symbol name is a type parameter
# or a field

@inline ssjsym(s::Symbol) = SSJSym{Any}(Any[s],newattributes(),newdownvalues(),newupvalues(),0,NullMxpr)

# Hmm. Careful, this only is the name if the symbol evaluates to itself

function symname{T}(s::SSJSym{T})
    s.val[1]
end

symname(s::SJSym) = s
symname(s::Qsym) = s.name

## Typed SJ Symbols. Only experimental

#@inline ssjsym{T<:DataType}(s::Symbol,dT::T) = SSJSym{dT}(zero(dT),newattributes(),newdownvalues(),newupvalues(),0,NullMxpr)
@inline ssjsym(s::Symbol,dT::DataType) = SSJSym{dT}(zero(dT),newattributes(),newdownvalues(),newupvalues(),0,NullMxpr)

# intended to be used from within Julia, or quoted julia. not used anywhere in code
@inline sjval(s::SJSym) = getssym(s).val[1]

# Don't make these one-line defintions. They are easier to search for this way.

"""
    symval(s::SJSym)

return the bound value of the `Symata` symbol with Julia-Symbol name `s`.
"""
function symval(s::SJSym)
    getssym(s).val[1]
end

function symval(s::Qsym)
    ssym = getssym(s)
    val = ssym.val[1]
    if val == s.name
        return s
    end
    val
end

doc"""
    symval(s::SSJSym)

Return the value bound to the Symata symbol `s`.
"""
function symval(s::SSJSym)
    s.val[1]
end

symval(x) = nothing  # maybe we should make this an error instead? We are using this method in exfunc.

## Sets an already existing Symata symbol

"""
    setsymval(s::SSJSym,val)

Set (bind) the Symata symbol `s` to `val`.
"""
function setsymval(s::SSJSym,val)
    s.val[1] = val
    s.age = increvalage()
end


"""
    setsymval(s::SJSymbol,val)

If `s` is `Symbol`, set the Symata symbol that the Julia symbol `s` is bound to to `val`.
Note `s` can also be a `Qsym`.
"""
function setsymval(s::SJSymbol,val)
    setsymval(getssym(s),val)
end

# function setsymval(qs::Qsym,val)
#     setsymval(getssym(qs),val)
# end

function set_system_symval(s::SJSym, val)
    setsymval(get_system_ssym(s),val)
end

fastsetsymval(s::SJSym,val) = (getssym(s).val[1] = val)

fastsetsymval(s::SSJSym,val) = (s.val[1] = val)

"""
    isbound(s::SJSym)

return true if `s` is unbound, that is, if evaluates to itself.
"""
isbound(s::SJSym) = (symval(s) != s)

function setdefinition(s::SSJSym, val::Mxpr)
    s.definition = val
end

getdefinition(s::SSJSym) = s.definition

function setdefinition(sym::SJSymbol, val::Mxpr)
    setdefinition(getssym(sym) , val)
end

getdefinition(sym::SJSymbol) = getdefinition(getssym(sym))

clear_ownvalue_definition(sym::SJSym) = (getssym(sym).definition = NullMxpr)

#############################################################################
# All direct access to the val field in SSJSym occurs above this line.
# No other file accesses it directly.
#############################################################################

#symname(s::AbstractString) = Symbol(s)

symattr(s::SJSymbol) = getssym(s).attr
# symattr(s::Qsym) = getssym(s).attr
# symattr(s::SJSym) = getssym(s).attr


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

import Base:  ==

# This does not work. We need to compare things like
# HoldPattern(f(1.0)) HoldPattern(f(1))
# The best solution  is probably to make a hash key of the lhs's
# We need to use a type like DownValueT above
downvalue_lhs_equal(x,y) = x == y
#downvalue_lhs_equal{T<:Number,V<:Number}(x::T,y::V) = x === y  #  f(1.0) is not f(1)
downvalue_lhs_equal(x::Number,y::Number) = x === y  #  f(1.0) is not f(1)

# Sets a downvalue associated with symbol

function set_downvalue(mx::Mxpr, s::SJSymbol, val)
    set_downvalue(mx, getssym(s), val)
end

# TODO: fix this. necessary for Derivative
#typealias SS Union{SSJSym,Mxpr{GenHead}}
typealias SS SSJSym
function set_downvalue(mx::Mxpr, s::SS, val)
#    s = getssym(ins)
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
    set_downvalue_def(val[1],mx)
    sort!(s.downvalues,lt=isless_patterns)
    s.age = increvalage()
end

function clear_downvalue_definitions(sym::SJSymbol)
    s = getssym(sym)
    dvs = s.downvalues
    for i in 1:length(dvs)
        lhs = dvs[i][1]
        delete_downvalue_def(lhs)
    end
end

function clear_downvalues(s::SJSym)
    clear_downvalue_definitions(s)
    getssym(s).downvalues = Array(Any,0)
end

downvalues(s::SJSymbol) = getssym(s).downvalues
sjlistdownvalues(s::SJSymbol) = mxpr(:List,downvalues(s)...)

function jlistdownvaluedefs(sym::SJSymbol)
    s = getssym(sym)
    dvs = s.downvalues
    dvlist = Array{Any,1}()
    for i in 1:length(dvs)
        lhs = dvs[i][1]
        mx = get_downvalue_def(lhs)
        mx != NullMxpr && push!(dvlist, mx)
    end
    dvlist
end

# FIXME: storing and printing definition is partly broken here.
function set_upvalue(mx, ins::SJSym,val)
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
    set_upvalue_def(val[1], mx)
    # How to sort upvalues ?
    s.age = increvalage()
end

@inline upvalues(s::SJSym) = getssym(s).upvalues
@inline sjlistupvalues(s::SJSym) = mxpr(:List,upvalues(s)...)
@inline has_upvalues(s::SJSym) = length(upvalues(s)) > 0

function clear_upvalue_definitions(sym::SJSym)
    s = getssym(sym)
    uvs = s.downvalues
    for i in 1:length(uvs)
        lhs = uvs[i][1]
        delete_upvalue_def(lhs)
    end
end

function clear_upvalues(s::SJSym)
    clear_upvalue_definitions(s)
    getssym(s).upvalues = Array(Any,0)
end

function jlistupvaluedefs(sym::SJSym)
    s = getssym(sym)
    uvs = s.upvalues
    uvlist = Array{Any,1}()
    for i in 1:length(uvs)
        lhs = uvs[i][1]
        mx = get_upvalue_def(lhs)
        mx != NullMxpr && push!(uvlist, get_upvalue_def(lhs))
    end
    uvlist
end

########################################################
## SJSymbol access
#######################################################

## Retrieve or create new symbol
function getssym(s::Symbol)
    if haskey(CurrentContext.symtab,s)
        return CurrentContext.symtab[s]
    else
        ns = ssjsym(s)
        CurrentContext.symtab[s] = ns
        return ns
    end
end

function getssym(qs::Qsym)
    res = getssym(qs.context, qs.name)
    if res == qs.name
        return qs
    end
    res
end

function getssym(context_name::Symbol, s::Symbol)
    symtab = get_context_symtab(context_name)
    if haskey(symtab,s)
        return symtab[s]
    else
        ns = ssjsym(s)
        symtab[s] = ns
        return ns
    end
end

get_system_ssym(s::Symbol) = getssym(:System, s)

getssym{T<:AbstractString}(ss::T) = getssym(Symbol(ss))

function delete_sym(s::Symbol)
    delete!(CurrentContext.symtab,s)
    nothing
end

function delete_sym(s::Qsym)
    delete!(get_context_symtab(s),symname(s))
    nothing
end

function delete_sym(s::AbstractString)
    delete_sym(Symbol(s))
end

##################################################################
# Mxpr                                                           #
# All Symata expressions are represented by instances of Mxpr    #
##################################################################

# The lines commented out make sense to me.
# But, tests fail when they are used
#function =={T<:Mxpr}(ax::T, bx::T)
#function =={T<:Mxpr, V<:Mxpr}(ax::T, bx::V)
function ==(ax::Mxpr, bx::Mxpr)
    mhead(ax) != mhead(bx)  && return false
    a = margs(ax)
    b = margs(bx)
    (na,nb) = (length(a),length(b))
    na != nb && return false
    @inbounds for i in 1:na
        a[i] != b[i] && return false
    end
    true
end

# =={T<:Mxpr, V<:Mxpr}(ax::T, bx::V) = false

typealias Symbolic Union{Mxpr,SJSym}

"""
    newargs()

return an empty container to hold the arguments in an `Mxpr`. This is
currently `Array{Any,1}`.
"""
@inline newargs() = Array(Any,0)

"""
    newargs(n::Integer)

return a container with n elements to hold arguments for an `Mxpr`.
"""
@inline newargs(n::Integer) = Array(Any,n)

"""
    newargs(m::Mxpr)

return a container with `length(m)` elements to hold arguments for an `Mxpr`.
"""
@inline newargs(m::Mxpr) = newargs(length(m))

@inline newargs(a::Array) = newargs(length(a))


# is this just convenient ?
function tomxprargs(args...)
     nargs = MxprArgType[args...]
end

function tomxprargs(args::Array)
     nargs = MxprArgType[args...]
end

@inline newsymsdict() = FreeSyms() # Dict{Symbol,Bool}()  # create dict for field syms of Mxpr

# FIXME. I don't think we need the template here

"""
    mhead(mx::Mxpr)

return the `Head` of `mx.
"""
mhead(mx::Mxpr) = mx.head

"""
    margs(mx::Mxpr)

return the `Array` of arguments `mx`.
"""
margs(mx::Mxpr) = mx.args

# Everything that is not an Mxpr
mhead(x) = typeof(x)
# This allows, in some cases, Symata code to operate directly on a Dict.
# Eg, it works with Count.
# If we always access via iterators, then we don't need to 'collect' the values
# Probably not slower, either.
margs(d::Dict) = collect(values(d))

# The following makes sense for Mxpr{:GenHead}. It might be useful in this case. But, probably not.
# """
#     setmhead(mx::Mxpr,val)
# set the `Head` of `mx` to `val`. This function is not
# worth much because the head information is also stored in the type.
# """
# setmhead(mx::Mxpr,val) = (mx.head = val)

@inline setage(mx::Mxpr) = mx.age = increvalage()
@inline getage(mx::Mxpr) = mx.age
getfreesyms(mx::Mxpr) = mx.syms
setfreesyms(mx::Mxpr, syms::FreeSyms) = (mx.syms = syms)

# We need to think about copying in the following. Support both refs and copies ?

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

"""
     getpart(mx::Mxpr,ind1::Integer,ind2::Integer,...)

return part of expression `mx`. `ind1,...` indices into `mx`.
An index value `0` refers to the head of `mx` or a subexpression, that is the part returned by `mhead`.
Index values greater than `0` refer to elements.

Note that `0` only makes sense as the last index.

Negative indices are not supported here. That is, we assume they have already been converted to positive
"""
getpart(mx::Mxpr,ind) = (ind == 0 ? mhead(mx) : margs(mx)[ind])
getpart(mx::Mxpr, ind1, ind2) = getpart(getpart(mx,ind1),ind2)
getpart(mx::Mxpr, ind1, ind2, inds...) = getpart(getpart(getpart(mx,ind1),ind2), inds...)


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

##### Create Mxpr

# Create a new Mxpr from list of args
## We create Mxpr in two different ways.
## One is to supply the array of args.
## Two is to supply arguments one by one on the command line
## All the code (Nov 2016) uses one function mxpr for both of these
## methods. We want to change this and use mxpra for case one
## and mxpr for case two.

function mxpr(s::SJSym,iargs...)
    args = newargs(length(iargs))
    copy!(args,iargs)
    return mxpr(s,args)
end

## MxprArgs is just an alias to array of Any
## we should call have different functions mxpr and mxpra for the two cases
## 1) we supply a list of args as arguments 2) we supply the entire array
# Create a new Mxpr from Array of args
@inline function mxpr(s::SJSym,args::MxprArgs)
    mx = Mxpr{symname(s)}(s,args,false,false,newsymsdict(),0,0,Any)
    setage(mx)
    mx
end

## Prefer this one. We may want to use an array of type Any as an mxpr argument.
## Not possible if we dispatch on type to distinguis from mxpr() above
@inline function mxpra(s::SJSym,args::MxprArgs)
    mx = Mxpr{symname(s)}(s,args,false,false,newsymsdict(),0,0,Any)
    setage(mx)
    mx
end

function mxpra(s,args::MxprArgs)
    mx = Mxpr{GenHead}(s,args,false,false,newsymsdict(),0,0,Any)
    setage(mx)
    mx
end

## We can't change the head of an expression (except for genhead). The head is also the type.
## Next best thing is create a new Mxpr and use the fields from the old Mxpr without copying
function mxprnewhead(mx::Mxpr,head::SJSym)
    mx = Mxpr{head}(head,mx.args,mx.fixed,mx.canon,mx.syms,mx.age,mx.key,mx.typ)
end

# nonsymbolic head
function mxprnewhead(mx::Mxpr,head)
    mx = Mxpr{GenHead}(head,mx.args,mx.fixed,mx.canon,mx.syms,mx.age,mx.key,mx.typ)
    setage(mx)
    mx
end

function mxprnewhead(mx::Mxpr{GenHead},head::SJSym)
    mx = Mxpr{head}(head,mx.args,mx.fixed,mx.canon,mx.syms,mx.age,mx.key,mx.typ)
    setage(mx)
    mx
end

## For GenHead to GenHead we don't need to copy
function mxprnewhead(mx::Mxpr{GenHead},head)
    mx.head = head
    setage(mx)
    mx
end

# New method May 2016. We do want to use Mxpr's as heads
# We disable this for now. A number of expressions are interpreted incorrectly this way.
# eg  s = [f,g]
#   s[1](x) , should give f(x). But it is instead caught by this method.
# I don't know how s[1](x) is handled, then !?
# function mxpr(mxhead::Mxpr,args...)
#     println("mxpr is  head method")
#     nargs = Any[args...]
#     mx = Mxpr{:Mxpr}(mxhead,nargs,false,false,newsymsdict(),0,0,Any)
#     setage(mx)
#     mx
# end

# # New method May 2016
# function mxpr(mxhead::Mxpr,args::MxprArgs)
#     println("mxpr is  head method, with args mxhead is ", mxhead, ", args are ", args)
#     mx = Mxpr{:Mxpr}(mxhead,args,false,false,newsymsdict(),0,0,Any)
#     setage(mx)
#     mx
# end

# Non-symbolic Heads have type GenHead, for now
function mxpr(s,args::MxprArgs)
    mx = Mxpr{GenHead}(s,args,false,false,newsymsdict(),0,0,Any)
    setage(mx)
    mx
end

function mxpr(s,iargs...)
    len = length(iargs)
    args = newargs(len)
    copy!(args,iargs)
    # for i in 1:len
    #     args[i] = iargs[i]
    # end
    mxpra(s,args)
end

# set fixed point and clean bits
@inline function mxprcf(s::SJSym,iargs...)
    args = newargs(length(iargs))
    copy!(args,iargs)
    mxprcf(s,args)
end

@inline function mxprcf(s::SJSym,args::MxprArgs)
    mx = Mxpr{symname(s)}(s,args,true,true,newsymsdict(),0,0,Any)
#    checkhash(mx)
    mx
end

function mxprcf(s,args::MxprArgs)
    mx = Mxpr{GenHead}(s,args,true,true,newsymsdict(),0,0,Any)
    setage(mx)
    mx
end


######  Manage lists of free symbols

# Sometimes protected symbols need to be merged, somewhere.
#is_sym_mergeable(s) = ! is_protected(s)

is_sym_mergeable(s::Symbol) = true
# Don't put GenHead into a dict of symbols. Its not a symbol.
is_sym_mergeable(s) = false

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
        mxs[sym] = true
    end
    h = mhead(a)
    if is_sym_mergeable(h)
        mxs[h] = true
    end
end

# Add Symbol a to list of free symbols syms
@inline function mergesyms(syms::FreeSyms, a::SJSym)
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
    foreach(x -> mergesyms(mx,x), mx)
    # @inbounds for i in 1:length(mx)
    #     mergesyms(mx,mx[i])
    # end
end

@inline mergeargs(x) = nothing

## clear list of free symbols in mx.
# is it cheaper to delete keys, or throw it away ?
function clearsyms(mx::Mxpr)
    mx.syms = newsymsdict()
end

# return true if a free symbol in mx has a more recent timestamp than mx
@inline function checkdirtysyms(mx::Mxpr)
#    length(mx.syms) == 0 && return true   # assume re-eval is necessary if there are no syms
    isempty(mx.syms) && return true   # assume re-eval is necessary if there are no syms
    mxage = mx.age
    for sym in keys(mx.syms) # is there a better data structure for this ?
        symage(sym) > mxage && return true
    end
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
is_fixed(s::SJSym) = symval(s) == s  # unbound is a better work, maybe. Where is this used ?? see checkunbound in apprules.jl
@inline setcanon(mx::Mxpr) = (mx.canon = true; mx)
@inline setfixed(mx::Mxpr) = (mx.fixed = true; setage(mx); mx)
@inline setfixed(x) = x
@inline unsetcanon(mx::Mxpr) = (mx.canon = false; mx)
@inline unsetfixed(mx::Mxpr) = (mx.fixed = false ; mx)

debug_is_fixed(mx) = is_fixed(mx) ? symprintln(mx, " *is* fixed") : symprintln(mx, " is *not* fixed")

deepsetfixed(x) = x
function deepsetfixed{T<:Mxpr}(mx::T)
    nargs = newargs(mx)
    for i in 1:length(nargs)
        nargs[i] = deepsetfixed(mx[i])
    end
    return mxprcf(mhead(mx), nargs)
end

deepunsetfixed(x) = x
function deepunsetfixed{T<:Mxpr}(mx::T)
    nargs = newargs(mx)
    map!(deepunsetfixed,nargs,margs(mx))
    unsetfixed(mx)
end

@inline is_canon(x) = false
@inline setcanon(x) = false
@inline unsetcanon(x) = false
#unsetfixed(x) = false  # sometimes we have a Julia object
unsetfixed(x) = x  # This behavior is more useful


function protectedsymbols_strings()
    symstrings = Array(Compat.String,0)
    for s in keys(CurrentContext.symtab)
        if get_attribute(s,:Protected) && s != :ans
            push!(symstrings,string(getsym(s))) end
    end
    sort!(symstrings)
end

function protectedsymbols()
    args = newargs()
    for s in keys(CurrentContext.symtab)
        if get_attribute(s,:Protected) && s != :ans
            push!(args,getsym(s)) end
    end
    mx = mxpra(:List, sort!(args))
end

function usersymbols()
    nargs = newargs()
    for k in keys(get_context_symtab(:Main))
        if ! haskey(get_context_symtab(:System),k)
            push!(nargs,string(k))
        end
    end
    return nargs
end

# This is the new version used by UserSyms()
# Experiment with namespaces
function usersymbolsList()
        mxpra(:List, usersymbols())
end

# This is the old versions
# For now, we exclude Temporary symbols
# We return symbols as strings to avoid infinite eval loops
# function usersymbolsListold()
#     args = newargs()
#     for s in keys(CurrentContext.symtab)
#         if  get_attribute(s,:Temporary) continue end
#         if ! haskey(system_symbols, s) push!(args,string(getsym(s))) end
#     end
#     mx = mxpr(:List, sort!(args)...)
#     setcanon(mx)
#     setfixed(mx)
#     mx
# end

# function usersymbolsold()
#     args = newargs()
#     for s in keys(CurrentContext.symtab)
#         if  get_attribute(s,:Temporary) continue end  # This does not help.
#         if ! haskey(system_symbols, s) push!(args,string(getsym(s))) end
#     end
#     return args
# end

# For Heads that are not symbols
get_attribute(args...) = false

# Return true if sj has attribute attr
function get_attribute(sj::SJSymbol, attr::Symbol)
    get(getssym(sj).attr,attr,false)
end

# function get_attribute(sj::Qsym, attr::Symbol)
#     get(getssym(sj).attr,attr,false)
# end

# Return true if head of mx has attribute attr
function get_attribute{T}(mx::Mxpr{T}, attr::Symbol)
    get_attribute(T,attr)
end

for s in (:HoldFirst,:HoldAll,:HoldRest,:HoldAllComplete, :SequenceHold, :Flat, :Listable)
    sf = Symbol("has",s)
    @eval ($sf)(x) = get_attribute(x,$(QuoteNode(s)))
end


# Related code in predicates.jl and attributes.jl
unprotect(sj::SJSym) = unset_attribute(sj,:Protected)
protect(sj::SJSym) = set_attribute(sj,:Protected)
set_attribute(sj::SJSymbol, attr::Symbol) = (getssym(sj).attr[attr] = true)

# Better to delete the symbol
#unset_attribute(sj::SJSym, attr::Symbol) = (getssym(sj).attr[attr] = false)

unset_attribute(sj::SJSymbol, attr::Symbol) = delete!(getssym(sj).attr, attr)

clear_attributes(sj::SJSymbol) =  empty!(getssym(sj).attr)

#### typealiases

typealias Orderless Union{Mxpr{:Plus},Mxpr{:Times}}

# Everything except Bool is what we want
# Maybe we actually need a separate Bool from julia
typealias SJReal Union{AbstractFloat, Irrational, Rational{Integer},BigInt,Signed, Unsigned}

# used in predicates.jl
typealias BlankXXX Union{Mxpr{:Blank},Mxpr{:BlankSequence},Mxpr{:BlankNullSequence}}

# used in apprules.jl
typealias Rules Union{Mxpr{:Rule},Mxpr{:RuleDelayed}}  ## find uses of this and remove them
typealias RulesT Union{Mxpr{:Rule},Mxpr{:RuleDelayed}}

# used in expressions.jl
typealias Holds Union{Mxpr{:Hold}, Mxpr{:HoldForm}, Mxpr{:HoldPattern}, Mxpr{:HoldComplete}}

# used in sortorderless.jl
typealias ExpNoCanon Union{SJSym,Number}

# used in flatten.jl
typealias FlatT Union{Mxpr{:Plus},Mxpr{:Times},Mxpr{:And},Mxpr{:Or}, Mxpr{:LCM}, Mxpr{:GCD} }

## Alias types for convenience and clarity.
for s in (:List, :Power, :Times, :Plus, :Rule)
    @eval typealias $(Symbol(s,"T")) Mxpr{$(QuoteNode(s))}
end

## For many jula functions that take real or complex floating point args.
## Complex{AbstractFloat} does not do what we want. The inner type must be concrete
typealias FloatRC  Union{AbstractFloat, Complex{AbstractFloat},Complex{Float64}}

### Iterator

# moved these into the loop below
# Base.start(mx::Mxpr) = start(margs(mx))
# Base.next(mx::Mxpr,state) = next(margs(mx),state)
# Base.done(mx::Mxpr,state) = done(margs(mx),state)

####

for s in (:(Base.pop!), :(Base.shift!), :(Base.unshift!), :(Base.push!), :(Base.start), :(Base.next), :(Base.done))
    @eval ($s)(mx::Mxpr,args...) = ($s)(margs(mx),args...)
end


# These should be fast: In the Symata language, mx[0] gets the head, but not here.
setindex!(mx::Mxpr, val, k::Integer) = (margs(mx)[k] = val)
## getindex(mx::Mxpr,ind) with single index does not check for index 0 for efficiency.
## Also the iterator over mx relies on this behavior.
## But, with more than one index, getindex checks for index of zero (heads)
@inline getindex(mx::Mxpr, k::Integer) = margs(mx)[k]
getindex(mx::Mxpr, inds...) = getpart(mx,inds...)

#@inline Base.endof(mx::Mxpr) = length(mx)
@inline Base.endof(mx::Mxpr) = endof(margs(mx))
@inline Base.copy(mx::Mxpr) = mxpra(mhead(mx),copy(margs(mx)))

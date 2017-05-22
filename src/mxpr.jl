############################################################
#                                                          #
#  The top part of this file contains system state. There  #
#  is more in kernelstate.jl.                              #
#                                                          #
############################################################

"""
    SJDOCS

is the dictionary containing documentation for Symata symbols.
"""
const SJDOCS = Dict{Symbol,Any}()

"""
    @sjdoc sym::Symbol str::String

associate Symata langague documentation `str` with `sym`.
"""
macro sjdoc(sym,str)
    SJDOCS[sym] = Markdown.parse(str)
    nothing
end

function sjdocfun(sym,str)
    SJDOCS[sym] = Markdown.parse(str)
    nothing
end

## Types SSJSym and Mxpr
# SSJSym are Symata symbols
# Mxpr are Symata expressions

const MxprArgs = Array{Any,1}
const MxprArgType = Any
const MxprArgT  = Any
const FreeSyms = Dict{Symbol,Bool}
const SymString  = Union{Symbol,String}

"""
    UnitRangeInf

like `UnitRange`, but the upper limit is "infinity"
"""
immutable UnitRangeInf{T<:Real}  <: AbstractUnitRange{T}
    start::T
end


# AbstractMxpr is not used for anything
# abstract AbstractMxpr
# type Mxpr{T} <: AbstractMxpr

## Help for builtin types print documentation for fields, or at least lists them.
## How can we do this ?

"""
    Mxpr{T}

is the type representing Symata expressions. If `T` is a symbol it is also the head of the Symata
expression. Otherwise, `T` is the type `GenHead`. In any case, the head is stored in the field `head`.
"""

### NOTE: Julia apparently allows any object for T. But, maybe we still need GenHead

type Mxpr{T}
    """
       head
    the `head` of the Symata expression.
    """
    head::Any  # making this Any instead of Symbol slows things a bit. hmmm. we should subtype
    args::MxprArgT
    fixed::Bool
    canon::Bool
    syms::FreeSyms
    age::UInt64
    key::UInt64
    typ::DataType
end

#Mxpr{T}(args...) = Mxpr{T,Any}(args...)

const SJSymAttrs = Dict{Symbol,Bool}
const SJSymDVs =  Array{Any,1}
const SJSymuVs =  Array{Any,1}
@inline newattributes() = SJSymAttrs()
@inline newdownvalues() = Array{Any}(0)
@inline newupvalues() = Array{Any}(0)

# Almost all symbols use Any for parameter T.  We experimented a bit
# with a value of Int for some symbols It may be better to have no
# parameter, or that it means something else.
#
# Note that Mma will create a packed array of Ints with Range[n]. Upon changing setting an element to
# a value of a different type, an Any array is created and the array of Ints is copied. This can be
# seen by timing these operations with a large value such as n=10^6
#
# The name of the Symata symbol is a Symbol. The symbol table maps
# Symbol to SSJSym.  There is only one element in val::Array{T,1}. It
# is much faster to set this value, than to set a field val::T.
#
# If the value was set with SetDelayed, we set the delayed bit.  But,
# this is only used, so far, in printing definitions to be read
# later. Since a := b is probably far less common than a = b, I guess
# that it is better to store the delayed bit in a Dict, But, this
# would have to be profiled.
#
## It will almost certainly be more efficient for most symbols to store up/downvalues, attributes, and definition
## In an external structure. Or, to somehow leave these them unallocated till needed.
## For symbols with downvalues, getting a field might be faster than looking up in a table.
@compat abstract type AbstractSJSym end

type SSJSym{T}  <: AbstractSJSym
    val::Array{T,1}
    attr::SJSymAttrs
    downvalues::SJSymDVs
    upvalues::SJSymDVs
    age::UInt64
    definition::Mxpr
end

immutable Qsym
    context::Symbol
    name::Symbol
end

########################################################################
# SSJSym                                                               #
# data associated with Symata symbols are instances of type SSJSym     #
# SJSym is just Symbol. It is an older abstraction. Maybe we need it ! #
########################################################################
const SJSym = Symbol

#### Symbol Table

## symbol table for Symata symbols

const SymTab = Dict{Symbol,SSJSym}

newsymtable() = SymTab()

#const SYMTAB = newsymtable()

const SYMTABLES = Dict{Symbol,SymTab}()
SYMTABLES[:System] = newsymtable()
SYMTABLES[:Main] = newsymtable()

function getcontexts()
    sort!(map(string, collect(keys(SYMTABLES))))
end

function getsymbolsincontext(s)
    c = Symbol(s)
    ! haskey(SYMTABLES,c) && error("No context named ", s)
    sort!(map(string, collect(keys(SYMTABLES[c]))))
end

function  get_context_symtab(context_name::SymString)
    cn = Symbol(context_name)
    if ! haskey(SYMTABLES, cn)
        SYMTABLES[cn] = newsymtable()
    end
    SYMTABLES[cn]
end

function get_context_symtab(s::Qsym)
    get_context_symtab(s.context)
end

type CurrentContextT
    name::Symbol
    symtab::SymTab
end

function CurrentContextT(name::Symbol)
    CurrentContextT(name, get_context_symtab(name))
end

const CurrentContext = CurrentContextT(:System)

function get_current_context_name()
    CurrentContext.name
end

function set_current_context(name::Symbol)
    CurrentContext.name = name
    CurrentContext.symtab = get_context_symtab(name)
#    println("Set current context to $name")
end

function sjimportall(src::Symbol, targ::Symbol)
    tc = get_context_symtab(targ)
    for (k,v) in get_context_symtab(src)
        tc[k] = v
    end
    nothing
end

#### Evalage

## Counter for timestamps ("age") of instances of SSJSym and Mxpr
type Evalage
    t::UInt64
end
const evalage = Evalage(0)

increvalage() = evalage.t += 1
getevalage() = evalage.t

#### System Symbols

# These are more or less the "builtin" symbols
# After filling the Dict, its contents should be static.
## TODO: make use of this
## This dict is independent of the attempt at using Contexts below.
const system_symbols = Dict{Symbol,Bool}()

register_system_symbol(s::SymString) =  system_symbols[Symbol(s)] = true
is_system_symbol(s::SymString) =  system_symbols[Symbol(s)]

get_system_symbols() = sort!(collect(keys(system_symbols)))

#### Down values

# We store the Mxpr definition to define downvalues. These
# can be written to a file.
const DOWNVALUEDEFDICT = Dict{Any,Any}()

#get_downvalue_def(lhs) = getkey(DOWNVALUEDEFDICT, lhs, NullMxpr)
get_downvalue_def(lhs) = get(DOWNVALUEDEFDICT, lhs, NullMxpr)
set_downvalue_def(lhs,rhs) = (DOWNVALUEDEFDICT[lhs] = rhs)
delete_downvalue_def(lhs) =  haskey(DOWNVALUEDEFDICT,lhs) ? delete!(DOWNVALUEDEFDICT,lhs) : nothing

#### Up values

const UPVALUEDEFDICT = Dict{Any,Any}()
#get_upvalue_def(lhs) = getkey(UPVALUEDEFDICT[lhs], lhs, NullMxpr) # Wrong. below is correct
get_upvalue_def(lhs) = getkey(UPVALUEDEFDICT, lhs, NullMxpr)
set_upvalue_def(lhs,rhs) = (UPVALUEDEFDICT[lhs] = rhs)
delete_upvalue_def(lhs) =  haskey(UPVALUEDEFDICT,lhs) ? delete!(UPVALUEDEFDICT,lhs) : nothing

#### For storing hashed Mxpr. But, we do not use this now.
const EXPRDICT = Dict{UInt64,Mxpr}()

# Creating an Array of these Dicts and using them is slower than
# Just creating them one at a time. So this is disabled.
# type Freesymsind
#     ind::Int
# end
# const Freesymspoolsize = 10^6
# const Freesymspool = Array{FreeSyms}(Freesymspoolsize)
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

## FWIF, there is no `const` data below this line.

###################################################################
###################################################################

const SJSymbol = Union{SJSym,Qsym}

"""
    GenHead

"Generic" head. The parameter `x` in any `Mxpr{x}` is either a symbol or `GenHead`.
The actual head is stored in a field of the `Mxpr`.
"""
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
# @inline sjval(s::SJSym) = getssym(s).val[1]

"""
   getsymata(s::Symbol)

gets the value that `s` is bound to in Symata.
"""
getsymata(args...) = symval(args...)

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

symval(x) = nothing  # maybe we should make this an error instead? We are using this method in symataevaluate.

## Sets an already existing Symata symbol
"""
   setsymata(s::Symbol, val)

binds `val` to `s` in Symata.
"""
setsymata(args...) = setsymval(args...)

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

"""
    set_downvalue(mx::Mxpr, s::SJSymbol, val)

sets a downvalue associated with `s` to `val`.
"""
function set_downvalue(mx::Mxpr, s::SJSymbol, val)
    set_downvalue(mx, getssym(s), val)
end

# TODO: fix this. necessary for Derivative
#typealias SS Union{SSJSym,Mxpr{GenHead}}
const SS = SSJSym
function set_downvalue(mx::Mxpr, s::SS, val)
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
    getssym(s).downvalues = Array{Any}(0)
end

downvalues(s::SJSymbol) = getssym(s).downvalues

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
@inline has_upvalues(s::SJSym) = length(upvalues(s)) > 0

function clear_upvalue_definitions(sym::SJSym)
    s = getssym(sym)
    uvs = s.upvalues
    for i in 1:length(uvs)
        lhs = uvs[i][1]
        delete_upvalue_def(lhs)
    end
end

function clear_upvalues(s::SJSym)
    clear_upvalue_definitions(s)
    getssym(s).upvalues = Array{Any}(0)
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

const Symbolic = Union{Mxpr,SJSym}

"""
    newargs()

return an empty container to hold the arguments in an `Mxpr`. This is
currently `Array{Any,1}`.
"""
@inline newargs() = Array{MxprArgType}(0)

"""
    newargs(n::Integer)

return a container with n elements to hold arguments for an `Mxpr`.
"""
@inline newargs(n::Integer) = Array{Any}(n)

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

"""
    mhead(mx::Mxpr)

return the `Head` of `mx`.
"""
mhead(mx::Mxpr) = mx.head

"""
   a = margs(mx::Mxpr)

return the arguments `mx`. The arguments `a` are of type `MxprArgType`,
which is an alias for `Array{Any,1}`.
Many methods for `Mxpr` simply call the same method on `a`. For instance the
iterator for `Mxpr` wraps the iterator for `Array{Any,1}`. Thus, iterating over
`mx` iterates over the arguments only, not the head.
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

"""
    mxpr(head,iargs...)

create a Symata expression with head `head` and arguments `iargs`.
The arguments are copied. An object of type `Mxpr{head}` is returned
if `head` is a symbol, and of type `Mxpr{GenHead}` otherwise.
"""
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

"""
    mxpra(head,args::MxprArgs)

create a Symata expression with head `head` and arguments `args`.
The arguments are not copied. An object of type `Mxpr{head}` is returned
if `head` is a symbol, and of type `Mxpr{GenHead}` otherwise. The type `MxprArgs`
is currently an alias for `Array{Any,1}` and can be instantiated with the function
`newargs()`. This alias is unlikely to change, but til now, this abstraction has been
maintained.
"""
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
end

## TODO: prefer mxprcfa to mxcprcf if we are not copying args.
## TODO: when does clean bit matter ? Is fixed bit enough.
##  in mxpr_utils, tolistfixed, etc. only set fixed.
@inline function mxprcfa(s::SJSym,args::MxprArgs)
    mx = Mxpr{symname(s)}(s,args,true,true,newsymsdict(),0,0,Any)
end

function mxprcf(s,args::MxprArgs)
    mx = Mxpr{GenHead}(s,args,true,true,newsymsdict(),0,0,Any)
    setage(mx)
    mx
end


######  Manage lists of free symbols

# Sometimes protected symbols need to be merged, somewhere.
#is_sym_mergeable(s) = ! hasProtected(s)

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


"""
    mergesyms(syms::FreeSyms, a::SJSym)

adds `a` to list of free symbols `syms`
"""
@inline mergesyms(syms::FreeSyms, a::SJSym) =  syms[a] = true

"""
    mergesyms(mx::Mxpr, a::SJSym)

adds `a` to list of free symbols in `mx`.
"""
@inline mergesyms(mx::Mxpr, a::SJSym) = (mx.syms)[a] = true

"""
    mergesyms(x,y)

does nothing and returns `nothing` if `x` is neither a `Mxpr` nor a `FreeSyms`.
"""
@inline mergesyms(x,y) = nothing


# We also merge the head of mx. Maybe its better to
# do merging the head  in a separate function.

"""
    mergeargs(mx::Mxpr)

copies lists of free symbols in subexpressions of `mx` to
the list of free symbols of `mx`.

`mergeargs` descends only one level.
Also merge the head of `mx`.
"""
function mergeargs(mx::Mxpr)
    h = mhead(mx)
    if is_sym_mergeable(h)  mergesyms(mx,h)  end
    foreach(x -> mergesyms(mx,x), mx)
end

"""
    mergeargs(x)

returns `nothing` if `x` is not a `Mxpr`.
"""
@inline mergeargs(x) = nothing

## clear list of free symbols in mx.
"""
    clearsyms(mx::Mxpr)

removes all symbols from the list of free symbols in `mx`.
"""
clearsyms(mx::Mxpr) =  empty!(mx.syms)

## TODO: we have no unit tests (or close to unit tests) for checkdirtysyms and related functions
"""
    checkdirtysyms(mx::Mxpr)

returns `true` if any free symbol in `mx` has a more recent timestamp than `mx`, and `false` otherwise.
"""
@inline function checkdirtysyms(mx::Mxpr)
    isempty(mx.syms) && return true   # assume re-eval is necessary if there are no syms
    mxage = mx.age
    for sym in keys(mx.syms) # is there a better data structure for this ?
        symage(sym) > mxage && return true
    end
    return false  # no symbols in mx have been set since mx age was updated
end

"""
    checkdirtysyms(x)

returns `false` if `x` is not a `Mxpr`.
"""
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
is_fixed(s::SJSym) = symval(s) == s  # unbound is a better word, maybe. Where is this used ?? see checkunbound in apprules.jl
@inline setcanon(mx::Mxpr) = (mx.canon = true; mx)

@inline unsetcanon(mx::Mxpr) = (mx.canon = false; mx)

"""
    setfixed(mx::Mxpr)

sets the fixed bit on `mx`.
"""
@inline setfixed(mx::Mxpr) = (mx.fixed = true; setage(mx); mx)

"""
    setfixed(x)

is the identity if `x` is not a `Mxpr`.
"""
@inline setfixed(x) = x

"""
    unsetfixed(mx::Mxpr)

unsets the fixed bit on `mx`.
"""
@inline unsetfixed(mx::Mxpr) = (mx.fixed = false ; mx)

"""
    unsetfixed(x)

is the identity if `x` is not a `Mxpr`.
"""
unsetfixed(x) = x

debug_is_fixed(mx) = is_fixed(mx) ? symprintln(mx, " *is* fixed") : symprintln(mx, " is *not* fixed")

"""
    deepsetfixed(mx::Mxpr)

returns a copy of `mx` with its fixed bit set, as well as the fixed bit on all `Mxpr` subexpressions.
"""
function deepsetfixed(mx::Mxpr)
    nargs = newargs(mx)
    for i in 1:length(nargs)
        nargs[i] = deepsetfixed(mx[i])
    end
    return mxprcf(mhead(mx), nargs)
end

"""
    deepsetfixed(x)

is the identity if `x` is not a `Mxpr`.
"""
deepsetfixed(x) = x


function deepsetfixed_nocopy(mx::Mxpr)
    foreach(x -> setfixed(x), mx)
    setfixed(mx)
end
deepsetfixed_nocopy(x) = x

"""
    deepunsetfixed(mx::Mxpr)

return `mx` with the fixed bit unset and the fixed bit of all subexpressions unset.
"""
function deepunsetfixed(mx::Mxpr)
    nargs = newargs(mx)
    map!(deepunsetfixed,nargs,margs(mx))
    unsetfixed(mx)
end


"""
    deepunsetfixed(x)

is the identity if `x` is not a `Mxpr`.
"""
deepunsetfixed(x) = x

@inline is_canon(x) = false
@inline setcanon(x) = false
@inline unsetcanon(x) = false


function usersymbols()
    nargs = newargs()
    for k in keys(get_context_symtab(:Main))
        if ! haskey(get_context_symtab(:System),k)
            push!(nargs,string(k))
        end
    end
    return nargs
end

#### typealiases

const Orderless = Union{Mxpr{:Plus},Mxpr{:Times}}

# Everything except Bool is what we want
# Maybe we actually need a separate Bool from julia
const SJReal = Union{AbstractFloat, Irrational, Rational{Integer},BigInt,Signed, Unsigned}

# used in predicates.jl
const BlankXXX = Union{Mxpr{:Blank},Mxpr{:BlankSequence},Mxpr{:BlankNullSequence}}

# used in apprules.jl
const Rules = Union{Mxpr{:Rule},Mxpr{:RuleDelayed}}  ## find uses of this and remove them
const RulesT = Union{Mxpr{:Rule},Mxpr{:RuleDelayed}}

# used in expressions.jl
const Holds = Union{Mxpr{:Hold}, Mxpr{:HoldForm}, Mxpr{:HoldPattern}, Mxpr{:HoldComplete}}

# used in sortorderless.jl
const ExpNoCanon = Union{SJSym,Number}

# used in flatten.jl
const FlatT = Union{Mxpr{:Plus},Mxpr{:Times},Mxpr{:And},Mxpr{:Or}, Mxpr{:LCM}, Mxpr{:GCD} }

## Alias types for convenience and clarity.
for s in (:List, :Power, :Times, :Plus, :Rule)
    @eval const $(Symbol(s,"T"))  = Mxpr{$(QuoteNode(s))}
end

## For many jula functions that take real or complex floating point args.
## Complex{AbstractFloat} does not do what we want. The inner type must be concrete
const FloatRC  = Union{AbstractFloat, Complex{AbstractFloat},Complex{Float64}}

### Iterator, etc.

for s in (:(Base.pop!), :(Base.shift!), :(Base.unshift!), :(Base.push!), :(Base.start), :(Base.next), :(Base.done))
    @eval ($s)(mx::Mxpr,args...) = ($s)(margs(mx),args...)
end

Base.length(mx::Mxpr) = length(margs(mx))

# We try to make this fast: In the Symata language, mx[0] gets the head, but not here.
setindex!(mx::Mxpr, val, k::Integer) = (margs(mx)[k] = val)

## getindex(mx::Mxpr,ind) with single index does not check for index 0 for efficiency.
## Also the iterator over mx relies on this behavior.
## But, with more than one index, getindex checks for index of zero (heads)
## It is probably faster to use the 
@inline getindex(mx::Mxpr, k::Integer) = margs(mx)[k]
getindex(mx::Mxpr, inds...) = getpart(mx,inds...)

#@inline Base.endof(mx::Mxpr) = length(mx)
@inline Base.endof(mx::Mxpr) = endof(margs(mx))
@inline Base.copy(mx::Mxpr) = mxpra(mhead(mx),copy(margs(mx)))

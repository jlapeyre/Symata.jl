###########################################
#                                         #
# This file contains system state. There  #
# is more in kernelstate.jl.              #
#                                         #
###########################################

## Types SSJSym and Mxpr
# SSJSym are Symata symbols
# Mxpr are Symata expressions

typealias MxprArgs Array{Any,1}
typealias MxprArgType Any
typealias MxprArgT Any
typealias FreeSyms Dict{Symbol,Bool}

# AbstractMxpr is not used for anything
# abstract AbstractMxpr
# type Mxpr{T} <: AbstractMxpr

"""
    Mxpr{T}

is the type representing Symata expressions. If `T` is a symbol it is also the head of the Symata
expression.
"""
type Mxpr{T}
    head::Any  # making this Any instead of Symbol slows things a bit. hmmm. we should subtype
    args::MxprArgs
    fixed::Bool
    canon::Bool
    syms::FreeSyms
    age::UInt64
    key::UInt64
    typ::DataType
end

typealias SJSymAttrs Dict{Symbol,Bool}
typealias SJSymDVs Array{Any,1}
typealias SJSymuVs Array{Any,1}
@inline newattributes() = SJSymAttrs()
@inline newdownvalues() = Array(Any,0)
@inline newupvalues() = Array(Any,0)

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

abstract AbstractSJSym
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
typealias SJSym Symbol

#### Symbol Table

## symbol table for Symata symbols

typealias SymTab Dict{Symbol,SSJSym}

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

function  get_context_symtab{T<:Union{AbstractString,Symbol}}(context_name::T)
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
const system_symbols = Dict{Symbol,Bool}()
register_system_symbol(s::Symbol) =  system_symbols[s] = true
register_system_symbol{T<:AbstractString}(s::T) =  system_symbols[Symbol(s)] = true

#### Down values

# We store the Mxpr used definition to define downvalues. These
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

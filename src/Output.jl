"""
    module Symata.SymataIO

This module provides "1d" text representations of Symata expressions. It is
for use at the REPL, and for other purposes, such as writing to files.
"""
module SymataIO

import Symata: Mxpr, SJSym, SSJSym, ListT, TimesT, PowerT,
       getsym, symname, mhead, margs,  getoptype, mtojsym, newargs, mapmargs,
       mxpr, mxprcf, Infinity, getkerneloptions, unicode_output, Qsym,
       CurrentContext, wrapout, using_unicode_output, comparison_translation,
       symnumerator, symdenominator, Formatting

import Symata: print_with_function_to_string

const infix_with_space = Dict( :&& => true , :|| => true, :| => true)

# A space, or maybe not.
opspc(sym) = haskey(infix_with_space,sym) ? " " : getkerneloptions(:compact_output) ? "" : " "
const binaryops_space = collect(keys(comparison_translation) )

## Some of these, at least "^:=" can't be parsed as quoted symbols, but can be
## read as command line input.... So, we use some strings.
const binaryops_space_strings = [ "=>",  "->" , ":>" , "^:=" , "⇒", "∈" ]

function binaryopspc(ss)
    ss in binaryops_space && return " "
    s = string(ss)
    s in binaryops_space_strings && return " "
    return ""
end

## TODO: pass formatting choices to the functions rather than hardcoding them.

# Julia-like syntax
const FUNCL = '('
const FUNCR = ')'
const LISTL = '['
const LISTR = ']'

Istring() =  using_unicode_output() ? "𝕚" : "I"

# We could probably replace all instances of mtojsym below with this.
# It depends on which positions unicode symbols may occur.
function outsym(s)
    haskey(unicode_output, s) && using_unicode_output() && return unicode_output[s]
    os = mtojsym(s)
    haskey(unicode_output, os) && using_unicode_output() && return unicode_output[os]
    os
end

mutable struct FullFormData
    lfunc::String
    rfunc::String
end

const juliafullformdata = FullFormData("(",")")
const mmafullformdata = FullFormData("[","]")

fullform(io::IO, mx::Union{Mxpr, Rational, Complex}) = fullform(io, mx, juliafullformdata)


function fullform(io::IO, mx::Mxpr, data::FullFormData)
    # Really fucking weird. I have to do wrapout here, but not in the method of wrapout(::Mxpr)
    fullform(io, wrapout(mhead(mx)), data)
    print(io, data.lfunc)
    if length(mx) > 0 fullform(io, mx[1], data) end
    for i in 2:length(mx)
        print(io,",")
        fullform(io, mx[i], data)
    end
    print(io, data.rfunc)
end

fullform(io::IO, x, data::FullFormData) = Base.show(io, x)
fullform(x, data::FullFormData) = fullform(stdout, x, data)


fullform(x) = fullform(stdout, x, juliafullformdata)

mmafullform(io::IO, x) = fullform(io, x, mmafullformdata)
mmafullform(x) = mmafullform(stdout, x)

function Base.show(io::IO, mx::Mxpr{:FullForm})
    fullform(io, mx[1], juliafullformdata)
end

symata_to_mma_fullform_string(x) = print_with_function_to_string(mmafullform, wrapout(x))

# This puts unecessary parens on prefix functions.
#needsparen(x::Mxpr) = length(x) > 1
needsparen(x::Mxpr) = (length(x) > 1  && getoptype(mhead(x)) == :infix)
needsparen(x::Rational{T}) where {T<:Integer} = true

needsparen(x::Real) =  x < 0
needsparen(x::Complex{T}) where {T<:Integer} = (real(x) != 0)
needsparen(x::Complex{T}) where {T<:Real} = true

needsparen(x) = false

# Mma displays gensyms with all the linenoise.
# So, we try disabling de_gensym
de_gensym(x) = x
# function de_gensym{T<:AbstractString}(str::T)
#     if str[1] == '#' && str[2] == '#'  # De-gensym local variables for display
#         return split(str,['#'],keepempty=false)[1]
#     else
#         return str
#     end
# end

#### Wrap output

abstract type AbstractWO end

needsparen(y::AbstractWO) = needsparen(y.x)

struct WOSymbol <: AbstractWO
    x::Symbol
end

struct WOBool <: AbstractWO
    x::Bool
end

struct WORational{T}  <: AbstractWO
    x::T
end

abstract type WOComplex <: AbstractWO end

struct WOComplexInteger{T}  <: WOComplex
    x::T
end
wrapout(x::Complex{T}) where {T<:Integer}=  WOComplexInteger(x)
Base.show(io::IO, ws::WOComplexInteger) = show_complexinteger(io, ws.x)

struct WOComplexReal{T}  <: WOComplex
    x::T
end
wrapout(x::Complex{T}) where {T<:Real} = WOComplexReal(x)
Base.show(io::IO,ws::WOComplexReal) = show_complexreal(io, ws.x)

struct WOComplexRational{T}  <: WOComplex
    x::T
end
wrapout(x::Complex{Rational{T}}) where {T<:Integer} = WOComplexRational(x)
Base.show(io::IO,ws::WOComplexRational) = show_complexrational(io, ws.x)

unwrapout(x::AbstractWO) = x.x
unwrapout(x) = x

needsparen(x::WORational) = true
needsparen(x::WOComplexRational) = true

function fullform(io::IO, x::Union{WORational, Rational}, data::FullFormData)
    print(io, "Rational")
    print(io, data.lfunc)
    print(io, numerator(unwrapout(x)), ", ", denominator(unwrapout(x)))
    print(io, data.rfunc)
end

function fullform(io::IO, x::WOComplex, data::FullFormData)
    print(io, "Complex")
    print(io, data.lfunc)
    fullform(io, real(x.x), data)
    print(io, ", ")
    fullform(io, imag(x.x), data)
    print(io, data.rfunc)
end

struct WOAbstractFloat{T}  <: AbstractWO
    x::T
end
wrapout(x::AbstractFloat) = WOAbstractFloat(x)
Base.show(io::IO, ws::WOAbstractFloat) = show_float(io,ws.x)
## Not performant. We do a dictionary lookup every time we print a float.
function show_float(io,x::Float64)
    _show_float(io,x,getkerneloptions(:float_format))
end

function show_float(io,x::BigFloat)
    _show_float(io,x,getkerneloptions(:bigfloat_format))
end

function _show_float(io,x,fmt)
    if fmt == ""
        Base.show(io,x)
    else
        s = Formatting.sprintf1(fmt,x)   # always print a `.` for floating point numbers
        s = occursin(r"\.",s) ? s : s * "."
        print(io,s)
    end
end

function show_float(io,x)
    Base.show(io,x)
end

# This breaks printing
#Base.string{T<:AbstractWO}(y::T) = string(y.x)

#wrapout(x) = x  # defined in wrapout.jl

function wrapout(mx::Mxpr)
    # nargs = newargs()
    # for x in margs(mx)
    #     push!(nargs,wrapout(x))
    # end
    mxprcf(mhead(mx), mapmargs(wrapout, margs(mx)))  # In v0.6, this chooses most efficient Array type. We want Any.
#    mxprcf(mhead(mx), nargs)
end

function wrapout(s::Symbol)
    WOSymbol(s)
end

function wrapout(s::Bool)
    WOBool(s)
end

function wrapout(x::Rational)
    WORational(x)
end

Base.show(io::IO,ws::WOSymbol) = show_symbol(io, ws.x)


#### Symbol and SSJSym

# NB: This comment is only relevant if we change the Symata symbol implementation.
# This may break. It will only work if the value of s
# is the symbol name in the symbol table that is associated
# with s; ie it is a 'free' symbol. SSJSym does not carry symbol name information.
# This is only stored as the key to the symbol table.

Base.show(io::IO, s::SSJSym) = show_symbol(io, symname(s))

# SJSym is just a symbol, so this translates everything
# We need this, because SSJSym symbols are sometimes stored as Symbol's (ie SJSym)
#Base.show(io::IO, s::SJSym) = show_symbol(io,s)

function show_symbol(io::IO, s::SJSym)
#    println("*** showing $s")
    if haskey(unicode_output, s) && using_unicode_output()
        Base.show_unquoted(io,unicode_output[s])
    else
        ss = string(symname(s))
        ss = de_gensym(ss) # remove gensym characters
        Base.show_unquoted(io,Symbol(ss))
    end
end

Base.show(io::IO, x::WORational) = show_rational(io, x.x)

function show_rational(io::IO, x::Rational)
    Base.show(io, symnumerator(x))
    print(io, "/")
    Base.show(io, symdenominator(x))
end

# Yes, it is REPL.REPLCompletions.latex_symbols
function Base.show(io::IO, x::Mxpr{:Subscript})
    if using_unicode_output()
        try
            s1 = string(x[1].x) # unwrap symbol
            for i in 2:length(x)
                y = typeof(x[i]) <: AbstractWO ? string(x[i].x) : string(x[i])
                s1 *=  REPL.REPLCompletions.latex_symbols["\\_" * y]
            end
            Base.show(io, wrapout(Symbol(s1)))
        catch
            show_prefix_function(io,x)
        end
    else
        show_prefix_function(io,x)
    end
end

# We display real part if it is 0.0
function show_complexreal(io::IO, z::Complex{T}) where T<:Real
#    Base.show(io,real(z))
    show_float(io,real(z))
    print(io," + ")
    show_float(io,imag(z))
#    Base.show(io,imag(z))
    print(io,Istring())
end

# Do not display real part if it is 0
function show_complexinteger(io::IO, z::Complex{T}) where T<:Integer
    if real(z) != 0
        Base.show(io,real(z))
        print(io," + ")
    end
    if imag(z) == 1
        print(io,Istring())
    else
        iz = imag(z)
        if iz == -1
            print(io,"-")
        else
            Base.show(io,iz)
        end
        print(io,Istring())
    end
end

function show_complexrational(io::IO, z::Complex{Rational{T}}) where T<:Integer
    if real(z) != 0
        show_rational(io,real(z))
        print(io," + ")
    end
    if imag(z) == 1
        print(io,Istring())
    else
        print(io,Istring(), " * ")
        show_rational(io,imag(z))
    end
end

function  Base.show(io::IO, mx::Mxpr{:DirectedInfinity})
    if length(mx) == 0
        show_symbol(io, :ComplexInfinity)
    elseif mx[1] == 1
        show_symbol(io, :Infinity)
    elseif mx[1] == -1
        Base.show(io, wrapout(mxprcf(:Times, -1, Infinity)))
    else
        print(io, "DirectedInfinity(")
        print(io, mx[1])
        print(io, ")")
    end
end

# This is causing stack overflow in some cases.
# Base.show{T<:BigFloat}(io::IO,x::T) = Base.showcompact(io,x)

Base.show(io::IO, v::WOBool) = show_bool(io,v.x)

function show_bool(io::IO, v::Bool)
    v ? Base.show_unquoted(io,:True) : Base.show_unquoted(io,:False)
end

# For Holdform, arguments are not evaluated, as in Hold.
# But, in addition, Holdform is not printed.
function Base.show(io::IO, s::Mxpr{:HoldForm})
    Base.show(io,s[1])
end

function Base.show(io::IO, s::Mxpr)
    if getoptype(mhead(s)) == :binary
        return show_binary(io,s)
    elseif getoptype(mhead(s)) == :infix
        return show_infix(io,s)
    end
    show_prefix_function(io,s)
end

function Base.show(io::IO, mx::Mxpr{:Comparison})
    args = margs(mx)
    for i in 1:length(args)-1
        Base.show(io, outsym(args[i]))
        print(io," ")
    end
    isempty(args) || Base.show(io,args[end])
end


function show_prefix_function(io::IO, mx::Mxpr)
    if ! isa(mx,ListT)
        mh = mhead(mx)
        wantparen::Bool = false
        if isa(mh,Mxpr)
            wantparen = getoptype(mhead(mh)) == :infix ? true : false
        end
        if wantparen
            print(io,"(")  # refactor this! --> maybeparen(wantparen,obj) or s.t.
        end
        Base.show(io, wrapout(mhead(mx)))
        if wantparen
            print(io,")")
        end
    end
    args = mx.args
    print(io,mhead(mx) == getsym(:List) ? LISTL : FUNCL)
    wantparens = true
    if mhead(mx) == :List wantparens = false end
    for i in 1:length(args)-1
        if needsparen(args[i]) && wantparens
            print(io,"(")
        end
        Base.show(io,args[i])
        if needsparen(args[i]) && wantparens
            print(io,")")
        end
        print(io,",")
    end
    isempty(args) || Base.show(io,args[end])
    print(io,mx.head == getsym(:List) ? LISTR : FUNCR)
end

function show_binary(io::IO, mx::Mxpr)
    if length(mx) != 2
        show_prefix_function(io,mx)
    else
        opstr = outsym(mhead(mx))
        lop = mx[1]
        if needsparen(lop)
            print(io,"(")
            Base.show(io,lop)
            print(io,")")
        else
            Base.show(io,lop)
        end
        print(io, binaryopspc(opstr), opstr , binaryopspc(opstr))
        rop = mx[2]
        if  needsparen(rop)
            print(io,"(")
            Base.show(io,rop)
            print(io,")")
        else
            Base.show(io,rop)
        end
    end
end

function Base.show(io::IO, mx::Mxpr{:Part})
    args = margs(mx)
    Base.show(io,args[1])
    print(io,"[")
    Base.show(io,args[2])
    for i in 3:length(args)
        print(io,",")
        Base.show(io,args[i])
    end
    print(io,"]")
end

# unary minus
function Base.show(io::IO, mx::Mxpr{:Minus})
    arg = mx.args[1]
    if isa(arg,Number) || isa(arg,SJSym)
        print(io,"-")
        Base.show(io,arg)
    else
        print(io,"-(")
        Base.show(io,arg)
        print(io,")")
    end
end

function Base.show(io::IO, mx::Mxpr{:Plus})
    args = margs(mx)
    if length(args) < 1
        error("show: can't show Plus with no args.")
    end
    Base.show(io,args[1])
    for i in 2:length(args)
        if isa(args[i],Mxpr{:Minus})
            print(io, " - ")
            Base.show(io,(args[i]).args[1])
        else
            if isa(args[i], TimesT) && typeof(args[i][1])  <:Union{AbstractFloat,Integer} && args[i][1] < 0
                show_infix(io, args[i], true)
            else
                print(io, " + ")
                Base.show(io, args[i])
            end
        end
    end
end

# This includes Times. No spaces surround the infix symbol

show_infix(io::IO, mx::Mxpr)  = show_infix(io,mx, false)
function show_infix(io::IO, mx::Mxpr, spaceminus::Bool)
    args = margs(mx)
    np = false
    sepsym = mtojsym(mhead(mx))
    spc = opspc(sepsym)
    # uncomment following to print space for multiplication rather than *. But, I want "InputForm" for now,
    # so we can copy output to input.
#    if mhead(mx) == :Times sepsym = " " end # not a sym. Maybe we should make them all strings
    startind = 1
    if isa(mx,TimesT) && ! isempty(args)
        a1 = args[1]
        if a1 == -1
            print(io, spaceminus ? " - " : "-")
            startind = 2
        elseif typeof(a1) <:Union{AbstractFloat,Integer}
            if a1 < 0 && spaceminus
                print(io, " - ", - a1)
            else
                print(io, a1)
                if length(args) > 1 && isa(args[2],Union{Number,PowerT})
                    print(io, spc, sepsym, spc)
                end
            end
            startind = 2
        end
    end
    for i in startind:length(args)-1
        arg = args[i]
        if needsparen(arg)
            np = true
            print(io,"(")
        else
            np = false
        end
        Base.show(io,arg)
        if np
            print(io,")")
        end
        print(io, spc, sepsym, spc)
    end
    if ! isempty(args)
        if needsparen(args[end])
            np = true
            print(io,"(")
        else
            np = false
        end
        Base.show(io, args[end])
        if np
            print(io,")")
        end
    end
end

function Base.show(io::IO, mx::Mxpr{:Blank})
    print(io,"_")
    if length(mx) > 0
        Base.show(io,mx[1])
    end
end

function Base.show(io::IO, mx::Mxpr{:BlankSequence})
    print(io,"__")
    if length(mx) > 0
        Base.show(io,mx[1])
    end
end

function Base.show(io::IO, mx::Mxpr{:BlankNullSequence})
    print(io,"___")
    if length(mx) > 0
        Base.show(io,mx[1])
    end
end

function Base.show(io::IO, mx::Mxpr{:Pattern})
    Base.show(io,mx[1])
    if isa(mx[2],Mxpr{:Blank})
        Base.show(io,mx[2])
    else
        print(io,"::(")
        Base.show(io,mx[2])
        print(io,")")
    end
end

function Base.show(io::IO, mx::Mxpr{:Element})
    if using_unicode_output()
        show_binary(io,mx)
    else
        show_prefix_function(io,mx)
    end
end

function Base.show(io::IO, qs::Qsym)
    if qs.context != CurrentContext.name
        Base.show(io,qs.context)
        print(io,".")
    end
    Base.show(io,qs.name)
end


end # module SymataIO

module SJuliaIO

import Base: show

import SJulia: Mxpr, SJSym, SSJSym, is_Mxpr, is_Number, is_SJSym,
       getsym, symname, mhead, margs, is_type, getoptype, mtojsym,
       mxpr, mxprcf, Infinity, getkerneloptions, unicode_output, Qsym,
       CurrentContext


const infix_with_space = Dict( :&& => true , :|| => true, :| => true)

# A space, or maybe not.
#opspc() = getkerneloptions(:compact_output) ? "" : " "

opspc(sym) = haskey(infix_with_space,sym) ? " " : getkerneloptions(:compact_output) ? "" : " "

# These are binary operators that want one space before and one after.
# The default, e.g  Power is no space. x^y
# This broke somehow
function oldbinaryopspc(ss)
    s = Symbol(ss)
    (s == :(=>) || s == :(->) || s == Symbol(":>")  || s == Symbol("^:=")) && return " "
    return ""
end

function binaryopspc(ss)
    s = string(ss)
    (s == "=>" || s == "->" || s == ":>"  || s == "^:=") && return " "
    return ""
end

# Julia-like syntax
const FUNCL = '('
const FUNCR = ')'
const LISTL = '['
const LISTR = ']'

# This is faster than looking in the dict for I
Istring() = getkerneloptions(:unicode_output) ? "𝕚" : "I"

# We could probably replace all instances of mtosjym below with this.
# It depends on which positions unicode symbols may occur.
function outsym(s)
    haskey(unicode_output, s) && getkerneloptions(:unicode_output) && return unicode_output[s]
    mtojsym(s)
end

function fullform(io::IO, mx::Mxpr)
    print(io,mhead(mx))
    print("(")
    if length(mx) > 0 fullform(io,mx[1]) end
    for i in 2:length(mx)
        print(io,",")
        fullform(io,mx[i])
    end
    print(")")
end
fullform(io::IO,x) = show(io,x)
fullform(x) = fullform(STDOUT,x)

needsparen(x::Mxpr) = length(x) > 1
needsparen{T<:Integer}(x::Rational{T}) = true

needsparen(x::Real) =  x < 0
needsparen{T<:Integer}(x::Complex{T}) = real(x) != 0
needsparen{T<:Real}(x::Complex{T}) = true
needsparen(x) = false

# Mma displays gensyms with all the linenoise.
# So, we try disabling de_gensym
de_gensym(x) = x
# function de_gensym{T<:AbstractString}(str::T)
#     if str[1] == '#' && str[2] == '#'  # De-gensym local variables for display
#         return split(str,['#'],keep=false)[1]
#     else
#         return str
#     end
# end

# We need a way to force using this. In the REPL, I guess
# Hmm. Wrap all output in a type made just for that purpose.... no that won't work
# sjshow(io::IO,x) = Base.show(io,x)

# We translate <= below in show_Comparison.
# Probably more efficient to do it here.
# SJSym is just a symbol, so this translates everything
function Base.show(io::IO, s::SJSym)
    if haskey(unicode_output, s) && getkerneloptions(:unicode_output)
        Base.show_unquoted(io,unicode_output[s])
    else
        ss = string(symname(s))
        ss = de_gensym(ss) # remove gensym characters
        Base.show_unquoted(io,Symbol(ss))
    end
end
                           
# Overwrite Base definition
function Base.show(io::IO, x::Rational)
    show(io, num(x))
    print(io, "/")
    show(io, den(x))
end

# NB: This comment is only relevant if we change the SJulia symbol implementation.
# This may break. It will only work if the value of s
# is the symbol name in the symbol table that is associated
# with s; ie it is a 'free' symbol. SSJSym does not carry symbol name information.
# This is only stored as the key to the symbol table.
Base.show(io::IO, s::SSJSym) = Base.show(symname(s))

# We display real part if it is 0.0
function Base.show{T<:Real}(io::IO, z::Complex{T})
    show(io,real(z))
    print(io," + ")
    show(io,imag(z))
    print(io,Istring())
end

# Do not display real part if it is 0
function Base.show{T<:Integer}(io::IO, z::Complex{T})
    if real(z) != 0
        show(io,real(z))
        print(io," + ")
    end
    if imag(z) == 1
        print(io,Istring())
    else
        iz = imag(z)
        if iz == -1
            print(io,"-")
        else
            show(io,iz)
        end
        print(io,Istring())
    end
end

function Base.show{T<:Integer}(io::IO, z::Complex{Rational{T}})
    if real(z) != 0
        show(io,real(z))
        print(io," + ")
    end
    if imag(z) == 1
        print(io,Istring())
    else
        show(io,imag(z))
        print(io,Istring())
    end
end

function  Base.show(io::IO, mx::Mxpr{:DirectedInfinity})
    if length(mx) == 0
        Base.show(io, :ComplexInfinity)
    elseif mx[1] == 1
        Base.show(io, :Infinity)
    elseif mx[1] == -1
        Base.show(io, mxprcf(:Times, -1, Infinity))
    else
        print(io, "DirectedInfinity(")
        print(io, mx[1])
        print(io, ")")
    end
end

# This is causing stack overflow in some cases.
# Base.show{T<:BigFloat}(io::IO,x::T) = Base.showcompact(io,x)

# Not sure this is a good idea, confusing symbols with boolean values
# NB, so far, it seems to be working well.
function Base.show(io::IO, v::Bool)
    v ? Base.show_unquoted(io,:True) : Base.show_unquoted(io,:False)
end

Base.show(io::IO, mx::Mxpr{:FullForm}) = fullform(io,mx[1])

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
    args = mx.args
    for i in 1:length(args)-1
        show(io, outsym(args[i]))
        print(io," ")
    end
    isempty(args) || show(io,args[end])
end

function show_prefix_function(io::IO, mx::Mxpr)
    is_Mxpr(mx,:List) ? nothing : print(io,outsym(mhead(mx)))
    args = mx.args
    print(io,mhead(mx) == getsym(:List) ? LISTL : FUNCL)
    wantparens = true
    if mhead(mx) == :List wantparens = false end
    for i in 1:length(args)-1
        if needsparen(args[i]) && wantparens
            print(io,"(")
        end
        show(io,args[i])
        if needsparen(args[i]) && wantparens
            print(io,")")
        end
        print(io,",")
    end
    isempty(args) || show(io,args[end])
    print(io,mx.head == getsym(:List) ? LISTR : FUNCR)
end

function show_binary(io::IO, mx::Mxpr)
    if length(mx) != 2
        show_prefix_function(io,mx)
    else
        opstr = mtojsym(mhead(mx))
        lop = mx[1]
        if needsparen(lop)
            print(io,"(")
            show(io,lop)
            print(io,")")
        else
            show(io,lop)
        end
        print(io, binaryopspc(opstr), opstr , binaryopspc(opstr))
        rop = mx[2]
        if  needsparen(rop)
            print(io,"(")
            show(io,rop)
            print(io,")")
        else
            show(io,rop)
        end
    end
end

function Base.show(io::IO, mx::Mxpr{:Part})
    args = margs(mx)
    show(io,args[1])
    print(io,"[")
    show(io,args[2])
    for i in 3:length(args)
        print(io,",")
        show(io,args[i])
    end
    print(io,"]")
end

# unary minus
function Base.show(io::IO, mx::Mxpr{:Minus})
    arg = mx.args[1]
    if is_Number(arg) || is_SJSym(arg)
        print(io,"-")
        show(io,arg)
    else
        print(io,"-(")
        show(io,arg)
        print(io,")")
    end
end

function Base.show(io::IO, mx::Mxpr{:Plus})
    args = mx.args
    if length(args) < 1
        error("show: can't show Plus with no args.")
    end
    show(io,args[1])
    for i in 2:length(args)
        if is_type(args[i],Mxpr{:Minus})
            print(io, " - ")
            show(io,(args[i]).args[1])
        else
            print(io, " + ")
            show(io,args[i])
        end
    end
end

# This includes Times. No spaces surround the infix symbol
function show_infix(io::IO, mx::Mxpr)
    args = margs(mx)
    np = false
    sepsym = mtojsym(mhead(mx))
    # uncomment following to print space for multiplication. But, I want "InputForm" for now,
    # so we can copy output to input.
#    if mhead(mx) == :Times sepsym = " " end # not a sym. Maybe we should make them all strings
    startind = 1
    if is_Mxpr(mx,:Times) && length(args) > 0
        if args[1] == -1
            print(io, "-")
            startind = 2            
        elseif typeof(args[1]) <:Union{AbstractFloat,Integer}
            print(io,args[1])
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
        show(io,arg)
        if np
            print(io,")")
        end
        spc = opspc(sepsym)
        print(io, spc, sepsym, spc)
    end
    if ! isempty(args)
        if needsparen(args[end])
            np = true
            print(io,"(")
        else
            np = false
        end
        show(io, args[end])
        if np
            print(io,")")
        end
    end
end

function Base.show(io::IO, mx::Mxpr{:Blank})
    print(io,"_")
    if length(mx) > 0
        show(io,mx[1])
    end
end

function Base.show(io::IO, mx::Mxpr{:BlankSequence})
    print(io,"__")
    if length(mx) > 0
        show(io,mx[1])
    end
end

function Base.show(io::IO, mx::Mxpr{:BlankNullSequence})
    print(io,"__")
    if length(mx) > 0
        show(io,mx[1])
    end
end

function Base.show(io::IO, mx::Mxpr{:Pattern})
    show(io,mx[1])
    if is_Mxpr(mx[2],:Blank)
        show(io,mx[2])
    else
        print(io,"::(")
        show(io,mx[2])
        print(io,")")        
    end
end

function Base.show(io::IO, qs::Qsym)
    if qs.context != CurrentContext.name
        show(io,qs.context)
        print(io,".")
    end
    show(io,qs.name)    
end


end # module SJuliaIO

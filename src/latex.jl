# Is import file-scoped ? If not, we need to do something else
import Symata.SymataIO: WORational, WOComplexRational, Istring, outsym, FUNCR, FUNCL, LISTR, LISTL, needsparen
import Symata.SymataIO: WOComplexReal, WOComplexInteger, WOSymbol, WOBool

#######################################################################

# Following copied from LaTeXString by Steven G Johnson. We surely need almost none of this.
# But, there is something magic here that makes IJulia render the strings
# as LaTeX. I don't yet know where it is.

immutable MyLaTeXString <: AbstractString
    s::String
end

# coercing constructor:
function latexstring(s::String)
    # the only point of using MyLaTeXString to represent equations, since
    # IJulia doesn't support MyLaTeX output other than equations, so add $'s
    # around the string if they aren't there (ignoring \$)
    return ismatch(r"[^\\]\$|^\$", s) ?
        MyLaTeXString(s) :  MyLaTeXString(string("\$", s, "\$"))
end
latexstring(s::AbstractString) = latexstring(String(s))
latexstring(args...) = latexstring(string(args...))

macro L_str(s, flags...) latexstring(s) end
macro L_mstr(s, flags...) latexstring(s) end

import Base: write, endof, getindex, sizeof, search, rsearch, isvalid, next, length, bytestring, IOBuffer, pointer
@compat import Base.show

write(io::IO, s::MyLaTeXString) = write(io, s.s)
@compat show(io::IO, ::MIME"application/x-latex", s::MyLaTeXString) = write(io, s)
@compat show(io::IO, ::MIME"text/latex", s::MyLaTeXString) = write(io, s)

function show(io::IO, s::MyLaTeXString)
    print(io, "L")
    Base.print_quoted_literal(io, s.s)
end

bytestring(s::MyLaTeXString) = bytestring(s.s)
endof(s::MyLaTeXString) = endof(s.s)
next(s::MyLaTeXString, i::Int) = next(s.s, i)
length(s::MyLaTeXString) = length(s.s)
getindex(s::MyLaTeXString, i::Int) = getindex(s.s, i)
getindex(s::MyLaTeXString, i::Integer) = getindex(s.s, i)
getindex(s::MyLaTeXString, i::Real) = getindex(s.s, i)
getindex(s::MyLaTeXString, i::UnitRange{Int}) = getindex(s.s, i)
getindex{T<:Integer}(s::MyLaTeXString, i::UnitRange{T}) = getindex(s.s, i)
getindex(s::MyLaTeXString, i::AbstractVector) = getindex(s.s, i)
sizeof(s::MyLaTeXString) = sizeof(s.s)
search(s::MyLaTeXString, c::Char, i::Integer) = search(s.s, c, i)
rsearch(s::MyLaTeXString, c::Char, i::Integer) = rsearch(s.s, c, i)
isvalid(s::MyLaTeXString, i::Integer) = isvalid(s.s, i)
pointer(s::MyLaTeXString) = pointer(s.s)
IOBuffer(s::MyLaTeXString) = IOBuffer(s.s)

# conversion to pass MyLaTeXString to ccall arguments
if VERSION >= v"0.4.0-dev+3710"
    import Base.unsafe_convert
else
    import Base.convert
    const unsafe_convert = Base.convert
end
@compat unsafe_convert(T::Union{Type{Ptr{UInt8}},Type{Ptr{Int8}}}, s::MyLaTeXString) = convert(T, s.s)
if VERSION >= v"0.4.0-dev+4603"
    unsafe_convert(::Type{Cstring}, s::MyLaTeXString) = unsafe_convert(Cstring, s.s)
end

################################################################################

const llparen = " \\left( "
const lrparen = " \\right) "

#const LFUNCL = " \\left( "
#const LFUNCR = " \\right) "
const LLISTL = " \\left[ "
const LLISTR = " \\right] "

latex_needsparen(x) = needsparen(x)

latex_needsparen(mx::Mxpr{:Power}) = symjlength(base(mx)) > 1

Ilatexstring() = "\\mathbb{i}"
Infinitylatexstring() = "\\infty"

# These are translations of the printed symbols to latex.
# Emacs is fighting me over indentation here. Fix it when we are more or less done with this.
const symbol_to_latex_table = Dict(
                                   :(=>) => " \\Rightarrow ",
                                   :(->) => " \\rightarrow ",
                                   Symbol("^:=") => " \\text{^:=} ",
                                   :(>=) => " \\ge ",
                                   :(<=) => " \\le ",
                                   :(!=) => " \\ne ",
                                   :(&&) => " \\, \\text{&&} \\, ",
:(||) => " \\, \\text{||} \\, ",
:Pi => " \\pi ",
:E => " \\mathbb{e} ",
:Gamma => " \\Gamma ",
:EulerGamma => " \\gamma "
                                   )


function latex_symbol(s::Symbol)
    haskey(symbol_to_latex_table, s) ? symbol_to_latex_table[s] : latex_string(s)
end

function latex_display(x)
    MyLaTeXString("\$\$ " * latex_string(x) *  " \$\$")
end

# show will print this correctly
latex_display(mx::Mxpr{:FullForm}) = mx

#### LaTeXString

@mkapprule LaTeXString :nargs => 1

@sjdoc LaTeXString "
LaTeXString(expr)

convert `expr` to a LaTeX string. This is the same string used for display
in the Jupyter notebook. Conversion to LaTeX strings optimized for
mathematical texts is not implemented.
"

@doap function LaTeXString(x)
    latex_string(x)
end

# TODO: an all cases we should use takebuf_string(buf) for efficiency rather
# than s *= "new text". Some functions below already use takebuf_string

latex_string(x) = string(x)

latex_string(s::String) =  latex_text("\"" * s * "\"")

function latex_string(s::Mxpr)
    if getoptype(mhead(s)) == :binary
        return latex_string_binary(s)
    elseif getoptype(mhead(s)) == :infix
        return latex_string_infix(s)
    end
    latex_string_prefix_function(s)
end

# This will not be called if FullForm is the toplevel expression.
# Here, FullForm will be converted to a string, as in Plain style and then wrapped in math mode text macro.
latex_string(mx::Mxpr{:FullForm}) = latex_text(mx)

latex_text(s) =  "\\text{" * string(s)  * "}"

function latex_string_mathop(ins)
    s = string(ins)
    isempty(s) && return ""
    islower(s[1]) && return s  # if Head begins lower case print in math italic, e.g. f(x)
    latex_text(s)
end

# ipython inserts a space that we don't want, so we use  \!
# tex2mail also inserts spurious spaces, but prints \! as more spaces
function latex_string_prefix_function(mx::Mxpr)
    buf = IOBuffer()
    print(buf, is_Mxpr(mx,:List) ? ""  : latex_string(mhead(mx)) * " \\! ")
#    print(buf, is_Mxpr(mx,:List) ? ""  : latex_string(mhead(mx)) )  # ipython inserts a space that we don't want
    args = margs(mx)
    print(buf, mhead(mx) == getsym(:List) ? LISTL : llparen)
    wantparens = mhead(mx) == :List ? false : true
    for i in 1:length(args)-1
        if latex_needsparen(args[i]) && wantparens
           print(buf, llparen)
        end
        print(buf, latex_string(args[i]))
        if latex_needsparen(args[i]) && wantparens
            print(buf, lrparen)
        end
        print(buf, ",")
    end
    if  ! isempty(args) print(buf, latex_string(args[end])) end
    print(buf, mx.head == getsym(:List) ? LISTR : lrparen)
    takebuf_string(buf)
end


function latex_string(mx::Mxpr{:Plus})
    terms = margs(mx) # why does terms(mx) not work ?
    if length(terms) < 1
        error("show: can't display Plus with no terms.")
    end
    s = latex_string(terms[1])
    for i in 2:length(terms)
        if is_type(terms[i],Mxpr{:Minus})
            s *=  " - " * latex_string((terms[i]).terms[1])
        # following is slightly broken. Fix it later
        # elseif  is_Mxpr(terms[i], :Times) && typeof(terms[i][1])  <:Union{AbstractFloat,Integer} && terms[i][1] < 0
        #     s *= latex_string(terms[i], true)
        else
            s *= " + " * latex_string(terms[i])
        end
    end
    s
end

function latex_string(mx::Mxpr{:Power})
    buf = IOBuffer()
    latex_needsparen(mx) ? print(buf, llparen, latex_string(base(mx)), lrparen) : print(buf,latex_string(base(mx)))
    print(buf, "^{" * latex_string(exponent(mx)) * "}")
    takebuf_string(buf)
end

function separate_negative_powers(facs)
    other = Array(Any,0)
    negpows = Array(Any,0)
    rationals = Array(Any,0)
    for x in facs
        t = typeof(x)
        if t <: Rational
            push!(rationals,x)
        elseif t <: WORational
            push!(rationals,x.x)
        elseif t == Mxpr{:Power} && typeof(exponent(x)) <: Number && exponent(x) < 0
            push!(negpows,x)
        else
            push!(other,x)
        end
    end
    return (other, negpows, rationals)
end

function get_nums_dens(other, negpows, rationals)
    nums = Array(Any,0)
    dens = Array(Any,0)
    for x in rationals
        if num(x) != 1 push!(nums, num(x)) end
        push!(dens, den(x))
    end
    for x in negpows
        e = -exponent(x)
        if e == 1
            push!(dens, base(x))
        else
            push!(dens, mxpr(:Power, base(x), -exponent(x)))
        end
    end
    append!(nums, other)
    (nums,dens)
end

function latex_string_factor(buf::IOBuffer, x, nfacs)
    latex_needsparen(x) && nfacs > 1 ? print(buf, llparen, latex_string(x), lrparen) : print(buf, latex_string(x))
end

# spaceminus is passed around, but does not yet do anything.
# Needs to be fixed and implemented.
minus_or_factor(buf, x, nfacs) = minus_or_factor(buf, x, nfacs, false)
function minus_or_factor(buf, x, nfacs, spaceminus::Bool)
    if x == -1
#        print(buf, spaceminus ? " - " : "-")  # maybe don't even need this
        print(buf, "-")
    else
        latex_string_factor(buf,x, nfacs)  # check for minus space here ?
    end
end

latex_string_factors(buf,facs) = latex_string_factors(buf,facs,false)
function latex_string_factors(buf,facs,  spaceminus)
    minus_or_factor(buf, facs[1], length(facs), spaceminus)
    if length(facs) > 1  print(buf, " \\ ") end  # always will be
    if length(facs) > 2
        for i in 2:length(facs)-1
            minus_or_factor(buf, facs[i], length(facs))
            print(buf, " \\ ")
        end
    end
    if length(facs) > 1  minus_or_factor(buf, facs[end], length(facs)) end
end

# LaTeX does not put spaces between factors. But, Symata has multicharacter symbols, so we need spaces to distinguish them
latex_string(mx::Mxpr{:Times})  = latex_string(mx, false)
function latex_string(mx::Mxpr{:Times}, spaceminus::Bool)
    (other, negpows, rationals) = separate_negative_powers(factors(mx))
    buf = IOBuffer()
    if ( isempty(negpows) && isempty(rationals) )
        latex_string_factors(buf, factors(mx), spaceminus)
    else
        (nums,dens) = get_nums_dens(other,negpows, rationals)
        print(buf,"\\frac{")
        latex_string_factors(buf,nums)
        print(buf,  "}{")
        latex_string_factors(buf,dens)
        print(buf,"}")
    end
    takebuf_string(buf)
end

function latex_string(x::WORational)
    r = x.x
    latex_string(r)
end

function latex_string{T<:Real}(z::Rational{T})
    "\\frac{" * latex_string(num(z)) * "}{" * latex_string(den(z)) * "}"
end

function latex_string(x::WOComplexRational)
    z = x.x
    s = ""
    if real(z) != 0
        s *= latex_string(real(z)) * " + "
    end
    if imag(z) == 1
        s *= Ilatexstring()
    else
        s *=   latex_string(imag(z)) *  " \\ " * Ilatexstring()
    end
    s
end

# We display real part if it is 0.0
function latex_string(x::WOComplexReal)
    z = x.x
    latex_string(real(z)) * " + " * latex_string(imag(z)) * Ilatexstring()
end

# Do not display real part if it is 0
function latex_string(x::WOComplexInteger)
    z = x.x
    s = ""
    if real(z) != 0
        s *= latex_string(real(z)) * " + "
    end
    if imag(z) == 1
        s *= Ilatexstring()
    else
        iz = imag(z)
        if iz == -1
            s *= "-"
        else
            s *= latex_string(iz)
        end
        s *= Ilatexstring()
    end
    s
end

function  latex_string(mx::Mxpr{:DirectedInfinity})
    if length(mx) == 0
        latex_string_mathop(:ComplexInfinity)
    elseif mx[1] == 1
        Infinitylatexstring()
    elseif mx[1] == -1
        "-" * Infinitylatexstring()
    else
        latex_string_mathop(:DirectedInfinity) * llparen * latex_string(mx[1]) * lrparen
    end
end

function latex_string(mx::Mxpr{:Subscript})
    latex_string(mx[1]) * "_{" *
    join([latex_string(mx[i]) for i in 2:length(mx)], ",") *
    "}"
end

latex_string(s::SSJSym) = latex_string(symname(s))

function latex_string(s::WOSymbol)
    latex_string(s.x)
end

function latex_string(s::Symbol)
    ms = mtojsym(s)
    haskey(symbol_to_latex_table, ms) ? symbol_to_latex_table[ms] : latex_string_mathop(ms)
#    latex_string_mathop(mtojsym(s))
end

function latex_string(v::WOBool)
    v.x ? latex_text(:True) : latex_text(:False)
end

function latex_string(t::DataType)
    latex_text(t)
end

function latex_string(mx::Mxpr{:Comparison})
    join([latex_string(outsym(x)) for x in margs(mx)], " \\, " )
end

function latex_string_binary(mx::Mxpr)
    if length(mx) != 2
        return latex_string_prefix_function(mx)
    else
        s = ""
        opstr = outsym(mhead(mx))
        lop = mx[1]
        if latex_needsparen(lop)
            s *= llparen * latex_string(lop) * lrparen
        else
            s *= latex_string(lop)
        end
        s *= latex_symbol(opstr)
        rop = mx[2]
        if  latex_needsparen(rop)
            s *= llparen * latex_string(rop) * lrparen
        else
            s *= latex_string(rop)
        end
    end
    s
end


# Times is handled above. This needs to be cleaned up.
latex_string_infix(s) = s
function latex_string_infix(mx::Mxpr)
    args = margs(mx)
    np = false
    sepsym = mtojsym(mhead(mx))
    s = ""
    for i in 1:length(args)-1
        arg = args[i]
        if latex_needsparen(arg)
            np = true
            s *= llparen
        else
            np = false
        end
        s *= latex_string(arg)
        if np
            s *= lrparen
        end
        s *= latex_string(sepsym)
    end
    if ! isempty(args)
        if latex_needsparen(args[end])
            np = true
            s *= llparen
        else
            np = false
        end
        s *= latex_string(args[end])
        if np
            s *= lrparen
        end
    end
    s
end


function latex_string(mx::Mxpr{:Blank})
    s = latex_text("_")
    if length(mx) > 0
        s *= latex_string(mx[1])
    end
    s
end

function latex_string(mx::Mxpr{:BlankSequence})
    s = latex_text("__")
    if length(mx) > 0
        s *= latex_string(mx[1])
    end
    s
end

function latex_string(mx::Mxpr{:BlankNullSequence})
    s = latex_text("___")
    if length(mx) > 0
        s *= latex_string(mx[1])
    end
    s
end

function latex_string(mx::Mxpr{:Pattern})
    s = latex_string(mx[1])
    if is_Mxpr(mx[2],:Blank)
        s *= latex_string(mx[2])
    else
        s *= " \\text{::}(" * latex_string(mx[2]) * ")"
    end
    s
end

function latex_string(qs::Qsym)
    s = ""
    if qs.context != CurrentContext.name
        s *= latex_string(qs.context) * "."
    end
    s *= latex_string(qs.name)
end

# For Holdform, arguments are not evaluated, as in Hold.
# But, in addition, Holdform is not printed.
latex_string(mx::Mxpr{:HoldForm}) = latex_string(mx[1])


function integral_limits_string(varrange)
    if length(varrange) == 3
        return "\\int_{" * latex_string(varrange[2]) * "}^{" * latex_string(varrange[3]) * "}"
    else
        return "\\int "
    end
end

function infinitessimal_string(varrange)
    return " \\mathbb{d}" * latex_string(varrange[1])
end

function latex_string(mx::Mxpr{:Integrate})
    buf = IOBuffer()
    integrand = latex_string(mx[1])
    if length(mx) == 2  # single integration
        varrange = mx[2]
        print(buf, integral_limits_string(varrange))
        print(buf, integrand * " \\, " * infinitessimal_string(varrange))
    else
        for i in 2:length(mx)
            varrange = mx[i]
            print(buf, integral_limits_string(varrange))
            print(buf, " " * infinitessimal_string(varrange))
        end
        print(buf, " \\, " * integrand)
    end
    return takebuf_string(buf)
end

function sum_limits_string(varrange)
    if length(varrange) == 3
        return "\\sum_{" * latex_string(varrange[1]) * "=" * latex_string(varrange[2]) * "}^{" * latex_string(varrange[3]) * "}"
    else
        return "\\sum " # This probably should not happen
    end
end

function latex_string(mx::Mxpr{:Sum})
    buf = IOBuffer()
    summand = latex_string(mx[1])
    if length(mx) == 2  # single summation
        varrange = mx[2]
        print(buf, sum_limits_string(varrange))
        print(buf, summand)
        return takebuf_string(buf)
    else
        args = margs(mx)
        sargs = args[2:end]
        print(buf, " \\sum ")
        print(buf, "_{\\substack{" * join([ latex_string(v[1]) * "=" * latex_string(v[2]) for v in sargs], " \\\\ ") * "}}")
        ul = sargs[1][3]
        if all( x -> x[3] == ul , sargs)
            print(buf, "^{" * latex_string(sargs[1][3]) * "}")   # only display one upper limit if they are all the same
        else
            print(buf, "^{" * join([latex_string(v[3]) for v in sargs], ",") * "}")
        end
        print(buf, summand)
        return takebuf_string(buf)
    end
end

# Is import file-scoped ? If not, we need to do something else
import Symata.SymataIO: WORational, WOComplexRational, Istring, outsym, FUNCR, FUNCL, LISTR, LISTL, needsparen
import Symata.SymataIO: WOComplexReal, WOComplexInteger, WOSymbol, WOBool, show_float

#######################################################################

# Following copied from LaTeXString by Steven G Johnson. We surely need almost none of this.
# But, there is something magic here that makes IJulia render the strings
# as LaTeX. I don't yet know where it is.

struct MyLaTeXString <: AbstractString
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

import Base: write, endof, getindex, sizeof, search, rsearch, isvalid, next, length, IOBuffer, pointer
import Base.show

write(io::IO, s::MyLaTeXString) = write(io, s.s)
show(io::IO, ::MIME"application/x-latex", s::MyLaTeXString) = write(io, s)
show(io::IO, ::MIME"text/latex", s::MyLaTeXString) = write(io, s)

function show(io::IO, s::MyLaTeXString)
    print(io, "L")
    Base.print_quoted_literal(io, s.s)
end

Base.string(s::MyLaTeXString) = string(s.s)
endof(s::MyLaTeXString) = endof(s.s)
next(s::MyLaTeXString, i::Int) = next(s.s, i)
length(s::MyLaTeXString) = symlength(s.s)
getindex(s::MyLaTeXString, i::Int) = getindex(s.s, i)
getindex(s::MyLaTeXString, i::Integer) = getindex(s.s, i)
getindex(s::MyLaTeXString, i::Real) = getindex(s.s, i)
getindex(s::MyLaTeXString, i::UnitRange{Int}) = getindex(s.s, i)
getindex(s::MyLaTeXString, i::UnitRange{T}) where {T<:Integer} = getindex(s.s, i)
getindex(s::MyLaTeXString, i::AbstractVector) = getindex(s.s, i)
sizeof(s::MyLaTeXString) = sizeof(s.s)
search(s::MyLaTeXString, c::Char, i::Integer) = search(s.s, c, i)
rsearch(s::MyLaTeXString, c::Char, i::Integer) = rsearch(s.s, c, i)
isvalid(s::MyLaTeXString, i::Integer) = isvalid(s.s, i)
pointer(s::MyLaTeXString) = pointer(s.s)
IOBuffer(s::MyLaTeXString) = IOBuffer(s.s)

# conversion to pass MyLaTeXString to ccall arguments
import Base.unsafe_convert
unsafe_convert(T::Union{Type{Ptr{UInt8}},Type{Ptr{Int8}}}, s::MyLaTeXString) = convert(T, s.s)
unsafe_convert(::Type{Cstring}, s::MyLaTeXString) = unsafe_convert(Cstring, s.s)

################################################################################

const llparen = " \\left( "
const lrparen = " \\right) "

#const LFUNCL = " \\left( "
#const LFUNCR = " \\right) "
const LLISTL = " \\left[ "
const LLISTR = " \\right] "

latex_needsparen(x) = needsparen(x)

latex_needsparen(mx::Mxpr{:Power}) = _latex_needsparen_power(base(mx),expt(mx))
_latex_needsparen_power(base,expt) =  symlength(base) > 1
_latex_needsparen_power(base::Number,expt) =  base < 0

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


function latex_symbol(opt, s::Symbol)
    haskey(symbol_to_latex_table, s) ? symbol_to_latex_table[s] : latex_string(opt, s)
end

# opt is meant to pass options. It is unused at the moment.
function latex_display(x)
    opt = nothing
    MyLaTeXString("\$\$ " * latex_string(opt,x) *  " \$\$")
end

# show will print this correctly
latex_display(mx::Mxpr{:FullForm}) = mx

## Strip the wrapper.... hmmm print InputForm// or something ?
latex_display(mx::Mxpr{:InputForm}) = length(mx) > 0 ? mx[1] : Null


#### LaTeXString

@mkapprule LaTeXString :nargs => 1

@sjdoc LaTeXString """
    LaTeXString(expr)

convert `expr` to a LaTeX string. This is the same string used for display
in the Jupyter notebook. Conversion to LaTeX strings optimized for
mathematical texts is not implemented.
"""

@doap function LaTeXString(x)
    opt = nothing
    latex_string(opt,wrapout(x))
end

# TODO: an all cases we should use takebuf_string(buf) for efficiency rather
# than s *= "new text". Some functions below already use takebuf_string

latex_string(opt, x) = string(x)

## Hmm. real floats print 'Short' without this line. But, complex floats need this.
latex_string(opt, x::AbstractFloat) = string(wrapout(x))

latex_string(opt, s::String) =  latex_text("\"" * s * "\"")

function latex_string(opt, s::Mxpr)
    if getoptype(mhead(s)) == :binary
        return latex_string_binary(opt, s)
    elseif getoptype(mhead(s)) == :infix
        return latex_string_infix(opt, s)
    end
    latex_string_prefix_function(opt, s)
end

# This will not be called if FullForm is the toplevel expression.
# Here, FullForm will be converted to a string, as in Plain style and then wrapped in math mode text macro.
latex_string(opt, mx::Mxpr{:FullForm}) = latex_text(mx)


latex_text(s) =  "\\text{" * string(s)  * "}"

function latex_string_mathop(ins)
    s = string(ins)
    isempty(s) && return ""
    all(islower,s[1]) && return s  # if Head begins lower case print in math italic, e.g. f(x)
    latex_text(s)
end

# ipython inserts a space that we don't want, so we use  \!
# tex2mail also inserts spurious spaces, but prints \! as more spaces
function latex_string_prefix_function(opt, mx::Mxpr)
    buf = IOBuffer()
    print(buf, is_Mxpr(mx,:List) ? ""  : latex_string(opt, mhead(mx)) * " \\! ")
#    print(buf, is_Mxpr(mx,:List) ? ""  : latex_string(opt, mhead(mx)) )  # ipython inserts a space that we don't want
    args = margs(mx)
    print(buf, mhead(mx) == getsym(:List) ? LLISTL : llparen)
    wantparens = mhead(mx) == :List ? false : true
    for i in 1:length(args)-1
        if latex_needsparen(args[i]) && wantparens
           print(buf, llparen)
        end
        print(buf, latex_string(opt, args[i]))
        if latex_needsparen(args[i]) && wantparens
            print(buf, lrparen)
        end
        print(buf, ",")
    end
    if  ! isempty(args) print(buf, latex_string(opt, args[end])) end
    print(buf, mx.head == getsym(:List) ? LLISTR : lrparen)
    String(take!(buf))
end

will_print_minus(x::Number) = x < 0
will_print_minus(x) = false
function will_print_minus(mx::TimesT)
    isempty(mx) && return false
    isa(mx[1],Number) && return mx[1] < 0
end


function latex_string(opt, mx::Mxpr{:Plus})
    terms = margs(mx) # why does terms(mx) not work ?
    if isempty(terms)
        error("show: can't display Plus with no terms.")
    end
    s = latex_string(opt, terms[1])
    for i in 2:length(terms)
        if isa(terms[i],Mxpr{:Minus})
            s *=  " - " * latex_string(opt, (terms[i]).terms[1])
        # following is slightly broken. Fix it later
        # elseif  is_Mxpr(terms[i], :Times) && typeof(terms[i][1])  <:Union{AbstractFloat,Integer} && terms[i][1] < 0
        #     s *= latex_string(opt, terms[i], true)
        else
            if will_print_minus(terms[i])
                opt = Dict( :needsign => true ) ## Signal that we have not yet printed plus or minus. If terms[i] is a fraction, we need to print a sign
                s *= " \\ " * latex_string(opt, terms[i])
            else
                s *= " + " * latex_string(opt, terms[i])
            end
        end
    end
    s
end

function latex_string(opt, mx::Mxpr{:Element})
    buf = IOBuffer()
    if latex_needsparen(mx)  print(buf, llparen)  end
    print(buf, latex_string(opt, mx[1]) * " \\in " * latex_string(opt,mx[2]))
    if latex_needsparen(mx)  print(buf, lrparen) end
    String(take!(buf))
end

function latex_string(opt, mx::Mxpr{:Power})
    buf = IOBuffer()
    latex_needsparen(mx) ? print(buf, llparen, latex_string(opt, base(mx)), lrparen) : print(buf,latex_string(opt, base(mx)))
    print(buf, "^{" * latex_string(opt, exponent(mx)) * "}")
    String(take!(buf))
end

function separate_negative_powers(facs)
    other = Array{Any}(0)
    negpows = Array{Any}(0)
    rationals = Array{Any}(0)
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
    nums = Array{Any}(0)
    dens = Array{Any}(0)
    for x in rationals
        if numerator(x) != 1 push!(nums, numerator(x)) end
        push!(dens, denominator(x))
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

function latex_string_factor(opt, buf::IOBuffer, x, nfacs)
    latex_needsparen(x) && nfacs > 1 ? print(buf, llparen, latex_string(opt, x), lrparen) : print(buf, latex_string(opt, x))
end

# spaceminus is passed around, but does not yet do anything.
# Needs to be fixed and implemented.
minus_or_factor(opt, buf, x, nfacs) = minus_or_factor(opt, buf, x, nfacs, false)
function minus_or_factor(opt, buf, x, nfacs, spaceminus::Bool)
    if x == -1
#        print(buf, spaceminus ? " - " : "-")  # maybe don't even need this
        print(buf, "-")
    else
        latex_string_factor(opt, buf,x, nfacs)  # check for minus space here ?
    end
end

latex_string_factors(opt, buf,facs) = latex_string_factors(opt, buf,facs,false)
function latex_string_factors(opt, buf,facs,  spaceminus)
    minus_or_factor(opt, buf, facs[1], length(facs), spaceminus)
    if length(facs) > 1  print(buf, " \\ ") end  # always will be
    if length(facs) > 2
        for i in 2:length(facs)-1
            minus_or_factor(opt, buf, facs[i], length(facs))
            print(buf, " \\ ")
        end
    end
    if length(facs) > 1  minus_or_factor(opt, buf, facs[end], length(facs)) end
end

# LaTeX does not put spaces between factors. But, Symata has multicharacter symbols, so we need spaces to distinguish them
latex_string(opt, mx::Mxpr{:Times})  = latex_string(opt, mx, false)
function latex_string(opt, mx::Mxpr{:Times}, spaceminus::Bool)
    (other, negpows, rationals) = separate_negative_powers(factors(mx))
    buf = IOBuffer()
    if ( isempty(negpows) && isempty(rationals) )
        latex_string_factors(opt, buf, factors(mx), spaceminus)
    else
        (nums,dens) = get_nums_dens(other,negpows, rationals)
        if length(nums) == 0
            nums = Any[1]
        end
        if isa(opt,Dict) && get(opt,:needsign, false)  ## TODO: detect leading minus sign in numerator and pull it out. At least it is correct now
            print(buf," + ")
            delete!(opt,:needsign)
        end
        print(buf,"\\frac{")
        latex_string_factors(opt, buf,nums)
        print(buf,  "}{")
        latex_string_factors(opt, buf,dens)
        print(buf,"}")
    end
    String(take!(buf))
end

function latex_string(opt, x::WORational)
    r = x.x
    latex_string(opt, r)
end

function latex_string(opt, z::Rational{T}) where T<:Real
    "\\frac{" * latex_string(opt, numerator(z)) * "}{" * latex_string(opt, denominator(z)) * "}"
end

function latex_string(opt, x::WOComplexRational)
    z = x.x
    s = ""
    if real(z) != 0
        s *= latex_string(opt, real(z)) * " + "
    end
    if imag(z) == 1
        s *= Ilatexstring()
    else
        s *=   latex_string(opt, imag(z)) *  " \\ " * Ilatexstring()
    end
    s
end

# We display real part if it is 0.0
function latex_string(opt, x::WOComplexReal)
    z = x.x
    latex_string(opt, real(z)) * " + " * latex_string(opt, imag(z)) * Ilatexstring()
end

# Do not display real part if it is 0
function latex_string(opt, x::WOComplexInteger)
    z = x.x
    s = ""
    if real(z) != 0
        s *= latex_string(opt, real(z)) * " + "
    end
    if imag(z) == 1
        s *= Ilatexstring()
    else
        iz = imag(z)
        if iz == -1
            s *= "-"
        else
            s *= latex_string(opt, iz)
        end
        s *= Ilatexstring()
    end
    s
end

function  latex_string(opt, mx::Mxpr{:DirectedInfinity})
    if length(mx) == 0
        latex_string_mathop(:ComplexInfinity)
    elseif mx[1] == 1
        Infinitylatexstring()
    elseif mx[1] == -1
        "-" * Infinitylatexstring()
    else
        latex_string_mathop(:DirectedInfinity) * llparen * latex_string(opt, mx[1]) * lrparen
    end
end

function latex_string(opt, mx::Mxpr{:Subscript})
    latex_string(opt, mx[1]) * "_{" *
    join([latex_string(opt, mx[i]) for i in 2:length(mx)], ",") *
    "}"
end

latex_string(opt, s::SSJSym) = latex_string(opt, symname(s))

function latex_string(opt, s::WOSymbol)
    latex_string(opt, s.x)
end

function latex_string(opt, s::Symbol)
#    ms = mtojsym(s)
    ms = s
    haskey(symbol_to_latex_table, ms) ? symbol_to_latex_table[ms] : latex_string_mathop(ms)
end

function latex_string(opt, v::WOBool)
    v.x ? latex_text(:True) : latex_text(:False)
end

function latex_string(opt, t::DataType)
    latex_text(t)
end

function latex_string(opt, mx::Mxpr{:Comparison})
    join([latex_string(opt, outsym(x)) for x in margs(mx)], " \\, " )
end

function latex_string_binary(opt, mx::Mxpr)
    if length(mx) != 2
        return latex_string_prefix_function(opt, mx)
    else
        s = ""
        opstr = outsym(mhead(mx))
        lop = mx[1]
        if latex_needsparen(lop)
            s *= llparen * latex_string(opt, lop) * lrparen
        else
            s *= latex_string(opt, lop)
        end
        s *= latex_symbol(opt, opstr)
        rop = mx[2]
        if  latex_needsparen(rop)
            s *= llparen * latex_string(opt, rop) * lrparen
        else
            s *= latex_string(opt, rop)
        end
    end
    s
end


# Times is handled above. This needs to be cleaned up.
latex_string_infix(opt, s) = s
function latex_string_infix(opt, mx::Mxpr)
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
        s *= latex_string(opt, arg)
        if np
            s *= lrparen
        end
        s *= latex_string(opt, sepsym)
    end
    if ! isempty(args)
        if latex_needsparen(args[end])
            np = true
            s *= llparen
        else
            np = false
        end
        s *= latex_string(opt, args[end])
        if np
            s *= lrparen
        end
    end
    s
end


function latex_string(opt, mx::Mxpr{:Blank})
    s = latex_text("_")
    if length(mx) > 0
        s *= latex_string(opt, mx[1])
    end
    s
end

function latex_string(opt, mx::Mxpr{:BlankSequence})
    s = latex_text("__")
    if length(mx) > 0
        s *= latex_string(opt, mx[1])
    end
    s
end

function latex_string(opt, mx::Mxpr{:BlankNullSequence})
    s = latex_text("___")
    if length(mx) > 0
        s *= latex_string(opt, mx[1])
    end
    s
end

function latex_string(opt, mx::Mxpr{:Pattern})
    s = latex_string(opt, mx[1])
    if is_Mxpr(mx[2],:Blank)
        s *= latex_string(opt, mx[2])
    else
        s *= " \\text{::}(" * latex_string(opt, mx[2]) * ")"
    end
    s
end

function latex_string(opt, qs::Qsym)
    s = ""
    if qs.context != CurrentContext.name
        s *= latex_string(opt, qs.context) * "."
    end
    s *= latex_string(opt, qs.name)
end

# For Holdform, arguments are not evaluated, as in Hold.
# But, in addition, Holdform is not printed.
latex_string(opt, mx::Mxpr{:HoldForm}) = latex_string(opt, mx[1])


function integral_limits_string(opt, varrange)
    if length(varrange) == 3
        return "\\int_{" * latex_string(opt, varrange[2]) * "}^{" * latex_string(opt, varrange[3]) * "}"
    else
        return "\\int "
    end
end

function infinitessimal_string(opt, varrange)
    return " \\mathbb{d}" * latex_string(opt, varrange[1])
end

## FIXME, print the conditions somehow
function latex_string(opt, mx::Mxpr{:Integrate})
    buf = IOBuffer()
    integrand = latex_string(opt, mx[1])
    nconds = mxpr_count_heads(mx,:Rule)  # won't catch RuleDelayed
    nn = length(mx) - nconds
    if nn == 2  # single integration
        varrange = mx[2]
        print(buf, integral_limits_string(opt, varrange))
        print(buf, integrand * " \\, " * infinitessimal_string(opt, varrange))
    else
        for i in 2:nn
            varrange = mx[i]
            print(buf, integral_limits_string(opt, varrange))
            print(buf, " " * infinitessimal_string(opt, varrange))
        end
        print(buf, " \\, " * integrand)
    end
    return String(take!(buf))
end

function sum_limits_string(opt, varrange)
    if length(varrange) == 3
        return "\\sum_{" * latex_string(opt, varrange[1]) * "=" * latex_string(opt, varrange[2]) * "}^{" * latex_string(opt, varrange[3]) * "}"
    else
        return "\\sum " # This probably should not happen
    end
end

function latex_string(opt, mx::Mxpr{:Sum})
    buf = IOBuffer()
    summand = latex_string(opt, mx[1])
    if length(mx) == 2  # single summation
        varrange = mx[2]
        print(buf, sum_limits_string(opt, varrange))
        print(buf, summand)
        return String(take!(buf))
    else
        args = margs(mx)
        sargs = args[2:end]
        print(buf, " \\sum ")
        print(buf, "_{\\substack{" * join([ latex_string(opt, v[1]) * "=" * latex_string(opt, v[2]) for v in sargs], " \\\\ ") * "}}")
        ul = sargs[1][3]
        if all( x -> x[3] == ul , sargs)
            print(buf, "^{" * latex_string(opt, sargs[1][3]) * "}")   # only display one upper limit if they are all the same
        else
            print(buf, "^{" * join([latex_string(opt, v[3]) for v in sargs], ",") * "}")
        end
        print(buf, summand)
        return String(take!(buf))
    end
end

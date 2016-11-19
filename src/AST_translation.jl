## Translate Expr to Mxpr

# Input lines are lexed and parsed by the Julia parser into an AST stored
# in the Julia Expr type. The code here reinterprets the Expr into an Symata AST,
# which is stored in the Julia type Mxpr.

## NB: Checking for help characters, ?,  is done in doc.jl

# At the command line h"topic" prints help
macro h_str(s)
    try
        check_autoload(Symbol(strip(s)))
    end
    reg = eval(Expr(:macrocall, Symbol("@r_str"), strip(s), "i"))
    print_matching_topics(reg)
    :Null
end

## Julia already has some of these. We need to consolidate

macro bf_str(s)
    parse(BigFloat,s)
end

macro bi_str(s)
    parse(BigInt,s)
end

macro BI_str(s)
    parse(BigInt,s)
end


macro BF_str(s)
    parse(BigFloat,s)
end

macro jul(ex)
    extomx(Expr(:call, :J, ex))
end

# Complicated:
# 1. preprocess :> to .>, because :> cannot be parsed
#  on output write .> as :>, so that it can be read again. But, .> also works as input
# 2. We want -> for Rule. But, IIRC, the precedence required parenthesizing more than we want.
#   So, we use =>. We would then like to preprocess -> to => on input and the reverse on output.
#   But, we want to be able to write:
# symata > f = :( (x) -> x^2 ), which would be rewritten f = :( (x) => x^2 ).
# We could, when analyzing the parsed :( (x) => x^2 ), convert => back to ->, but I don't want to risk
# messing the precedence of ->, which is asymmetric and special. Mostly because I guess this example will
# be one of the most common uses of :( expr ), to wrap a Julia function.
# So... what to do. Let us try simply breaking with Mma and using => for Rule.
# Perhaps we can also allow -> for Rule as well.
# We would have to do more than simple replacement form string preprocessing. I don't want to do that.
const PREPROCESS_SYMBOL_TRANSLATION = Dict{Any,Any}(
#                                                    "->" => "=>",   #disable
                                                    ":>" => ".>",
                                                    "^:=" => "&=")


const REVERSE_PREPROCESS_SYMBOL_TRANSLATION = Dict{Symbol,Symbol}(
#                                                                  :Rule => :(->), # disabled
                                                                  :Rule => :(=>),
                                                                  :RuleDelayed => Symbol(":>"),
                                                                  :UpSetDelayed => Symbol("^:="))


# Symata language expressions are first processed here (at
# least interactvley now. The string is rewritten and then
# parse to an AST which is the input to exfunc. This is
# mostly done to rewrite the string into legal Julia syntax.
# Now, we only look for the help symbol "?"
function sjpreprocess_interactive(line::AbstractString)
    if length(line) > 1 && line[1] == '?'             # User wants documentation
        line = "?," * line[2:end] # We add a comma so that the julia parse will accept it.
    end
    sjpreprocess_string(line)
end

function sjpreprocess_string(line::AbstractString)
    for (k,v) in PREPROCESS_SYMBOL_TRANSLATION
        line = replace(line, k, v)
    end
    return line
end


#### extomx  translate a Julia expression ex, to an SJuia expression mx

function extomx{T<:Integer}(x::T)
    getkerneloptions(:bigint_input) ? BigInt(x) : x
end

function extomx{T<:AbstractFloat}(x::T)
    getkerneloptions(:bigfloat_input) ? BigFloat(rationalize(x)) : x
end

extomx(x) = x
# This system needs to be rationalized.
# Comment out lines are no longer needed
# We can move the rest out of here too
# jtomsym is what picks these out.
function extomx(s::Symbol)
    s == :I && return complex(0,1)
    s == :∑ && return :Sum
    s == :True && return true
    s == :False && return false

    ss = string(s)
    if contains(ss,"_")  # Blanks used in patterns
        return parseblank(ss)
    else
        return getsym(jtomsym(s)) # Maybe translate the symbol
    end
end

# Underscore is not allowed in symbols. Instead,
# they signify part of a pattern.
# TODO. This is the only way we construct patterns at the moment.
# But the name of the pattern, the first arg, can refer to not just a Blank,
# but an entire pattern expression. Mma does this with a colon.
# f:(_^_), or f:_^_  --> Pattern[x, Power[Blank[], Blank[]]].
# We are using colon for Span. I don't know what we can do.
## Nov 2016. Moved away from :( ) for Julia expressions. Will remove it soon.
## This frees :( ) and :s for other things
# Mma does  Fullform[a::b] --> MessageName[a, "b"]. We could take
# :: for Pattern
function parseblank(s::AbstractString)
    a = split(s,['_'], keep=true)
    length(a) > 4 && error("parseblank: Illegal Pattern expression '$s'")
    if length(a) == 2
        blanktype = :Blank
        (blankhead,blankname) = (a[1],a[2])
    elseif length(a) == 3
        a[2] != "" && error("parseblank: Illegal Pattern expression '$s'")
        blanktype = :BlankSequence
        (blankhead,blankname) = (a[1],a[3])
    else
        a[2] != "" && a[3] != "" && error("parseblank: Illegal Pattern expression '$s'")
        blanktype = :BlankNullSequence
        (blankhead,blankname) = (a[1],a[4])
    end
    if length(blankname) == 0
        blank = mxpr(blanktype)
    else
        blank = mxpr(blanktype,Symbol(blankname))
    end
    length(blankhead) == 0 && return blank
    mxpr(:Pattern,Symbol(blankhead),blank)
end

# Pattern::argrx: Pattern called with 3 arguments; 2 arguments are expected.
function parsepattern(ex)
    mxpr(:Pattern,map(extomx,ex.args))
end

function extomxarr!(ain,aout)
    for x in ain
        push!(aout,extomx(x))
    end
end


# We currently have two kinds of symbols.
# 1) Those tagged by Julia symbols, forming a single namespace
# 2) Those tagged by a.b or Qsym(a,b) in which a is a context or namespace.
# The latter is an experiment, not well integrated with the rest of the language.
# Interactively, the second argument is an Expr.
# Read from a file, it is a QuoteNode. Don't know why.
function parse_qualified_symbol(ex::Expr)
    args = ex.args
    length(args) != 2 && error("extomx: We can only handle context qualifications like this: a.b")
    typeof(args[1]) == Symbol || error("extomx: expecting symbol as first argument to context qualification")
    a2 = args[2]
    if typeof(a2) != Expr
        if typeof(a2) != QuoteNode
            dump(ex)
            error("extomx: error parsing second argument $a2 of ", ex, " , Expected an Expr or QuoteNode, got ", typeof(a2))
        end
        qsym = Qsym(args[1],a2.value)
        return qsym
    end
    typeof(a2.head) != Symbol && error("extomx: error parsing second argument of ", ex, ". Expected a symbol.")
    a2.head != :quote && error("extomx: error parsing second argument of ", ex, ". Expected symbol 'quote'.")
    length(a2.args) == 1 || error("extomx: error parsing second argument of ", ex, ". Expected one arg in quote node.")
    typeof(a2.args[1]) == Symbol || error("extomx: second argument of context qualification must be a symbol, got ", typeof(a2.args[1]))
    qsym = Qsym(args[1],a2.args[1])
    return(qsym)
end

function parse_quoted(ex::Expr,newa)
    # Quotes are wrapped in Jxpr which is evaluated by Julia eval()
    symwarn(":( ) for Julia code is deprecated. Use J( ) instead")
    head = :Jxpr           # This allows running Julia code from within Symata.
    push!(newa,ex.args[1]) # We evaluate the expression only whenever the Jxpr is evaled
                           # But, this is the same effect as evaling ex
    return head
end

## TODO: handle head :parameters here.
## Check if the first argument (ex.args[2]) is a :parameters expression.
## If so, rewrite ex. We probably want f(a,b;c,d;e,f) to be three compound expressions with two expressions each.
function parse_call(ex,newa)
    a = ex.args
    nhead = extomx(ex.args[1])
    if nhead == :J
        nhead = :Jxpr
        push!(newa,a[2]) # will be interpreted as Julia code, so don't translate it.
    else
        @inbounds for i in 2:length(a) push!(newa,extomx(a[i])) end
    end
    mxpr(nhead,newa)
end

## Main translation routine
# We use Julia for lexing/parsing. But we change the semantics:
# sometimes a little, sometimes a lot.
function extomx(ex::Expr)
    newa = newargs()
    local nhead::Any
    ex = rewrite_expr(ex)
    isa(ex,Expr) || return ex  # may be a rational
    ohead = ex.head
    a = ex.args
    # We usually set the head and args in the conditional and construct Mxpr at the end
    if iscall(ex) return parse_call(ex,newa)
    elseif ohead == :block && typeof(a[1]) == LineNumberNode  # g(x_Integer) = "int". julia finds line number node in rhs.
        return extomx(a[2])
    elseif ohead == :line return nothing # Ignore line number. part of misinterpretation of g(x_Integer) = "int".
    elseif haskey(JTOMSYM,ohead)
        nhead = JTOMSYM[ohead]
        check_autoload(nhead)
        extomxarr!(a,newa)
    elseif ohead == :kw  # Interpret keword as Set, but Expr is different than when ohead == :(=)
        nhead = :Set
        extomxarr!(a,newa)
    elseif ohead == :(::)
        return parsepattern(ex)
    elseif ohead == :(:) # Eg the colon here: g(x_Integer:?(EvenQ)) := x
        if length(a) == 2
            if isa(a[1], Symbol) && isa(a[2], Expr) &&   # FIXME use isa() here
                (a[2].args)[1] == :(?)
                ptargs = a[2].args
                length(ptargs) != 2 && error("extomx: too many args to PatternTest")
                pt = ptargs[2]
                if isa(pt,Expr)  # assume it is a Function
                    if pt.head == :call && length(pt.args) > 1 && pt.args[1] == :J
                        pt = eval(eval(pt.args[2]))
                    else
                        pt = eval(eval(pt)) # first eval gets Symbol from Expr, Second gives Function.
                    end
                end
                isa(pt,Symbol) || isa(pt,Function) || typeof(pt) <: Function ||
                        error("extomx: argument to PatternTest must be a Symbol or a Function")
                nhead = :PatternTest
                push!(newa,extomx(a[1]),pt)
            else
                lhs = extomx(a[1])
                if typeof(lhs) <: Union{BlankXXX, Mxpr{:Pattern}}  # This is probably not good enough!
                                                                   # Maybe : represents too many things.
                    return mxpr(:Optional,lhs,extomx(a[2]))
                end
                nhead = :Span      # Span syntax like:  a(::10), a(1::2), etc. clash with use of colon above
                extomxarr!(a,newa) # We may have to change the syntax
            end
        else
            nhead = :Span
            extomxarr!(a,newa)
        end
    elseif ohead == :(.)  # don't need parens, but this is readable
        return parse_qualified_symbol(ex)
    elseif ohead == :quote
        nhead = parse_quoted(ex,newa)
    elseif ohead == :macrocall
        return eval(ex )  # is this exactly what we want ?
    elseif ohead == :string
        nhead = :StringInterpolation
        extomxarr!(a,newa)
    else
        dump(ex)
        error("extomx: No translation for Expr head '$(ohead)' in $ex")
    end
    mx = mxpr(nhead,newa)  # Create the Mxpr
end

"""
    iscall(ex::Expr)

true if `ex` has head `:call`

    iscall(ex::Expr, op::Symbol)

true if the first argument, the operator is `op`.

    iscall(ex::Expr, op::Symbol, len::Int)

true if number of args (including op) is `len`

    iscall(ex::Expr, len::Int)

true if number of args (including op) is `len` for any `op`.
"""
iscall(ex::Expr) = ex.head == :call
# is ex a call with operator op ?
iscall(ex::Expr, op::Symbol) = ex.head == :call && ex.args[1] == op
# is ex a call with operator op and len args (including the op) ?
iscall(ex::Expr, op::Symbol, len::Int) = ex.head == :call && ex.args[1] == op && length(ex.args) == len
# is ex a call with len args (including the op) ?
iscall(ex::Expr, len::Int) = iscall(ex) && length(ex.args) == len

# is of the form    a op b , where op is <,>, etc.  i.e. this is  not a chained comparison
is_single_comparison(ex::Expr, op::Symbol) = ex.head == :comparison && length(ex.args) == 3 && ex.args[2] == op

# We check for :call repeatedly. We can optimize this later.
is_binary_minus(ex::Expr) = iscall(ex, :-, 3)
is_unary_minus(ex::Expr) = iscall(ex, :-, 2)
# number of args != 3 will pass through. But probably can't be used
is_division(ex::Expr) = iscall(ex, :/,3)
is_power(ex::Expr) = iscall(ex, :^)

is_sqrt(ex::Expr) = iscall(ex,:Sqrt)

# save time, don't make Symata meval convert //(a,b) to rational
# we should also detect sums/products of like numbers, etc. and
# combine them. No* Sometimes we want to Hold expressions involving numbers.
function is_rational(ex::Expr)
    iscall(ex, :(//), 3) && is_Number(ex.args[2]) &&
        is_Number(ex.args[3])
end

function is_complex(ex::Expr)
    iscall(ex, :Complex, 3) && is_Number(ex.args[2]) &&
        is_Number(ex.args[3])
end

# In extomx, we first rewrite some Math to canonical forms
# a - b  -->  a + (-b)
rewrite_binary_minus(ex::Expr) = Expr(:call, :+, ex.args[2], Expr(:call,:(-),ex.args[3]))
#  - b  -->  -1 * b
rewrite_unary_minus(ex::Expr) = ex.args[2] == :Inf ? -Inf : Expr(:call, :*, -1 , ex.args[2])
# a / b -->  a * b^(-1)
rewrite_division(ex::Expr) = Expr(:call, :*, ex.args[2], Expr(:call,:^,ex.args[3],-1))

# not need in julia v0.4, but the parser has changed
# ==(a,b) --> comparison  a,==,b
# also,  '<:' is the head in v0.5. We don't yet handle this
#rewrite_to_comparison(ex::Expr) = Expr(:comparison, ex.args[2], ex.args[1], ex.args[3])

function rewrite_to_comparison(ex::Expr)
    op = ex.args[1]
    if haskey(comparison_translation,op)
        return Expr(:call, get(comparison_translation,op,:nothing), ex.args[2], ex.args[3])
    end
    return  Expr(:comparison, ex.args[2], ex.args[1], ex.args[3])
end

# There is no binary minus, no division, and no sqrt in Mxpr's.
# Concrete example: a - b --> a + -b.
# We definitely need to dispatch on a hash query, or types somehow
# Other rewrites needed, but not done.
function rewrite_expr(ex::Expr)
    for i in 1:length(ex.args)
        x = ex.args[i]
        if isa(x,Expr) && x.head == :macrocall
            ex.args[i] = eval(x)
        end
        if haskey(INSYMTRANS,x)
            ex.args[i] = INSYMTRANS[x]
        end
    end
    if iscall(ex) && length(ex.args) > 0 && is_comparison_symbol(ex.args[1])
        ex = rewrite_to_comparison(ex)
    elseif is_single_comparison(ex, :(.>))    # julia 0.4 parser does this. In 0.5, this is already a call
        return Expr(:call, :(.>), ex.args[1], ex.args[3])
    elseif is_unary_minus(ex)    #  - b --> -1 * b
        ex = rewrite_unary_minus(ex)
    elseif is_binary_minus(ex)  #  a - b --> a + -b.
        ex = rewrite_binary_minus(ex)
    elseif is_division(ex) # a / b --> a + b^(-1)
        ex = rewrite_division(ex)
    elseif iscall(ex, :Exp, 2)  # Exp(x) --> E^x
        ex = Expr(:call, :^, :E, ex.args[2])
    elseif iscall(ex,:Sqrt,2) # This should happen at Mxpr level, and be optimized
        ex = Expr(:call, :^, ex.args[2], Expr(:call,:(//), 1,2))
    elseif is_rational(ex)
        return eval(ex)  # TODO: don't do eval, use //
    elseif is_complex(ex)
        (real,imag) = (ex.args[2],ex.args[3])
        if is_Real(real) && is_Real(imag)
            return complex(real,imag)
        else
            return ex
        end
    end
    return ex
end

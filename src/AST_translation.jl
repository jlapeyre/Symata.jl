## Translate Expr to Mxpr

# Input lines are lexed and parsed by the Julia parser into an AST stored
# in the Julia Expr type. The code here reinterprets the Expr into an Symata AST,
# which is stored in the Julia type Mxpr.

## NB: Checking for help characters, ?,  is done in doc.jl

# At the command line h"topic" prints help
macro h_str(s)
    try
        check_autoload(Symbol(strip(s)))
    catch
    end
    reg = eval(Expr(:macrocall, Symbol("@r_str"), strip(s), "i"))
    print_matching_topics(reg)
    :Null
end

## Julia already has some of these. We need to consolidate

macro BI_str(s) parse(BigInt,s) end
macro BF_str(s) parse(BigFloat,s) end

"""
    symparsestring(s::String)

parses `s` into one or more Julia expressions, translates each one to a Symata expression,
and returns an array of the Symata expressions.

Note that the phrases *Symata expression* and *Julia expression* here include numbers, symbols, etc.
"""
function symparsestring(s)
    mxprs = Array{Any}(undef, 0)
    s = sjpreprocess_string(s)
    i = 1
    local sjretval
    local expr
    while ! (i > ncodeunits(s))
        symata_syntax_deprecation_warnings(false) do
            expr, i = Meta.parse(s,i)
        end
        sjretval =
            try
                mx = extomx(expr)
                push!(mxprs, mx)
            catch e
                symprintln("Reading string: got error ", e)
            end
    end
    return mxprs
end


## Complicated:
## 1. preprocess :> to .>, because :> cannot be parsed
##  on output write .> as :>, so that it can be read again. But, .> also works as input
## 2. We want -> for Rule. But, IIRC, the precedence required parenthesizing more than we want.
##   So, we use =>. We would then like to preprocess -> to => on input and the reverse on output.
##   But, we want to be able to write:
## symata > f = :( (x) -> x^2 ), which would be rewritten f = :( (x) => x^2 ).
## We could, when analyzing the parsed :( (x) => x^2 ), convert => back to ->, but I don't want to risk
## messing the precedence of ->, which is asymmetric and special. Mostly because I guess this example will
## be one of the most common uses of :( expr ), to wrap a Julia function.
## So... what to do. Let us try simply breaking with Mma and using => for Rule.
## Perhaps we can also allow -> for Rule as well.
## We would have to do more than simple replacement form string preprocessing. I don't want to do that.
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
# parse to an AST which is the input to symataevaluate. This is
# mostly done to rewrite the string into legal Julia syntax.
# Now, we only look for the help symbol "?"
const MAGIC_HELP_QUERY_STRING = "HELPQUERY!!"
const MAGIC_HELP_QUERY_SYMBOL = Symbol(MAGIC_HELP_QUERY_STRING)
function sjpreprocess_interactive(line::AbstractString)
    if length(line) > 1 && first(line) == '?'  # User wants documentation
        line =  MAGIC_HELP_QUERY_STRING * ", " *
            SubString(line, 2, lastindex(line)) # We add a comma so that the julia parse will accept it.
    end
    sjpreprocess_string(line)
end

function sjpreprocess_string(line::AbstractString)
    for (k,v) in PREPROCESS_SYMBOL_TRANSLATION
        line = replace(line, k => v)
    end
    return line
end

## Underscore is not allowed in symbols. Instead,
## they signify `Blank`s.
##
## TODO. This is the only way we construct patterns at the moment.
## But the name of the pattern, the first arg, can refer to not just a Blank,
## but an entire pattern expression. Mma does this with a colon.
## f:(_^_), or f:_^_  --> Pattern[x, Power[Blank[], Blank[]]].
## We are using colon for Span. I don't know what we can do.
## Nov 2016. Moved away from :( ) for Julia expressions. Will remove it soon.
## This frees :( ) and :s for other things
## Mma does  Fullform[a::b] --> MessageName[a, "b"]. We could take
## :: for Pattern
function parseblank(s::AbstractString)
    a = split(s,['_'], keepempty=true)
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
    blank = isempty(blankname) ? mxpr(blanktype) : mxpr(blanktype,Symbol(blankname))
    return isempty(blankhead) ? blank : mxpr(:Pattern,Symbol(blankhead),blank)
end

# Pattern::argrx: Pattern called with 3 arguments; 2 arguments are expected.
function parsepattern(ex)
    nargs = newargs(length(ex.args))
    mxpr(:Pattern,map!(extomx,nargs,ex.args))
end

extomxarr!(ain, aout) =  foreach(x -> push!(aout, extomx(x)), ain)

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
        return Qsym(args[1],a2.value)
    end
    typeof(a2.head) != Symbol && error("extomx: error parsing second argument of ", ex, ". Expected a symbol.")
    a2.head != :quote && error("extomx: error parsing second argument of ", ex, ". Expected symbol 'quote'.")
    length(a2.args) == 1 || error("extomx: error parsing second argument of ", ex, ". Expected one arg in quote node.")
    typeof(a2.args[1]) == Symbol || error("extomx: second argument of context qualification must be a symbol, got ", typeof(a2.args[1]))
    return Qsym(args[1],a2.args[1])
end

function parse_quoted(ex::Expr,newa)
    # Quotes are wrapped in Jxpr which is evaluated by Julia eval()
    symwarn(":( ) for Julia code is deprecated. Use J( ) instead")
    head = :Jxpr           # This allows running Julia code from within Symata.
    push!(newa,ex.args[1]) # We evaluate the expression only whenever the Jxpr is evaled
                           # But, this is the same effect as evaling ex
    return head
end

function rewrite_Jxpr!(a, newa)
    for i in 2:length(a)  ## TODO: use `rest` iterator instead of indices
        x = a[i]
        if isa(x,Expr) && x.head == :kw  # in  J( x =  1) ,  x=1 is interpeted as keyword, value pair. But the user means to do assignment
            x.head = :(=)                # will this break legit uses of keywords ?
        end
        push!(newa, a[i]) # will be interpreted as Julia code, so don't translate it.
    end
    return mxpr(:Jxpr, newa)
end

function parse_pattern(pattern_expr)
    if isa(pattern_expr, Expr)  # assume it is a Function
        if pattern_expr.head == :call && length(pattern_expr.args) > 1 && pattern_expr.args[1] == :J
            #pattern = Core.eval(Symata, pattern_expr.args[2])
            pattern = eval(pattern_expr.args[2])
        else
            pattern = eval(eval(pattern_expr)) # first eval gets Symbol from Expr, Second gives Function.
        end
    else
        pattern = pattern_expr
    end
    isa(pattern, Symbol) || isa(pattern, Function) || typeof(pattern) <: Function ||
        error("extomx: argument to PatternTest must be a Symbol or a Function")
    return pattern
end

# FIXME: refactor after testing
function parse_colon!(a, newa)
    local mxhead
    if length(a) == 2
        lhs = extomx(a[1])
        if typeof(lhs) <: Union{BlankXXX, Mxpr{:Pattern}}  # This is probably not good enough!
            # Maybe : represents too many things.
            return mxpr(:Optional, lhs, extomx(a[2]))
        end
        mxhead = :Span      # Span syntax like:  a(::10), a(1::2), etc. clash with use of colon above
        extomxarr!(a, newa) # We may have to change the syntax
    else
        mxhead = :Span
        extomxarr!(a, newa)
    end
    return mxpr(mxhead, newa)
end

## TODO: handle head :parameters here.
## Check if the first argument (ex.args[2]) is a :parameters expression.
## If so, rewrite ex. We probably want f(a,b;c,d;e,f) to be three compound expressions with two expressions each.
function parse_call!(ex, newa)
    a = ex.args
    operator = a[1]
    if operator == :(:)
        return parse_colon!(@view(a[2:end]), newa)
    end
    mxhead = extomx(operator)
    if mxhead == :J
        return rewrite_Jxpr!(a, newa)
    end
    @inbounds for i in 2:length(a) push!(newa, extomx(a[i])) end
    return mxpr(mxhead, newa)
end

## Translate this separately otherwise the body always inserts a CompoundExpression
function parse_function(ex)
    mxpr(:Function, extomx(ex.args[1]), extomx(ex.args[2].args[2]))
end

function parse_macrocall(ex, newa)
    fullmacroname = string(ex.args[1])
    r = r"^@(.*)_?cmd$"
    m = match(r, fullmacroname) # check for x`foo` (cmd macro)
    m == nothing && return(eval(ex)) # Not a cmd macro
    macroname_with_underscore = m.captures[1]
    macroname = SubString(macroname_with_underscore, 1, lastindex(macroname_with_underscore)-1)
#    macroname = macroname_with_underscore[1:lastindex(macroname_with_underscore)-1]
#    @show macroname
    cmd_string = ex.args[3]
#    @show cmd_string
    pattern_expr = Meta.parse(cmd_string)
    pattern = parse_pattern(pattern_expr)
    push!(newa, extomx(Symbol(macroname)), pattern)
    return mxpr(:PatternTest, newa)
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
is_sqrt(ex::Expr) = iscall(ex, :Sqrt)
# This may need to be refined
is_range(ex::Expr) = iscall(ex, :(:), 3)
is_range(args...) = false

"""
    range_start(ex::Expr)

return the start value assume `ex` evaluates to a `Range`.
"""
range_start(ex::Expr) = ex.args[2]
range_stop(ex::Expr) = ex.args[3]

## Needed for Julia v0.6.0. Pairs are parsed differently in different
## versions of Julia (as are many expressions).
## The change for Pair is here: https://github.com/JuliaLang/julia/pull/20327
## Goal is to encapsulate interpretation in one place. Here.
# """
#     is_pair(ex)
# """
# function is_pair(ex::Expr)
#     if iscall(ex, :(=>), 3) # In Julia v0.6
#         return true
#     elseif ex.head == :(=>) && length(ex.args) == 2
#         return true
#     end
#     return false
# end

# """
#     rewrite_pair(ex::Expr)
# Input is v6.0 Expr for :( a => b ). Output
# is v5.0 Expr for :( a => b ).
# """
# function rewrite_pair(ex::Expr)
#     Expr(:(=>),ex.args[2],ex.args[3])
# end

## save time, don't make Symata meval convert //(a,b) to rational
## we should also detect sums/products of like numbers, etc. and
## combine them. No* Sometimes we want to Hold expressions involving numbers.
function is_rational(ex::Expr)
    iscall(ex, :(//), 3) && isa(ex.args[2],Number) &&
        isa(ex.args[3],Number)
end

function is_complex(ex::Expr)
    iscall(ex, :Complex, 3) && isa(ex.args[2],Number) &&
        isa(ex.args[3],Number)
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

rewrite_comparison(ex::Expr) = ex
## The following does what we want. e.g. we get Less(x,y,z)
## But, we have not yet implemented the logic to reduce the expression.
## So, we disable this function and fall back to the older Comparison, for which
## we do have logic
# function rewrite_comparison(ex::Expr)
#     args = ex.args
#     ops = @view args[4:2:end]
#     op1 = args[2]
#     if all( x -> x == op1,  ops)
#         Expr(:call,get(comparison_translation,op1,op1),args[1:2:end]...)
#     else
#         ex
#     end
# end

# There is no binary minus, no division, and no sqrt in Mxpr's.
# Concrete example: a - b --> a + -b.
# We definitely need to dispatch on a hash query, or types somehow
# Other rewrites needed, but not done.

"""
    rewrite_expr(ex::Expr)

Preprocess `ex` to prepare for conversion to type `Mxpr`.
"""
function rewrite_expr(ex::Expr)
    for i in 1:length(ex.args)
        x = ex.args[i]
        if isa(x,Expr) && x.head == :macrocall
#            ex.args[i] = eval(x)
            ex.args[i] = parse_macrocall(x, newargs())
        end
        if haskey(INSYMTRANS,x)
            ex.args[i] = INSYMTRANS[x]
        end
    end
    if iscall(ex) && length(ex.args) > 0 && is_comparison_symbol(ex.args[1])
        ex = rewrite_to_comparison(ex)
    elseif is_single_comparison(ex, :(.>))    # julia 0.4 parser does this. In 0.5, this is already a call
        return Expr(:call, :(.>), ex.args[1], ex.args[3])
    elseif ex.head == :comparison
        ex = rewrite_comparison(ex)
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
    elseif iscall(ex, :(=>), 3)  # Convert v0.6 Pair to v0.5 Pair.
        ex = Expr(:(=>), ex.args[2], ex.args[3])
    elseif is_rational(ex)
        return eval(ex)  # TODO: don't do eval, use //
    elseif is_complex(ex)
        (real,imag) = (ex.args[2],ex.args[3])
        return isa(real,Real) && isa(imag,Real) ? complex(real,imag) : ex
    end
    return ex
end

## extomx

## Main translation routine
# We use Julia for lexing/parsing. But we change the semantics:
# sometimes a little, sometimes a lot.
"""
    extomx(expr::Expr)

Translate a Julia expression `expr`, to a Symata expression of
type `Mxpr{T}`, where `T` is typically determined by the operator
in `expr`.
"""
function extomx(ex::Expr)
    newa = newargs()
    local mxhead::Any
    ex = rewrite_expr(ex)
    isa(ex,Expr) || return ex  # may be a rational
    ohead = ex.head
    a = ex.args
    # We usually set the head and args in the conditional and construct Mxpr at the end
    if iscall(ex) return parse_call!(ex, newa)
    elseif ohead == :block && typeof(a[1]) == LineNumberNode  # g(x_Integer) = "int". julia finds line number node in rhs.
        return extomx(a[2])
    elseif ohead == :line return nothing # Ignore line number. part of misinterpretation of g(x_Integer) = "int".
    elseif ohead == :(->) return parse_function(ex)
    elseif haskey(JTOMSYM,ohead)
        mxhead = JTOMSYM[ohead]
        check_autoload(mxhead)
        extomxarr!(a,newa)
    elseif ohead == :kw  # Interpret keword as Set, but Expr is different than when ohead == :(=)
        mxhead = :Set
        extomxarr!(a,newa)
    elseif ohead == :(::)
        return parsepattern(ex)
    elseif ohead == :(:) # Eg the colon here: g(x_Integer:?(EvenQ)) := x
        error("This should never happen. Put assert or something here")
        if length(a) == 2
            lhs = extomx(a[1])
            if typeof(lhs) <: Union{BlankXXX, Mxpr{:Pattern}}  # This is probably not good enough!
                # Maybe : represents too many things.
                return mxpr(:Optional,lhs,extomx(a[2]))
            end
            mxhead = :Span      # Span syntax like:  a(::10), a(1::2), etc. clash with use of colon above
            extomxarr!(a,newa) # We may have to change the syntax
        else   # FIXME: refactor after testing
            mxhead = :Span
            extomxarr!(a,newa)
        end
    elseif ohead == :(.)  # don't need parens, but this is readable
        return parse_qualified_symbol(ex)
    elseif ohead == :quote
        mxhead = parse_quoted(ex, newa)
    elseif ohead == :macrocall
#        return eval(ex)
        return parse_macrocall(ex, newa)
    elseif ohead == :string
        mxhead = :StringInterpolation
        extomxarr!(a,newa)
    else
        dump(ex)
        error("extomx: No translation for Expr head '$(ohead)' in $ex")
    end
    mx = mxpr(mxhead,newa)  # Create the Mxpr
end

"""
    extomx(x::Integer)

Return `x` or convert `x` to `BigInt` if automatic
conversion is enabled with `BigIntInput(True)`.
"""
extomx(x::Integer) = getkerneloptions(:bigint_input) ? BigInt(x) : x
"""
    extomx(x::AbstractFloat)

Return `x` or convert `x` to `BigFloat` if automatic
conversion is enabled with `BigFloatInput(True)`.
"""
extomx(x::AbstractFloat) = getkerneloptions(:bigfloat_input) ? BigFloat(rationalize(x)) : x
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
    return occursin("_", ss)  ? parseblank(ss) : getsym(jtomsym(s))
end

"""
    @extomx(ex)

inserts translation of Julia expression `ex` to Symata into code. This is used
in Symata code. For instance in `Collect` in `algebra.jl`.

`@extomx` differs from `@sym` in that it does not evaluate the resulting Symata expression.
"""
macro extomx(ex)
    :($(extomx(ex)))
end

## FIXME: Find a good name for this macro and document.
## eg: f = @jul x -> x^2
macro jul(ex) extomx(Expr(:call, :J, ex)) end

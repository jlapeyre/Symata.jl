## Things that can't be loaded twice.g
# Some instances of data structures

## Symbol correspondence/translation between Julia and Symata

# This should be separated better. Some symbols are translated
# only on analyzing the ast, others, on both input and output.
# Fix. We delete the keys we don't want (unicode) from the reverse translation.

#### Input,Output translation table.

# These translations are made when encountered in the AST and on printing.
# A reversed dict is constructed for output.
# Excepted are the unicode characters. They are merged into the input dict.
# But, they are held in a separate table for output, so that unicode
# output can be disabled.

# Idea, but not implemented:
# We could use  a.b for contexts, modules.  In Mma
# a . b == a.b  == Dot[a,b], matrix and vector multiplication.
# a`b is symbol b in context a.  a .* b is illegal in Mma, but a .* b is legal syntax in Julia
# So: in Symata  a.b for contexts,  and a .* b for matrix multiplication

# We can use $ for (Symata) Function, but the translation to compound expression is broken
# We can use -> for (Symata) Function. This seems to work

## Symbols not yet in use
## .\
const JTOMSYM  =
 Dict(
     :(=) => :Set,
     :(:=) => :SetDelayed,
     :(^=) => :UpSet,
     :+ => :Plus,
     :- => :Minus,
     :* => :Times,
     :⋅ => :Dot,
     :^ => :Power,
     :(*=) => :TimesBy,
     :(+=) => :AddTo,
     # $ is good for Function below, but we might want it for interpolation instead
     # No, it does not interfere with interpolation, but it still wraps args in Comound expression. dont know why
     :(->) => :Function,
     :$ => :Function, #  $(1+~2) -->  Function(Plus(1,Slot(2))), but we don't do anything yet with this
     :~ => :Slot,     # we need to translate lone ~ , ie Slot into Slot(1)
     :(=>) => :Rule, # Mma uses ->. We use => because it has a higher precedence than ->, which is what we want.
     #      :(->) => :RuleDelayed,     # Mma uses :>. Julia parser does not allow this
     :(.>) => :RuleDelayed,      # Mma uses :>. Julia parser does not allow this.  .> has better precedence and parsing than ->
     :(./) => :ReplaceAll,       # Mma has /. for this !! But, /. is not legal Julia syntax
     :(.//) => :ReplaceRepeated,
     :(%) => :Map,
     :(.%) => :Apply,
     #      :(:) => :Span, # in v0.7, colon has become a call. We discriminate meaning by context
     :(&=) => :UpSetDelayed,  # This is available, at least. Not sure we want to take it.
     :... => :... ,  # We still need to decide what to do with this. Maybe evaluate the args and Apply Sequence
     :.. => :..,
     :! => :Not,
     :&& => :And,
     :|| => :Or,
     :| => :Alternatives,
     :(...) => :Repeated
 )

const JTOMSYM_ONEWAY  =
    Dict(
        :vcat => :List,
        :vect => :List,
        :ref => :Part,
        :cell1d => :List,   # curly brackets, but deprecated by julia
        :comparison => :Comparison,
    )

# FIXME  NB Alternatives: This is nary, but not Flat. So we need to parse
# a | b | c as Alternatives(a,b,c) rather than nested Alternatives as we do now.
# (a | b) | c is nested, and is not flattend.
# Other operators, such as Times are nary but Flat, so the flattening happens during the
# evaluations sequence.

# These symbols are unconditionally translated on input
# This is almost exactly the Dict unicode_translation. Maybe we can refactor.
const INSYMTRANS = Dict( :⇒  => :(=>),
                         :π => :Pi,
                         :≥ => :(>=),
                         :≤ => :(<=),
                         :γ => :EulerGamma,
                         :Γ => :Gamma,
                         :𝕖 => :E,
                         :𝕚 => :I,
                         :∞ => :Infinity,
                         :≠  =>  :!=,
                         :∈  => :Element
                         )

# Input translation. Output translation if unicode is enabled
const unicode_translation = Dict{Symbol,Symbol}(:π => :Pi,
                                                :γ => :EulerGamma,
                                                :∞ =>   :Infinity,
                                                :Γ =>   :Gamma,
                                                :𝕚 =>   :I,
                                                :≥  =>  :>=,
                                                :≤  =>  :<=,
                                                :≠  =>  :!=,
                                                :𝕖  =>  :E,
                                                :⇒  =>  :(=>),
                                                :→ => :Function,
                                                :∈ => :Element
)

const comparison_translation = Dict(
                                    :(==) => :Equal,
                                    :(!=) => :Unequal,
                                    :(>) => :Greater,
                                    :(<) => :Less,
                                    :(>=) => :GreaterEqual,
                                    :(<=) => :LessEqual,
                                    :(===) => :SameQ,
                                    :(!==) => :UnsameQ
                                    )


const inverse_comparison_translation = Dict{Symbol,Symbol}()
for x in comparison_translation
    inverse_comparison_translation[x[2]] = x[1]
end

# Output. Reverse dict for printing if unicode printing is enabled
const unicode_output = Dict{Symbol,Symbol}()
for (k,v) in unicode_translation unicode_output[v] = k end

# Reverse dict of ascii symbols for printing
const MTOJSYM = Dict{Any,Any}()
#const MTOJSYM = Dict{Symbol,Symbol}()
for (k,v) in JTOMSYM
    MTOJSYM[v] = k
end

# Finally, add the unicode symbols to the input translation table.
merge!(JTOMSYM, unicode_translation)
merge!(JTOMSYM, JTOMSYM_ONEWAY)

# This is only used for output. (Can't we detect it with the others on input ?)
MTOJSYM[:Span] = :(:)

merge!(MTOJSYM, inverse_comparison_translation)

function jtomsym(x::Symbol)
    check_autoload(x)  # trigger autoload of Symata language code
    if haskey(JTOMSYM,x)
        return JTOMSYM[x]
    end
    return x
end

jtomsym(x) = extomx(x)

function mtojsym(x::Symbol)
    # For output, reverse the rewrites done by preprocessing the input string befor parsing
    if haskey(REVERSE_PREPROCESS_SYMBOL_TRANSLATION,x)  # .>  --> :>   for output
         x = REVERSE_PREPROCESS_SYMBOL_TRANSLATION[x]   # =>  --> ->
     end
    if haskey(MTOJSYM,x)
        return MTOJSYM[x]
    end
    return x
end


# Non-Symbol Heads translate to themselves
mtojsym(x) = x

# These will not be translated back to Julia expression symbol on printing
# TODO. choose tuple or block, or both ?
# One unusual occurance of :block is filtered before we get here

# A compound expression is denoted by tuple () or by begin end.
# We could possible used tuple for something else.
for (k,v) = ( (:tuple,:CompoundExpression), (:block,:CompoundExpression), (:dict, :List), (:parameters, :TestExpression) )
    JTOMSYM[k] = v
end

#### Operator type: infix, prefix, postfix

#const OPTYPE  = Dict{Symbol,Symbol}()
const OPTYPE  = Dict{Any,Any}()

for op in (:(=), :(:=), :(=>), :Rule , :RuleDelayed, :Power, :(.>),
           :Set, :SetDelayed, :UpSet, :(*=), :(+=), :→, :(->), :Function,
           :TimesBy, :AddTo  # , :Element
           ) # need :Set here
    OPTYPE[op] = :binary
end

for op in keys(inverse_comparison_translation)
    OPTYPE[op] = :binary
end

for op in (:Plus, :Times, :Dot, :Span, :And, :Or, :Alternatives)
    OPTYPE[op] = :infix
end

# Nonsymbolic Heads, Integer, etc. assume they are prefix ops
getoptype(x) = :prefix

# function getoptype(x::Mxpr)
#     println("getting opt type for ", mhead(x), " args ", margs(x))
#     res = getoptype(mhead(x))
#     println("Resutt is $res")
#     res
# end

function getoptype(x::Symbol)
    if haskey(OPTYPE,x)
        return OPTYPE[x]
    end
    return :prefix
end

#### Comparison symbols

# This is only used in AST_tranlation.jl to make a Comparison Mxpr.
const COMPARISONSYMBOLS = Dict{Symbol,Bool}()

for op in ( :(==), :(<), :(>), :(>=), :(<=), :(===),
           :(!=), :(!==), :(<:))
    COMPARISONSYMBOLS[op] = true
end

function is_comparison_symbol(x::Symbol)
    if haskey(COMPARISONSYMBOLS,x)
        return true
    end
    return false
end

is_comparison_symbol(x) = false

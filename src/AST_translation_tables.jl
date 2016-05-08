## Things that can't be loaded twice.g
# Some instances of data structures

## Symbol correspondence/translation between Julia and SJulia

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
# So: in SJulia  a.b for contexts,  and a .* b for matrix multiplication

const JTOMSYM  =
 Dict(
      :(=) => :Set,
      :(:=) => :SetDelayed,
      :(^=) => :UpSet,
      :+ => :Plus,
      :- => :Minus,
      :* => :Times,
      :^ => :Power,
      :(*=) => :TimesBy,
      :(+=) => :AddTo,
# $ is good for Function below, but we might want it for interpolation instead
#      :$ => :Function, #  $(1+~2) -->  Function(Plus(1,Slot(2))), but we don't do anything yet with this
      :~ => :Slot,     # we need to translate lone ~ , ie Slot into Slot(1)
      :(=>) => :Rule, # Mma uses ->. We use => because it has a higher precedence than ->, which is what we want.
#      :(->) => :RuleDelayed, # Mma uses :>. Julia parser does not allow this
      :(.>) => :RuleDelayed, # Mma uses :>. Julia parser does not allow this.  .> has better precedence and parsing than ->
      :(./) => :ReplaceAll,   # Mma has /. for this !! But, /. is not legal Julia syntax
#      :(:) => :Span, # this is done specially in extomx. colon means various things
      :(&=) => :UpSetDelayed,  # This is available, at least. Not sure we want to take it.
      :vcat => :List,
      :vect => :List,
      :ref => :Part,
      :cell1d => :List,   # curly brackets, but deprecated by julia
      :comparison => :Comparison,
      :... => :... ,  # We still need to decide what to do with this. Maybe evaluate the args and Apply Sequence
      :! => :Not,
      :&& => :And,
      :|| => :Or,
      :| => :Alternatives   # Not implemented well, if at all
)

# Input translation
const unicode_translation = Dict{Symbol,Symbol}(:π => :Pi,
                                                :γ => :EulerGamma,
                                                :∞ =>   :Infinity,
                                                :Γ =>   :Gamma,
                                                :𝕚 =>   :I,
                                                :≥  =>  :>=,
                                                :≤  =>  :<=,
                                                :≠  =>  :!= )

# Output. Reverse dict for printing if unicode printing is enabled
const unicode_output = Dict{Symbol,Symbol}()
for (k,v) in unicode_translation unicode_output[v] = k end

# Reverse dict of ascii symbols for printing
const MTOJSYM = Dict{Symbol,Symbol}()
for (k,v) in JTOMSYM  MTOJSYM[v] = k end

# Finally, add the unicode symbols to the input translation table.
merge!(JTOMSYM, unicode_translation)

# This is only used for output. (Can't we detect it with the others on input ?)
MTOJSYM[:Span] = :(:)

function jtomsym(x::Symbol)
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
for (k,v) = ( (:tuple,:CompoundExpression), (:block,:CompoundExpression), (:dict, :List) )
    JTOMSYM[k] = v
end

#### Operator type: infix, prefix, postfix

const OPTYPE  = Dict{Symbol,Symbol}()

for op in (:(=), :(:=), :(=>), :Rule , :RuleDelayed, :Power, :(.>),
           :Set, :SetDelayed, :UpSet, :(*=), :(+=),
           :TimesBy, :AddTo ) # need :Set here
    OPTYPE[op] = :binary
end

for op in (:Plus, :Times, :Span, :And, :Or, :Alternatives)
    OPTYPE[op] = :infix
end

# Nonsymbolic Heads, Integer, etc. assume they are prefix ops
getoptype(x) = :prefix

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

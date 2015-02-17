## Things that can't be loaded twice.
# Some instances of data structures

## Symbol correspondence/translation between Julia and SJulia

const JTOMSYM  =
 Dict(
      :(=) => :Set,
      :(:=) => :SetDelayed,
      :(^=) => :UpSet,
      :+ => :Plus,
      :- => :Minus,
      :* => :Times,
      :^ => :Power,
      :$ => :Function, #  $(1+~2) -->  Function(Plus(1,Slot(2))), but we don't do anything yet with this
      :~ => :Slot,     # we need to translate lone ~ , ie Slot into Slot(1)
      :(=>) => :Rule, # Mma uses ->  (hmmm)
      :(->) => :RuleDelayed, # Mma uses :>. Julia parser does not allow this
      :vcat => :List,
      :ref => :Part,
      :cell1d => :List,   # curly brackets, but deprecated by julia
      :comparison => :Comparison,
      )

const MTOJSYM = Dict{Symbol,Symbol}()
for (k,v) in JTOMSYM  MTOJSYM[v] = k end

function jtomsym(x::Symbol)
    if haskey(JTOMSYM,x)
        return JTOMSYM[x]
    end
    return x
end

jtomsym(x) = extomx(x)

# Inverse of translations already present in MTOJSYM will be recorded
mtojsym(s::SJSym) = mtojsym(symname(s))
function mtojsym(x::Symbol)
    if haskey(MTOJSYM,x)
        return MTOJSYM[x]
    end
    return x
end

# Non-Symbol Heads translate to themselves
mtojsym(x) = x

# These will not be translated back to Julia expression symbol on printing
for (k,v) = ( (:tuple,:CompoundExpression),)
    JTOMSYM[k] = v
end

# For printing only
const OPTYPE  = Dict{Symbol,Symbol}()

for op in (:(=), :(:=), :(=>), :Rule , :RuleDelayed, :Power,
           :Set, :SetDelayed, :UpSet ) # need :Set here
    OPTYPE[op] = :binary
end

for op in (:Plus, :Times)
    OPTYPE[op] = :infix
end

getoptype(s::SJSym) = getoptype(symname(s))

# Nonsymbolic Heads, Integer, etc. assume they are prefix ops
getoptype(x) = :prefix

function getoptype(x::Symbol)
    if haskey(OPTYPE,x)
        return OPTYPE[x]
    end
    return :prefix
end

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
# stack overflow
#is_comparison_symbol(x::SJSym) = is_comparison_symbol(symname(x))
is_comparison_symbol(x) = false

const SYMTONUM  =
    Dict(
         :Plus => :mplus,
         :Times => :mmul
         );

function symtonum(x::Symbol)
    if haskey(SYMTONUM,x)
        return SYMTONUM[x]
    end
    return x
end

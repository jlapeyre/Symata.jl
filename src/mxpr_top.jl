## Things that can't be loaded twice.

## Symbol correspondence/translation between Julia and SJulia

const JTOMSYM  =
 Dict(
      :(=) => :Set,
      :(:=) => :SetDelayed,
      :+ => :Plus,
      :- => :Minus,
      :* => :Times,
      :^ => :Power,
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

mtojsym(s::SJSym) = mtojsym(symname(s))
function mtojsym(x::Symbol)
    if haskey(MTOJSYM,x)
        return MTOJSYM[x]
    end
    return x
end

const OPTYPE  = Dict{Symbol,Symbol}()

for op in (:(=), :(:=), :(=>), :Rule , :RuleDelayed, :Power )
    OPTYPE[op] = :binary
end

for op in (:Plus, :Times)
    OPTYPE[op] = :infix
end

getoptype(s::SJSym) = getoptype(symname(s))

function getoptype(x::Symbol)
#    println("Cheking $x")
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
is_comparison_symbol(x::SJSym) = is_comparison_symbol(symname(x))
is_comparison_symbol(x) = false


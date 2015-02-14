## These are the 'builtin' symbols
# They can by listed by giving `BuiltIn()'.
# They all have the attribute `Protected'.

# This file is loaded last because we are polluting Julia symbol table
# with symbols in order to get completion at the repl.  Each of these
# symbols will be bound in Julia to true, unless already bound.  We
# want to allow that they be bound to functions, instead, in files
# loaded before this one.

function set_pattributes{T<:AbstractString}(syms::Array{T,1},attrs::Array{Symbol,1})
    for s in syms
        for a in attrs
            set_attribute(symbol(s),a)
        end
        set_attribute(symbol(s),:Protected)  # They all are Protected
    end
end

set_pattributes{T<:AbstractString}(sym::T,attrs::Array{Symbol,1}) = set_pattributes([sym],attrs)
set_pattributes{T<:AbstractString}(syms::Array{T,1},attr::Symbol) = set_pattributes(syms,[attr])
set_pattributes{T<:AbstractString}(sym::T,attr::Symbol) = set_pattributes([sym],[attr])

set_pattributes(["Pattern", "SetJ", "SetAttributes"], :HoldFirst)

set_pattributes(["Module","LModule","Clear", "ClearAll", "HoldPattern", "HoldForm", "Hold",
                 "DumpHold", "DownValues", "HAge", "Table", "For", "If", "While", "Do",
                 "Jxpr", "Unprotect"],
                :HoldAll)

set_pattributes("Attributes",[:HoldAll,:Listable])

set_pattributes("Rule",:SequenceHold)

set_pattributes("Set",[:HoldFirst, :SequenceHold])

set_pattributes(["RuleDelayed","PatternTest"],[:HoldRest, :SequenceHold])

set_pattributes(["Timing","Allocated","SetDelayed"], [:HoldAll,:SequenceHold])

set_pattributes(["Pi","E"],[:ReadProtected,:Constant])

set_pattributes("I", [:ReadProtected,:Locked])

set_pattributes(["CompoundExpression","Sum"],[:ReadProtected,:HoldAll])

set_pattributes("Part",:ReadProtected)

set_pattributes(["Cos", "ACos", "Sin","Tan","Cosh","Sinh","Log","Minus","Abs"],
                [:Listable,:NumericFunction])

set_pattributes(["Plus", "Times"],
            [:Flat,:Listable,:NumericFunction,:OneIdentity,:Orderless])

set_pattributes("Power",[:Listable,:NumericFunction,:OneIdentity])

set_pattributes(["EvenQ","OddQ","Range"],[:Listable])

# Of course, these need to be organized!
set_pattributes(["Age","Apply","Dump", "Length","Blank","BlankSequence","BlankNullSequence",
          "JVar", "MatchQ", "AtomQ", "Println","Keys",
          "Replace", "ReplaceAll", "ReplaceRepeated", "TraceOn","TraceOff","FullForm", "Expand",
          "BI", "BF", "BuiltIns", "Symbol", "Pack", "Unpack","Example","Fixed",
          "UserSyms", "List","Syms",
          "Comparison", "DirtyQ", "Flat", "Listable", "Head", "Integer",
          "Orderless","NumericFunction","OneIdentity", "!=", "//", ">","==",
          "<", "<=","nothing","N","Unfix", "ExpToTrig",
          "String", "StringLength","Protected", "TimeOn", "TimeOff",
          "LeafCount","ByteCount","Depth"
           ],
           :Protected)

@sjdoc I "
I is the imaginary unit
"

@sjdoc E "
E is the base of the natural logarithm
"

@sjdoc Pi "
Pi is the trigonometric constant π.
"

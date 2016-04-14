# These are the 'builtin' symbols
# They can by listed by giving `BuiltIn()'.
# They all have the attribute `Protected'.

# This file is loaded last because we are polluting Julia symbol table
# with symbols in order to get completion at the repl.  Each of these
# symbols will be bound in Julia to true, unless already bound.  We
# want to allow that they be bound to functions, instead, in files
# loaded before this one.

set_pattributes{T<:AbstractString}(sym::T,attrs::Array{Symbol,1}) = set_pattributes([sym],attrs)
set_pattributes{T<:AbstractString}(syms::Array{T,1},attr::Symbol) = set_pattributes(syms,[attr])
set_pattributes{T<:AbstractString}(sym::T,attr::Symbol) = set_pattributes([sym],[attr])

set_pattributes(["Pattern", "SetJ", "SetAttributes", "TimesBy", "AddTo"], :HoldFirst)

set_pattributes(["Module","LModule","Clear", "ClearAll", "HoldPattern", "HoldForm", "Hold",
                 "DumpHold", "DownValues", "UpValues", "HAge", "Table", "For", "If", "While", "Do",
                 "Jxpr", "Unprotect"],
                :HoldAll)

set_pattributes("Attributes",[:HoldAll,:Listable])

set_pattributes("Rule",:SequenceHold)

set_pattributes(["Set","UpSet"],[:HoldFirst, :SequenceHold])

set_pattributes(["Increment","Decrement"],[:HoldFirst, :ReadProtected])

set_pattributes(["RuleDelayed","PatternTest"],[:HoldRest, :SequenceHold])

set_pattributes(["Timing","Allocated","SetDelayed"], [:HoldAll,:SequenceHold])

set_pattributes(["Pi","E"],[:ReadProtected,:Constant])
set_pattributes(["EulerGamma"],[:Protected,:Constant])

set_pattributes("I", [:ReadProtected,:Locked])

set_pattributes(["CompoundExpression","Sum","Product"],[:ReadProtected,:HoldAll])

set_pattributes(["Part","D","LaplaceTransform","InverseLaplaceTransform",
                 "FourierTransform","InverseFourierTransform", "Integrate", "DSolve"
                 ],:ReadProtected)

# We kinda need Exp, see the apprules.
set_pattributes(["Cos", "ArcCos", "Sin", "ArcSin", "Tan", "ArcTan",
                 "Cot", "Cosh","Sinh","Log","Minus","Abs","Re","Im","Exp",
                 "PolyGamma", "EllipticE", "EllipticF", "EllipticK", "EllipticPi"
                 ],
                [:Listable,:NumericFunction])

set_pattributes(["HeavisideTheta"],
                [:Listable, :Orderless])

set_pattributes(["Plus", "Times"],
            [:Flat,:Listable,:NumericFunction,:OneIdentity,:Orderless])

set_pattributes("Power",[:Listable,:NumericFunction,:OneIdentity])

set_pattributes(["EvenQ","OddQ","Range","Limit", "Together", "Apart"],[:Listable])

# Of course, these need to be organized!
set_pattributes(["Age","All","Apply","Dump", "Length","Blank","BlankSequence","BlankNullSequence",
          "JVar", "MatchQ", "AtomQ", "Println","Keys",
          "Replace", "ReplaceAll", "ReplaceRepeated", "TraceOn","TraceOff","FullForm", "Expand",
          "BI", "BF", "Big", "BuiltIns", "Symbol", "Pack", "Unpack","Example","Fixed",
          "UserSyms", "List","Syms", "Sequence", "Map",
          "Comparison", "DirtyQ", "Flat", "Listable", "Head", "Integer", "ListQ",
          "Orderless","NumericFunction","OneIdentity", "!=", "//", ">","==",
          "<", "<=","nothing","N","Unfix", "ExpToTrig",
          "String", "StringLength", "StringQ", "ToString", "StringJoin", "Protected", "TimeOn", "TimeOff",
          "TrUpOn","TrUpOff","TrDownOn","TrDownOff", "Numerator",
          "LeafCount","ByteCount","Depth","Permutations","Factor","FactorInteger","IntegerDigits",
          "Reverse","Help","Primes","Precision","Span","ConstantArray","Complex","Rational",
          "Simplify", "FullSimplify","RatSimp", "Solve", "Roots", "RealRoots",
          "TrigSimp", "Cancel","Collect", "ToSJulia", "ToSymPy", "Series", "Not", "Null",
                 "ans" # protect ans to keep it out of user symbols
           ],
                :Protected)
                

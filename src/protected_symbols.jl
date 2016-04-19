# These are the 'builtin' symbols
# They can by listed by giving `BuiltIn()'.
# They all have the attribute `Protected'.

# This file is loaded last because we are polluting Julia symbol table
# with symbols in order to get completion at the repl.  Each of these
# symbols will be bound in Julia to true, unless already bound.  We
# want to allow that they be bound to functions, instead, in files
# loaded before this one.

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
                 "PolyGamma", "EllipticE", "EllipticF", "EllipticK", "EllipticPi", "LogIntegral", "Mod", "DivRem",
                 "Sign", "SphericalHarmonicY"
                 ],
                [:Listable,:NumericFunction])

set_pattributes(["MeijerG", "HypergeometricPFQ"], [:NumericFunction])

set_pattributes(["HeavisideTheta"],
                [:Listable, :Orderless])

set_pattributes(["Plus", "Times"],
            [:Flat,:Listable,:NumericFunction,:OneIdentity,:Orderless])

set_pattributes("Power",[:Listable,:NumericFunction,:OneIdentity])

# This is not quite what Mma has for these. I don't understand why. Eg. CosIntegral is not a NumericFunction
# Many are not Listable in the docs, although they are in practice.
set_pattributes(["Pochhammer", "LogIntegral", "LerchPhi", "CosIntegral", "SinIntegral", "FresnelC", "FresnelS",
                 "HarmonicNumber", "BellB"],
                [:Listable,:NumericFunction,:ReadProtected])

set_pattributes(["EvenQ","OddQ", "PrimeQ", "Range","Limit", "Together", "Apart", "Cyclotomic"]
                ,[:Listable])

set_pattributes(["DirectedInfinity"], [:Listable, :ReadProtected])

set_pattributes(["PolarLift", "ExpPolar", "ExpandFunc"],
                :Protected)

# Of course, these need to be organized!

set_pattributes([
                  "!=","//","<","<=","==",">","Age","All","Apply","AtomQ",
                  "BF","BI","Big","Blank","BlankNullSequence","BlankSequence",
                  "BuiltIns","ByteCount","Cancel","Chop","Collect","Comparison",
                  "Complex","ComplexInfinity","ConstantArray","ConstantQ",
                  "Depth","DirtyQ","Dump","Example","ExpToTrig","Expand",
                  "Factor","FactorInteger","Fixed","Flat","Float64","FullForm",
                  "FullSimplify","Head","Help","Indeterminate","Infinity",
                  "Int64","Integer","IntegerDigits","JVar","Keys","LeafCount",
                  "Length","List","ListQ","Listable","Map","MatchQ","N",
                  "None","Not","Null","Numerator","NumericFunction","NumericQ",
                  "OneIdentity","Orderless","Pack","Permutations","Precision",
                  "Primes","Println","Protected","RatSimp","Rational","RealRoots",
                  "Replace","ReplaceAll","ReplaceRepeated","Reverse","Roots",
                  "Sequence","Series","Simplify","Solve","Span","String","StringJoin",
                  "StringLength","StringQ","Symbol","Syms","TimeOff","TimeOn",
                  "ToSJulia","ToString","ToSymPy","TrDownOff","TrDownOn","TrUpOff",
                  "TrUpOn","TraceOff","TraceOn","TrigSimp","Unfix","Unpack",
                  "UserSyms","ans","nothing"], :Protected)


# set_pattributes([ "Age","All","Apply","Dump",
#                  "Length","Blank","BlankSequence","BlankNullSequence",
#                  "JVar", "MatchQ", "AtomQ", "Println","Keys",
#                  "Replace", "ReplaceAll", "ReplaceRepeated",
#                  "TraceOn","TraceOff","FullForm", "Expand", "BI",
#                  "BF", "Big", "BuiltIns", "Symbol", "Pack",
#                  "Unpack","Example","Fixed", "UserSyms",
#                  "List","Syms", "Sequence", "Map", "Comparison",
#                  "DirtyQ", "Flat", "Listable", "Head", "Integer",
#                  "ListQ", "Orderless","NumericFunction","OneIdentity",
#                  "!=", "//", ">","==", "<",
#                  "<=","nothing","N","Unfix", "ExpToTrig", "String",
#                  "StringLength", "StringQ", "ToString", "StringJoin",
#                  "Protected", "TimeOn", "TimeOff",
#                  "TrUpOn","TrUpOff","TrDownOn","TrDownOff",
#                  "Numerator",
#                  "LeafCount","ByteCount","Depth","Permutations","Factor","FactorInteger","IntegerDigits",
#                  "Reverse","Help","Primes","Precision","Span","ConstantArray","Complex","Rational",
#                  "Simplify", "FullSimplify","RatSimp", "Solve",
#                  "Roots", "RealRoots", "TrigSimp", "Cancel","Collect",
#                  "ToSJulia", "ToSymPy", "Series", "Not", "Null",
#                  "None", "Indeterminate", "Infinity",
#                  "ComplexInfinity", "ConstantQ", "NumericQ",
#                  "Float64", "Int64", "Chop", "ans" ], :Protected)

# protect ans to keep it out of user symbols


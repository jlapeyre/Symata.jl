# Set the Attributes of some "builtin" Protected symbols

set_pattributes(["Pattern", "SetJ", "SetAttributes", "TimesBy", "AddTo"], :HoldFirst)

set_pattributes(["Module","LModule","Clear", "ClearAll", "HoldPattern", "HoldForm", "Hold",
                 "DumpHold", "DownValues", "UpValues", "HAge", "Table", "For", "If", "While", "Do",
                 "Jxpr", "Protect", "Unprotect", "Function", "Definition", "ToSJulia"],
                :HoldAll)

set_pattributes("Save", [:HoldRest])

set_pattributes("Attributes",[:HoldAll,:Listable])

set_pattributes("DivisorSigma",[:NHoldAll,:Listable])

set_pattributes("Rule",:SequenceHold)

set_pattributes(["Set","UpSet"],[:HoldFirst, :SequenceHold])

set_pattributes(["Increment","Decrement"],[:HoldFirst, :ReadProtected])

set_pattributes(["RuleDelayed","PatternTest"],[:HoldRest, :SequenceHold])

set_pattributes(["Timing","Allocated","SetDelayed","UpSetDelayed"], [:HoldAll,:SequenceHold])

set_pattributes(["Pi","E"],[:ReadProtected,:Constant])
set_pattributes(["EulerGamma"],[:Constant])

set_pattributes("I", [:ReadProtected,:Locked]) # Careful with this. We mostly use julia symbol :I bound to complex(0,1)

set_pattributes(["CompoundExpression","Sum","Product"],[:ReadProtected,:HoldAll])

set_pattributes(["Part","D","LaplaceTransform","InverseLaplaceTransform",
                 "FourierTransform","InverseFourierTransform", "Integrate", "DSolve", "ReadString",
                 "STDOUT", "STDERR", "DevNull"
                 ],:ReadProtected)

# We kinda need Exp, see the apprules.
set_pattributes(["Cos", "ArcCos", "Sin", "ArcSin", "Tan", "ArcTan",
                 "Cot", "Cosh","Sinh","Log","Minus","Abs","Re","Im","Exp", "Sqrt",
                 "PolyGamma", "EllipticE", "EllipticF", "EllipticK", "EllipticPi", "LogIntegral", "Mod", "DivRem",
                 "Sign", "SphericalHarmonicY", "SphericalBesselJ", "SphericalBesselY", "Erf", "Gamma"
                 ],
                [:Listable,:NumericFunction])

set_pattributes(["MeijerG", "HypergeometricPFQ"], [:NumericFunction])

set_pattributes(["HeavisideTheta"],
                [:Listable, :Orderless])

set_pattributes(["Plus", "Times"],
                [:Flat,:Listable,:NumericFunction,:OneIdentity,:Orderless])

set_pattributes(["LCM", "GCD"],
            [:Flat,:Listable,:OneIdentity,:Orderless])

set_pattributes("Power",[:Listable,:NumericFunction,:OneIdentity])

# This is not quite what Mma has for these. I don't understand why. Eg. CosIntegral is not a NumericFunction
# Many are not Listable in the docs, although they are in practice.
set_pattributes(["Pochhammer", "LogIntegral", "LerchPhi", "CosIntegral", "SinIntegral", "FresnelC", "FresnelS",
                 "HarmonicNumber", "BellB", "InverseErf"],
                [:Listable,:NumericFunction,:ReadProtected])

# BellB should be split into BellB and BellY
set_pattributes(["BellB"],
                [:NumericFunction,:ReadProtected])


set_pattributes(["EvenQ","OddQ", "PrimeQ", "Range","Limit", "Together", "Apart", "Cyclotomic", "MoebiusMu", "EulerPhi", "Divisors", "DivisorCount"]
                ,[:Listable])

set_pattributes(["DirectedInfinity"], [:Listable, :ReadProtected])

set_pattributes(["PolarLift", "ExpPolar", "ExpandFunc"],
                :Protected)

set_pattributes(["UnicodeOutput"], :Protected)

set_pattributes(["Return","Break","Continue"], :Protected)

set_pattributes(["HoldFirst","HoldAll","HoldRest","NHoldFirst","NHoldAll","NHoldRest"], :Protected)

set_pattributes(["Int64", "Int", "Float64", "AbstractString"], :Protected)

set_pattributes(["O","OO","OOO","OOOO","OOOOO","OOOOOO","OOOOOOO","OOOOOOOO","OOOOOOOOOO"], :Protected)

set_pattributes(["System", "Main"], :Protected)

set_pattributes([
                  "!=","//","<","<=","==",">","===",">=","Age","All","And","Apply","AtomQ",
                  "BF","BI","Big","Blank","BlankNullSequence","BlankSequence",
                  "BuiltIns","ByteCount","Cancel","Chop","Collect","Comparison",
                  "Complex","Complexes","ComplexInfinity","ConstantArray","ConstantQ",
                  "Depth","DirtyQ","Dump","Example","ExpToTrig","Expand","ExpandA",
                  "Factor","FactorInteger","FixedQ","Flat","FullForm",
                  "FullSimplify","Head","Help","Indeterminate","Infinity",
                  "Integer","Integers","IntegerDigits","JVar","Keys","LeafCount",
                  "Length","List","ListQ","Listable","Map","MatchQ","N",
                  "None","Not","Null","Numerator","NumericFunction","NumericQ",
                  "OneIdentity","Or","Order","Orderless","Pack","Permutations","PossibleClosedForm","Precision",
                  "Primes","Print", "Println","Protected","RatSimp","Rational",
                  "Rationals","RealRoots", "Reals",
                  "Replace","ReplaceAll","ReplaceRepeated","Reverse","Roots",
                  "Sequence","Series","Simplify","Solve","Span","String","StringJoin",
                  "StringLength","StringQ","Symbol","Syms","Temporary","TimeOff","TimeOn",
                  "ToString","ToSymPy","TrDownOff","TrDownOn","TrUpOff",
                  "TrUpOn","TraceOff","TraceOn","TrigSimp","Unfix","Unpack",
                  "UserSyms","Values", "ans","nothing"], :Protected)

# Set the Attributes of some "builtin" Protected symbols

set_pattributes(["Pattern", "SetJ", "SetAttributes", "TimesBy", "AddTo", "Catch"], :HoldFirst)

set_pattributes(["Module","LModule", "Clear", "ClearAll", "Condition", "HoldPattern", "HoldForm", "Hold",
                 "DumpHold", "DownValues", "UpValues", "HAge", "Table", "For", "If", "While", "Do",
                 "Jxpr", "Protect", "Unprotect", "Function", "Definition", "ToSymata"],
                :HoldAll)

set_pattributes("HoldComplete", [:HoldAllComplete])

set_pattributes(["Save", "Last"], [:HoldRest] )

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
                 "STDOUT", "STDERR", "DevNull", "HoldAllComplete"
                 ],:ReadProtected)

# We kinda need Exp, see the apprules.
set_pattributes(["Cos", "ArcCos", "Sin", "ArcSin", "Tan", "ArcTan",
                 "Cot", "Cosh","Sinh","Minus","Abs","Re","Im", "ReIm", "Exp", "Sqrt",
                 "PolyGamma", "EllipticE", "EllipticF", "EllipticK", "EllipticPi", "LogIntegral", "Mod", "DivRem",
                 "Sign", "SphericalHarmonicY", "SphericalBesselJ", "SphericalBesselY", "Erf", "Gamma"
                 ],
                [:Listable,:NumericFunction])

set_pattributes(["MeijerG", "HypergeometricPFQ"], [:NumericFunction])

set_pattributes(["HeavisideTheta"],
                [:Listable, :Orderless])

set_pattributes(["Plus", "Times"],
                [:Flat,:Listable,:NumericFunction,:OneIdentity,:Orderless])

set_pattributes(["Max", "Min"],
                [:Flat,:NumericFunction,:OneIdentity,:Orderless])

set_pattributes(["LCM", "GCD"],
            [:Flat,:Listable,:OneIdentity,:Orderless])

set_pattributes("Power",[:Listable,:NumericFunction,:OneIdentity])

# This is not quite what Mma has for these. I don't understand why. Eg. CosIntegral is not a NumericFunction
# Many are not Listable in the docs, although they are in practice.
set_pattributes(["Pochhammer", "LogIntegral", "LerchPhi", "CosIntegral", "SinIntegral", "FresnelC", "FresnelS",
                 "HarmonicNumber", "BellB", "InverseErf", "Log"],
                [:Listable,:NumericFunction,:ReadProtected])

# BellB should be split into BellB and BellY
set_pattributes(["BellB"],
                [:NumericFunction,:ReadProtected])


set_pattributes(["EvenQ","OddQ", "PrimeQ", "Range","Limit", "Together", "Apart", "Cyclotomic", "MoebiusMu", "EulerPhi", "Divisors", "DivisorCount"]
                ,[:Listable])

set_pattributes(["DirectedInfinity"], [:Listable, :ReadProtected])

# Some symbols that may be particular to SymPy
# We should use GaussianIntegers instead of Gaussian
set_pattributes(["PolarLift", "ExpPolar", "ExpandFunc", "Modulus","Force", "Deep", "Gaussian"],
                :Protected)

set_pattributes(["And", "Or"], [:Flat, :HoldAll, :OneIdentity])

set_pattributes(["UnicodeOutput"], :Protected)

set_pattributes(["Return","Break","Continue"], :Protected)

set_pattributes(["HoldFirst","HoldAll","HoldRest","NHoldFirst","NHoldAll","NHoldRest"], :Protected)

# FIXME: Subscript cannot have Attribute 'Protected', because the user must be able to assign, e.g. Subscript(a,1) = 1.
# But, without Protected, we do not get TAB completion.
set_attributes(:Subscript, :NHoldRest)

set_pattributes(["Int64", "Int", "Float64",  "BigFloat", "BigInt", "AbstractString", "Real", "Float"], :Protected)

set_pattributes(["O","OO","OOO","OOOO","OOOOO","OOOOOO","OOOOOOO","OOOOOOOO","OOOOOOOOOO"], :Protected)

set_pattributes(["System", "Main"], :Protected)

# True and False are actually parsed as Julia true and false. But, this is a quick way to get TAB completion for them
set_pattributes(["True", "False"], :Protected)

set_pattributes(["Optional","Repeated", "RepeatedNull" ], :Protected)

set_pattributes(["Plain","Unicode", "IJulia" ], :Protected)

set_pattributes([
                  "!=","//","<","<=","==",">","===",">=","Age","All","Alternatives","Apply","AtomQ",
                  "BF","BI","Big","Blank","BlankNullSequence","BlankSequence",
                  "BuiltIns","ByteCount","Cancel","Chop","Collect","Comparison",
                  "Complex","Complexes","ComplexInfinity","ConditionalExpression", "ConstantArray","ConstantQ",
                  "Depth","DirtyQ","Dump","Example","Except","ExpToTrig","Expand","ExpandA",
                  "Factor","FactorInteger","FixedQ","Flat","FullForm",
                  "FullSimplify","Head","Help","Indeterminate","Infinity",
                  "Integer","Integers","IntegerDigits","JVar","Keys","LeafCount",
                  "Length","List","ListQ","Listable","Map","MatchQ","N",
                  "None","Not","Null","Numerator","NumericFunction","NumericQ",
                  "OneIdentity","Order","Orderless","Pack","Permutations","PossibleClosedForm","Precision",
                  "Primes","Print", "Println","Protected","RatSimp","Rational",
                  "Rationals","RealRoots", "Reals",
                  "Replace","ReplaceAll","ReplaceRepeated","Reverse","Roots",
                  "Sequence","Series","Simplify","Solve","Span","String","StringJoin",
                  "StringLength","StringQ","Symbol","Syms","Temporary","Throw","Time",
                  "ToString","ToSymPy","TraceDownValues","TraceUpValues",
                  "Trace","TrigSimp","Undefined","Unfix","Unpack",
                  "UserSyms","Values", "ans","nothing"], :Protected)

# Set the Attributes of some "builtin" Protected symbols

## NOTE!: Until the attribute setting is cleaned up, in particular in math_functions.jl,
## This file should be loaded last or almost last in order to correct incorrectly set attributes.
## This happens because big lists of functions are handled by macros that set default attributes,
## but the defaults should not apply to a few of them.

## TODO: The use of arrays, vs. single elements and strings vs symbols is outdated
## set_sysattributes takes two args. either can be a string or a symbol or an array of strings or symbols.

set_sysattributes(["Pattern", "SetJ", "SetAttributes", "ClearAttributes", "TimesBy", "AddTo", "Catch"], :HoldFirst)

## Piecewise is also read protected in Mma
set_sysattributes(["Module","LModule", "Clear", "ClearAll", "Condition", "HoldPattern", "HoldForm", "Hold",
                   "DumpHold", "DownValues", "UpValues", "HAge", "Table", "NTable", "For", "If", "While", "Do",
                   "Jxpr", "JuliaExpression", "Protect", "Unprotect", "Function", "Definition", "DirtyQ", "ToSymata",
                   "NIntegrate", "Compile","SymataCall", "Piecewise", "ExportJ", "OwnValues"],
                  :HoldAll)

set_sysattributes(["HoldComplete", "Unevaluated"], [:HoldAllComplete])

set_sysattributes(["Save", "Last"], [:HoldRest] )

set_sysattributes("Attributes",[:HoldAll,:Listable])

set_sysattributes("DivisorSigma",[:NHoldAll,:Listable])

set_sysattributes("Rule",:SequenceHold)

set_sysattributes(["Set","UpSet"],[:HoldFirst, :SequenceHold])

set_sysattributes(["Increment","Decrement"],[:HoldFirst, :ReadProtected])

set_sysattributes(["RuleDelayed","PatternTest"],[:HoldRest, :SequenceHold])

set_sysattributes(["Timing","Allocated","SetDelayed","UpSetDelayed"], [:HoldAll,:SequenceHold])

set_sysattributes(["Pi","E"],[:ReadProtected,:Constant])
set_sysattributes(["EulerGamma", "GoldenRatio", "Catalan"],[:Constant])

set_sysattributes("I", [:ReadProtected,:Locked]) # Careful with this. We mostly use julia symbol :I bound to complex(0,1)

set_sysattributes(["CompoundExpression","Sum","Product"],[:ReadProtected,:HoldAll])

set_sysattributes(["Part","D","LaplaceTransform","InverseLaplaceTransform",
                 "FourierTransform","InverseFourierTransform", "Integrate", "DSolve", "ReadString",
                 "STDOUT", "STDERR", "DevNull", "HoldAllComplete"
                 ],:ReadProtected)

# We kinda need Exp, see the apprules.
set_sysattributes(["Cos", "ArcCos", "Sin", "ArcSin", "Tan", "ArcTan", "CosPi", "SinPi",
                 "Cot", "Cosh", "Conjugate", "Sinh","Minus","Abs","Re","Im", "ReIm", "Exp", "Sqrt",
                 "PolyGamma", "EllipticE", "EllipticF", "EllipticK", "EllipticPi", "LogIntegral", "Mod", "DivRem",
                 "Sign", "SphericalHarmonicY", "SphericalBesselJ", "SphericalBesselY", "Erf", "Gamma", "GammaRegularized", "Divide",
                 "DirichletEta", "PolyLog", "Sqrt", "CubeRoot", "Surd", "BesselJZero"
                 ],
                [:Listable,:NumericFunction])

## These were given Listable attribute in math_functions.jl
clear_attributes(:MeijerG, :HypergeometricPFQ)
set_sysattributes([:MeijerG, :HypergeometricPFQ], [:NumericFunction])

set_sysattributes([:HeavisideTheta],
                [:Listable, :Orderless])

set_sysattributes(["Plus", "Times"],
                [:Flat,:Listable,:NumericFunction,:OneIdentity,:Orderless])

set_sysattributes(["Max", "Min"],
                [:Flat,:NumericFunction,:OneIdentity,:Orderless])

set_sysattributes(["LCM", "GCD"],
                [:Flat,:Listable,:OneIdentity,:Orderless])

set_sysattributes(["Composition"],
            [:Flat,:OneIdentity,])

set_sysattributes("Power",[:Listable,:NumericFunction,:OneIdentity])

# This is not quite what Mma has for these. I don't understand why. Eg. CosIntegral is not a NumericFunction
# Many are not Listable in the docs, although they are in practice.
set_sysattributes(["Pochhammer", "LogIntegral", "LerchPhi", "CosIntegral", "SinIntegral", "FresnelC", "FresnelS", "MittagLefflerE",
                 "HarmonicNumber", "InverseErf", "Log"],
                [:Listable,:NumericFunction,:ReadProtected])

# BellB should be split into BellB and BellY
clear_attributes(:BellB)  ## FIXME: set these attributes only once
set_sysattributes(["BellB"],
                [:NumericFunction,:ReadProtected])


# Note: Simplify is not Listable in Mma
set_sysattributes(["Boole", "EvenQ","OddQ", "PrimeQ", "Range","Limit", "Together", "Apart", "Cyclotomic", "MoebiusMu", "EulerPhi", "Divisors", "DivisorCount",
                 "ToExpression", "Simplify", "FullSimplify", "StringReverse", "ToJExpression", "LetterNumber"]
                ,[:Listable])

set_sysattributes(["DirectedInfinity"], [:Listable, :ReadProtected])

# Some symbols that may be particular to SymPy
# We should use GaussianIntegers instead of Gaussian
set_sysattributes(["PolarLift", "ExpPolar", "ExpandFunc", "Modulus","Force", "Deep", "Gaussian"],
                :Protected)

set_sysattributes(["And", "Or"], [:Flat, :HoldAll, :OneIdentity])

set_sysattributes(["UnicodeForm", "JupyterForm", "InputForm"], :Protected)

## Do we still need these ?
#set_sysattributes(["Plain","Unicode", "IJulia" ], :Protected)

set_sysattributes(["Return","Break","Continue"], :Protected)

set_sysattributes(["HoldFirst","HoldAll","HoldRest","NHoldFirst","NHoldAll","NHoldRest"], :Protected)

# FIXME: Subscript cannot have Attribute 'Protected', because the user must be able to assign, e.g. Subscript(a,1) = 1.
# But, without Protected, we do not get TAB completion.
set_attributes(:Subscript, :NHoldRest)

set_sysattributes(["Int64", "Int", "Float64",  "BigFloat", "BigInt", "AbstractString", "Real", "Float"], :Protected)

set_sysattributes(["O","OO","OOO","OOOO","OOOOO","OOOOOO","OOOOOOO","OOOOOOOO","OOOOOOOOOO"], :Protected)

set_sysattributes(["System", "Main"], :Protected)

# True and False are actually parsed as Julia true and false. But, this is a quick way to get TAB completion for them
set_sysattributes(["True", "False"], :Protected)

set_sysattributes(["Optional","Repeated", "RepeatedNull" ], :Protected)

set_sysattributes([
    "!=","//","<","<=","==",">","===",">=","Age","Algebraics","All","Alternatives","Apply","Array","ArrayDepth",
    "AtomQ","BF","BI","Big","Binary","Blank",
    "BlankNullSequence","BlankSequence","Booleans","BuiltIns","ByteCount","Cancel",
    "Chop", "Collect","Comparison","CompiledFunction","Complex", "ComplexInfinity",
    "Complexes","ConditionalExpression",
    "Conditions","ConstantArray","ConstantQ","Depth","DirtyQ","Dump","Evaluate","Example","Except",
    "ExpToTrig", "Expand", "ExpandA", "ExpandCos", "ExpandCosRule",
    "ExpandSin","ExpandSinCos","ExpandSinRule","Factor","FactorInteger",
    "FixedQ","Flat","FullForm","Head", "Help",
    "Indeterminate","Infinity","Integer","IntegerDigits","Integers",
    "JVar","Keys","LeafCount","Length","List","ListQ","Listable",
    "Map","MatchQ","Missing","N","None","Not",
    "Null","Numerator","NumericFunction","NumericQ","OneIdentity","Order","Orderless",
    "Pack","Permutations", "PossibleClosedForm","Precision","Primes","Print",
    "PrintTests","Println", "Protected",
    "RatSimp","Rational","Rationals","RealRoots","Reals", "Replace", "ReplaceAll",
    "ReplaceRepeated","Reverse","Roots","Sequence","SequenceHold",
    "Series","Solve","Span","String","StringJoin","StringLength","StringQ","Subdivide","Symbol",
    "Syms","Temporary","TensorRank","Throw",
    "Time","ToLowerCase","ToString","ToSymPy","ToUpperCase", "Trace","TraceDownValues",
    "TraceUpValues","TrigSimp", "Undefined", "Unfix", "Unpack", "UserSyms", "Values", "Normal",
    "ans", "nothing" ], :Protected)

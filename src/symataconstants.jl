const SYMATA_VERSION = v"0.0.19-dev.006"

const NullMxpr = mxprcf(:Null)
const Null = :Null  # In Mma, Null is a Symbol. But, the Mma REPL prints nothing when encountering it (sometimes)

const ComplexInfinity = mxprcf(:DirectedInfinity)
const Infinity = mxprcf(:DirectedInfinity,1)
const MinusInfinity = mxprcf(:DirectedInfinity,-1)
const Indeterminate = :Indeterminate
const Undefined = :Undefined

const I = complex(0,1)

setsymval(:True, true)
setsymval(:False, true)
setsymval(:Infinity, Infinity)
setsymval(:ComplexInfinity, ComplexInfinity)
mergesyms(Infinity,:nothing)
mergesyms(MinusInfinity,:nothing)
mergesyms(ComplexInfinity,:nothing)

## 1/Sqrt(2) and -1/Sqrt(2)
# Making these constant does save a bit of time
# We could do this in an more organized way.
const _moosq2 = mmul(-1,mpow(2,-1//2))
const _oosq2 = mpow(2,-1//2)
mergesyms(_moosq2,:nothing)
mergesyms(_oosq2,:nothing)
setfixed(_moosq2)
setcanon(_moosq2)
setfixed(_oosq2)
setcanon(_oosq2)

setsymval(:BigInt, BigInt)
setsymval(:BigFloat, BigFloat)
setsymval(:Float64, Float64)
setsymval(:Int64, Int64)
setsymval(:Int, Int)

#### typealiases

# used in predicates.jl
typealias BlankXXX Union{Mxpr{:Blank},Mxpr{:BlankSequence},Mxpr{:BlankNullSequence}}

# used in apprules.jl
typealias Rules Union{Mxpr{:Rule},Mxpr{:RuleDelayed}}

# used in expressions.jl
typealias Holds Union{Mxpr{:Hold}, Mxpr{:HoldForm}, Mxpr{:HoldPattern}, Mxpr{:HoldComplete}}

# used in sortorderless.jl
typealias ExpNoCanon Union{SJSym,Number}

# used in flatten.jl
typealias FlatT Union{Mxpr{:Plus},Mxpr{:Times},Mxpr{:And},Mxpr{:Or}, Mxpr{:LCM}, Mxpr{:GCD} }

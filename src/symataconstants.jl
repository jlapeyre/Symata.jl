## NOTE: Some constants are defined in mxpr_type.jl

const SYMATA_VERSION = v"0.3.0-dev.003"

const NullMxpr = mxprcf(:Null)
const Null = :Null  # In Mma, Null is a Symbol. But, the Mma REPL prints nothing when encountering it (sometimes)

const ComplexInfinity = mxprcf(:DirectedInfinity)
const Infinity = mxprcf(:DirectedInfinity,1)
const MinusInfinity = mxprcf(:DirectedInfinity,-1)
const Indeterminate = :Indeterminate
const Undefined = :Undefined

const I = complex(0,1)
const MinusI = complex(0,-1)

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

## 1/Sqrt(2)
const _moosq2 = mmul(-1,mpow(2,-1//2))

## -1/Sqrt(2)
const _oosq2 = mpow(2,-1//2)

## Pi^2
const Pisq = mmul(Pi,Pi)

## Log(2)

const Log2 = Log(2)

for s in ( :_moosq2, :_oosq2 , :Pisq, :ComplexInfinity, :Infinity, :MinusInfinity, :NullMxpr, :Log2)
    @eval mergesyms($s, :nothing)
    @eval setfixed($s)
    @eval setcanon($s)
end

for s in ( :BigInt, :BigFloat, :Float64, :Int64, :Int)
    sq = QuoteNode(s)
    @eval setsymval($sq,$s)
end

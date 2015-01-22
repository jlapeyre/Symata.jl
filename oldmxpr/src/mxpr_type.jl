typealias SJSym Symbol  # This is not yet applied much in code.

abstract AbstractMxpr
type Mxpr{T} <: AbstractMxpr
    head::SJSym
    args::Array{Any,1}
    jhead::Symbol    # Actual exact Julia head: :call, etc. TODO: get rid of this field
    clean::Bool      # Is the expression canonicalized ?
end

# This is not yet applied much in code.
const SJFloat = Float64
const SJInt = Int
const SJRational = Rational{SJInt}
const _SJOne = 1
const _SJZero = 0
SJOne(x) = _SJOne
SJOne() = _SJOne
SJZero(x) = _SJZero
SJZero() = _SJZero
SJInt(x::Number) = convert(SJInt,x)
SJFloat(x::Number) = convert(SJFloat,x)
SJRational(x::Number) = convert(SJRational,x)

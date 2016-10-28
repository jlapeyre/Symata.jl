abstract SequenceSpec

#  All all elements
type SequenceAll <: SequenceSpec
end

#  None  no elements
type SequenceNone <: SequenceSpec
end

#  n  elements 1 through n
immutable SequenceN{T<:Integer} <: SequenceSpec
    n::T
end

#  UpTo[n] elements 1 up to at most n
immutable SequenceUpToN{T<:Integer} <: SequenceSpec
    n::T
end

#  [n]  element n only
immutable SequenceNOnly{T<:Integer} <: SequenceSpec
    n::T
end

#  [m,n] elements m through n
immutable SequenceMN{T<:Integer,V<:Integer} <: SequenceSpec
    m::V    
    n::T
end

#  [m,n] elements m through n, step s
immutable SequenceMNS{T<:Integer,V<:Integer,W<:Integer} <: SequenceSpec
    m::V
    n::T    
    s::W
end

seqspecerr(x) = symerror(x, " is not a valid sequence specification")
sequencespec(n::Integer) = SequenceN(n)
sequencespec(x::Mxpr{:UpTo}) = SequenceUpToN(x[1])
sequencespec(x::Symbol) = x == :None ? SequenceNone() : x == :All ? SequenceAll() : seqspecerr(x)

function sequencespec(x::List)
    len = length(x)
    len == 1 && return SequenceNOnly(x[1])
    len == 2 && return SequenceMN(x[1],x[2])
    len == 3 && return SequenceMNS(x[1],x[2],x[3])
    seqspecerr(x)
end

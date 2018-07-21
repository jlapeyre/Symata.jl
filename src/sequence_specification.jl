abstract type SequenceSpec end

#  All all elements
mutable struct SequenceAll <: SequenceSpec
end

#  None  no elements
mutable struct SequenceNone <: SequenceSpec
end

#  n  elements 1 through n
struct SequenceN{T<:Integer} <: SequenceSpec
    n::T
end

#  UpTo[n] elements 1 up to at most n
struct SequenceUpToN{T<:Integer} <: SequenceSpec
    n::T
end

#  [n]  element n only
struct SequenceNOnly{T<:Integer} <: SequenceSpec
    n::T
end

#  [m,n] elements m through n
struct SequenceMN{T<:Integer,V<:Integer} <: SequenceSpec
    m::V
    n::T
end

#  [m,n] elements m through n, step s
struct SequenceMNS{T<:Integer,V<:Integer,W<:Integer} <: SequenceSpec
    m::V
    n::T
    s::W
end

seqspecstart(s::SequenceN) = 1
seqspecstart(s::SequenceUpToN) = 1
seqspecstart(s::SequenceNOnly) = s.n
seqspecstart(s::SequenceAll) = 1
seqspecstart(s::SequenceMN) = s.m
seqspecstart(s::SequenceMNS) = s.m

seqspecend(s::SequenceN,n) = s.n
seqspecend(s::SequenceUpToN,n) = min(s.n,n)
seqspecend(s::SequenceNOnly,n) = s.n
seqspecend(s::SequenceAll,n) = n
seqspecend(s::SequenceMN,n) = s.n
seqspecend(s::SequenceMNS,n) = s.n

seqspecdi(s::SequenceSpec) = 1
seqspecdi(s::SequenceMNS) = s.s

seqiter(s::SequenceSpec,n) = (seqspecstart(s),seqspecend(s,n),seqspecdi(s))

seqspecerr(x) = symerror(x, " is not a valid sequence specification")

sequencespec(n::Integer) = SequenceN(n)
sequencespec(x::Mxpr{:UpTo}) = SequenceUpToN(x[1])
sequencespec(x::Symbol) = x == :None ? SequenceNone() : x == :All ? SequenceAll() : seqspecerr(x)

sequencespec(n::Integer,nmax) = SequenceN(posnegi(nmax,n))
sequencespec(x::Mxpr{:UpTo},nmax) = SequenceUpToN(posnegi(nmax,n))
sequencespec(x::Symbol,nmax) = x == :None ? SequenceNone() : x == :All ? SequenceAll() : seqspecerr(x)  # hmm maybe we should use nmax here

function sequencespec(x::ListT)
    len = length(x)
    len == 1 && return SequenceNOnly(x[1])
    len == 2 && return SequenceMN(x[1],x[2])
    len == 3 && return SequenceMNS(x[1],x[2],x[3])
    seqspecerr(x)
end

function sequencespec(x::ListT,nmax)
    len = length(x)
    len == 1 && return SequenceNOnly(posnegi(nmax,x[1]))
    len == 2 && return SequenceMN(posnegi(nmax,x[1]),posnegi(nmax,x[2]))
    len == 3 && return SequenceMNS(posnegi(nmax,x[1]),posnegi(nmax,x[2]),x[3])
    seqspecerr(x)
end

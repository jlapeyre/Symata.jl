# Take uses the standard sequence specification:
# 	All	all elements
# 	None	no elements
# 	n	elements 1 through n
# 	UpTo[n]	elements 1 up to at most n, as available
# 	-n	last n elements
# 	{n}	element n only
# 	{m,n}	elements m through n inclusive
# 	{m,n,s}	elements m through n in steps of s

abstract SequenceSpec

type SequenceAll <: SequenceSpec
end

type SequenceNone <: SequenceSpec
end

immutable SequenceN <: SequenceSpec
    n::Int
end

immutable SequenceUpToN <: SequenceSpec
    n::Int
end

immutable SequenceLastN <: SequenceSpec
    n::Int
end

immutable SequenceNOnly <: SequenceSpec
    n::Int
end

immutable SequenceMN <: SequenceSpec
    n::Int
    m::Int
end

immutable SequenceMNS <: SequenceSpec
    n::Int
    m::Int
    s::Int    
end


function make_sequence_specification(n::Int)
    n >= 0  && return SequenceN(n)
    SequenceLastN(n)
end

function make_sequence_specification(x::Symbol)
    x == :None && return SequenceNone()
    x == :All && return SequenceAll()
    symerror(x, " is not a valid sequence specification")
end

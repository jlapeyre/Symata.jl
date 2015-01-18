abstract AbstractMxpr

type Mxpr{T} <: AbstractMxpr
    head::Symbol
    args::Array{Any,1}
    jhead::Symbol    # Actual exact Julia head: :call, etc.
    clean::Bool      # Is the expression canonicalized ?
end

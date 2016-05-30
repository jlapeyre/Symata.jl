# We try some wrapper functions. Maybe we will use more.

sympy_gamma(a) = sympy.gamma(a)
sympy_gamma(a,z) = sympy.uppergamma(a,z)

sympy_erf(x) = sympy.erf(x)
sympy_erf(x,y) = sympy.erf2(x,y)

#### sjlog

function sjlog(x::AbstractFloat)
    x <= 0 ? log(complex(x,zero(x))) : log(x)
end

function sjlog(b::AbstractFloat,x::AbstractFloat)
    x <= 0 ? log(b,complex(x,zero(x))) : log(b,x)
end

function sjlog(b::Number,x::AbstractFloat)
    x <= 0 ? log(b,complex(x,zero(x))) : log(b,x)
end

function sjlog(b::AbstractFloat,x::Real)
    x <= 0 ? log(b,complex(x,zero(x))) : log(b,x)
end

sjlog{T<:AbstractFloat}(b::AbstractFloat,z::Complex{T}) = log(b,z)
function sjlog(b::AbstractFloat,x::Complex)
    log(b,complex(x,zero(x)))
end

sjlog{T<:AbstractFloat}(z::Complex{T}) = log(z)
sjlog{T<:AbstractFloat}(b::Number,z::Complex{T}) = log(b,z)
sjlog{T<:AbstractFloat}(b::Complex{T}, z::AbstractFloat) = log(b,z)
sjlog{T<:AbstractFloat,V<:AbstractFloat}(b::Complex{T},z::Complex{V}) = log(b,z)
sjlog{T<:AbstractFloat}(b::Complex{T}, z::Number) = log(b,z)

sjlog(x)  =  x |> sjtopy |> sympy.log |> pytosj

# SymPy reverse the args
function sjlog(b,x)
    sb = sjtopy(b)
    sx = sjtopy(x)
    pytosj(sympy.log(sx,sb))
end

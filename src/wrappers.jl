## We try some wrapper functions. Maybe we will use more.
## The main idea is to try automatically promoting to big integer
## or complex types. Eg. we want to allow sqrt(-.5), without requiring sqrt(Complex(-.5))

## FIXME: LaguerreL(3/2,x) is broken. does not agree with Mma, nor does it when evaluated. Mathics doc agrees with Mma
## Mathics also gives wrong result for LaguerreL(3/2,x) but gives the correct floating point result

## TODO: export sympy and mpmath
## All of the following are in sympy[:mpmath]
## sympy[:mpmath][:ber], bei, ker, kei  ### Kelvin functions
## also in mpmath, struvel, struveh, angerl, webere, lommels1, lommels2
## besseljzero, <-- can use this to test my code, besselyzero, airyaizero, airybizero,
## socrergi, socrerhi, coulombf, coulombg, coulombc,
## whitm, whitw, pcfd (para. cyl func), pcfu, pcfv
## hyp1f1, hyp1f2, hyp1f0, hyp1f2, 2f0, etc. etc. hypercomb (weighted combination)
## legendre, legenp, legenq
##  lots of stuff missing here.
## http://docs.sympy.org/0.7.0/modules/mpmath/functions/elliptic.html
## many things missing. Mertens constant, etc. etc.
## http://docs.sympy.org/0.7.6/modules/mpmath/functions/index.html
## TODO: remove sympy[:mpmath] in our code, use mpmath (which is bound to the same thing)
## Everything on this page. http://docs.sympy.org/0.7.0/modules/mpmath/functions/bessel.html
## TODO: maybe we need to call functions specialized for common bessel orders ?
## TODO: check does mpmath or sympy have more numerical int routines than quadgk ?

### sjgamma

sjgamma{T<:AbstractFloat}(x::T) = gamma(x)
sjgamma{T<:AbstractFloat}(x::Complex{T}) = gamma(x)
sjgamma(a) = a |> sjtopy |> sympy_gamma |> pytosj
sjgamma(a,z) = sympy_gamma(sjtopy(a),sjtopy(z)) |> pytosj

sympy_gamma(a) = sympy[:gamma](a)
sympy_gamma(a,z) = sympy[:uppergamma](a,z)

sympy_erf(x) = sympy[:erf](x)
sympy_erf(x,y) = sympy[:erf2](x,y)

### sjerf

sjerf{T<:AbstractFloat}(x::T) = erf(x)
sjerf{T<:AbstractFloat}(x::Complex{T}) = erf(x)
sjerf(a) = a |> sjtopy |> sympy_erf |> pytosj
sjerf(a,z) = sympy_erf(sjtopy(a),sjtopy(z)) |> pytosj

### sjlog

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

sjlog(x)  =  x |> sjtopy |> sympy[:log] |> pytosj

# SymPy reverse the args
function sjlog(b,x)
    sb = sjtopy(b)
    sx = sjtopy(x)
    pytosj(sympy[:log](sx,sb))
end

### sjfactorial

function sjfactorial(n::Int)
    n < 0 && return MinusInfinity
    n < 20 && return factorial(n) # Assumes Int64
    factorial(big(n))
end

sjfactorial(n::BigInt) = n < 0 ? MinusInfinity : factorial(n)
sjfactorial(n::Number) = sjgamma(n+1)

sjfactorial(x) = x |> sjtopy |> sympy[:factorial] |> pytosj

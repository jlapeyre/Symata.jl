#### Number theory and combinatorics.

using Memoize
using Primes

if VERSION >= v"0.5-"
    import Combinatorics: permutations
end

import Polynomials
import Combinatorics


# We have dropped support for v0.4 in any case
#using Combinatorics


### BinarySearch

@mkapprule BinarySearch

@sjdoc BinarySearch """
    BinarySearch(list,item)

returns a list of the indices at which `item` appears in the sorted list `list`.
"""

@doap BinarySearch(a::ListT,x) = tolist(searchsorted(margs(a),x))

## TODO: We have a julia package that is much more efficient
## at functions related to primes and factoring. Find a way to make an optional Symata package.

### FactorInteger

@sjdoc FactorInteger """
    FactorInteger(n)

give a list of prime factors of `n` and their multiplicities.
"""
@mkapprule FactorInteger nargs => 1:2
@doap FactorInteger(x) = tolistoflistsfixed(factor(x))

### PrimeList

@sjdoc PrimeList """
    PrimeList(n)

return the list of all prime numbers `<= n`.
"""

## Convert `n` to Int to work around julia bug
@mkapprule PrimeList nargs => 1
@doap PrimeList(n::Integer) = tolistfixed(primes(convert(Int,n)))

### Prime

@sjdoc Prime """
    Prime(n)

returns the `n`th prime number.
"""
@mkapprule Prime :nargs => 1
@doap Prime(n::Integer) = sympy[:prime](n)

### PrimePi

@mkapprule PrimePi nargs => 1  """
    PrimePi(n)

returns the number of primes less than or equal to `n`.
"""

@doap PrimePi(n::Integer) = sympy[:primepi](n)

### Permutations

@mkapprule Permutations nargs => 1:2  """
    Permutations(expr)

give a list of all permutations of elements in `expr`.
"""

@doap Permutations(x::Mxpr) = tolistoflistsfixed(permutations(margs(x)))

### IntegerPartitions

## The Mathematica "way" is not very convenient. It is clumsy to controll what you
## want (regardless of the implmentation here, it is clumsy for the user.) But, it
## is not clear how to avoid this kind of interface to julia `partitions`

## Setting "fixed" below increases speed by 3 or 5 times in some tests.

@mkapprule IntegerPartitions
@doap IntegerPartitions(n::Integer) = tolistoflistsfixed(partitions(n))
@doap IntegerPartitions(n::Integer, m::Integer) = _integer_partitions_range(n,1,m)

function _integer_partitions_range(n,m1,m2)
    a = collect(partitions(n,m1))
    for i in (m1+1):m2
        c = collect(partitions(n,i))
        isempty(c) && continue
        append!(a,c)
    end
    tolistoflists(a)
end

@doap IntegerPartitions(n::Integer, ls::ListT) = listofintegersq(ls) ?  _integer_partitions_list(mx,n,margs(ls)...) : mx
_integer_partitions_list(mx,n,m) = tolistoflistsfixed(collect(partitions(n,m)))
_integer_partitions_list(mx,n,m1,m2) = _integer_partitions_range(n,m1,m2)
_integer_partitions_list(mx,n,m,args...) = mx

@mkapprule NumberOfPartitions nargs => 1  """
    NumberOfPartitions(n)

gives the number of integer partitions of `n`.
"""

@doap NumberOfPartitions(n::Integer) = n <= 405 ? length(partitions(n)) : bnpartitions(n)

## copied from partitions.jl. We just add the BigInt dict
## in n > 405, we need BigInt

let _bnpartitions = Dict{BigInt,BigInt}()
    global bnpartitions
    function bnpartitions(n::Integer)
        if n < 0
            0
        elseif n < 2
            1
        elseif (np = get(_bnpartitions, n, 0)) > 0
            np
        else
            np = 0
            sgn = 1
            for k = 1:n
                np += sgn * (bnpartitions(n-k*(3k-1)>>1) + bnpartitions(n-k*(3k+1)>>1))
                sgn = -sgn
            end
            _bnpartitions[n] = np
        end
    end
end

### Named polynomial sequences.
##
## Many more of these are easy to reimplement. See sympy source. These are much more efficient than
## translating/computing with sympy.

### Fibonaccci

@mkapprule Fibbonaci nargs => 1:2

@doap function Fibbonaci(n::Integer)
    n < 0 && return mx
    Combinatorics.fibonaccinum(n)
end

@doap function Fibbonaci(n::Integer,x)
    n < 0 && return mx
    p = _fib_poly(n)
    fromjuliaPolynomial(p,x)
end


let xvar = Polynomials.Poly([0,1])
    global _fib_poly
    @memoize function _fib_poly(n)
        n == 0 ? zero(n) : n == 1 ? one(n) :  xvar *  _fib_poly(n-1) + _fib_poly(n-2)
    end
end

### LucasL

@mkapprule LucasL nargs => 1:2

@doap function LucasL(n::Integer)
    n < 0 && return mx
    Combinatorics.lucasnum(n)
end

@doap function LucasL(n::Integer,x)
    n < 0 && return mx
    p = _lucas_poly(n)
    fromjuliaPolynomial(p,x)
end

let xvar1 = Polynomials.Poly([0,1])
    global _lucas_poly
    @memoize function _lucas_poly(n::Integer)
        n == zero(n) ? convert(typeof(n),2) : n == 1 ? xvar1 :  xvar1 *  _lucas_poly(n-1) + _lucas_poly(n-2)
    end
end

#### Interface to Julia Polynomials.jl

## assumes n != 0,  n != 1
function _monomial(var,c,n)
    if c == 1
        mpow(var,n)
    else
        mmul(c,mpow(var,n))
    end
end

## TODO: we can use some logic to avoid reevaluation, which
## is orders of magnitude slower than constructing the polynomial.
## Eg. if var is a Symbol, I think we can fix the output Mxprs.

## It may be slightly faster to set fixed when the mxpr is constructed.
function fromjuliaPolynomial(p::Polynomials.Poly, var::Symbol)
    res = _fromjuliaPolynomial(p,var)
    if isa(res,Mxpr)
        a = margs(res)
        foreach(setfixed, a)
    end
    setfixed(res)
end

function fromjuliaPolynomial(p::Polynomials.Poly, var)
    _fromjuliaPolynomial(p,var)
end

function _fromjuliaPolynomial(p::Polynomials.Poly, var)
    cs = Polynomials.coeffs(p)
    nc = length(cs)
    nargs = newargs()
    if nc > 0
        if cs[1] != 0
            push!(nargs,cs[1])
        end
    end
    if nc > 1
        if cs[2] > 1
            push!(nargs,mmul(cs[2],var))
        elseif cs[2] == 1
            push!(nargs,var)
        end
    end
    for i in 3:length(cs)
        c = cs[i]
        c == 0 && continue
        n = i - 1
        push!(nargs, _monomial(var,c,n))
    end
    mxpra(:Plus,nargs)
end

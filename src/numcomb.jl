#### Number theory and combinatorics.

using Primes

if VERSION >= v"0.5-"
    import Combinatorics: permutations
end

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

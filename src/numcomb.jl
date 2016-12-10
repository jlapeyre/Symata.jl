#### Number theory and combinatorics.

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

@doap BinarySearch(a::ListT,x) = MListA(collect(Any,searchsorted(margs(a),x)))

### FactorInteger

@sjdoc FactorInteger """
    FactorInteger(n)

give a list of prime factors of `n` and their multiplicities.
"""
@mkapprule FactorInteger nargs => 1:2
@doap FactorInteger(x) = setfixed(mxpra(:List,do_unpack(factor(x))))


### Primes

@sjdoc Primes """
    Primes(n)

return a collection of the prime numbers `<= n`.
"""

apprules(mx::Mxpr{:Primes}) = do_Primes(mx,margs(mx)...)
do_Primes(mx,args...) = mx
do_Primes(mx,n::Integer) = setfixed(mxpr(:List,primes(n)...))


### Prime

@sjdoc Prime """
    Primes(n)

returns the `n`th prime number.
"""
@mkapprule Prime :nargs => 1
@doap Prime(n::Integer) = sympy[:prime](n)


### PrimePi

@sjdoc PrimePi """
    PrimePi(n)

returns the number of primes less than or equal to `n`.
"""
@mkapprule PrimePi :nargs => 1
@doap PrimePi(n::Integer) = sympy[:primepi](n)

### Permutations

@sjdoc Permutations """
    Permutations(expr)

give a list of all permutations of elements in `expr`.
"""

@mkapprule Permutations nargs => 1:2

## TODO:
## Permutations[list, n] gives all permutations containing at most n elements.
## Permutations[list, {n}] gives all permutations containing exactly n elements
@doap function Permutations(x::Mxpr)
    perms = collect(permutations(margs(x)))
    h = mhead(x)
    len = length(perms)
    nargs = newargs(len)
    @inbounds for i in 1:len
        nargs[i] = setfixed(mxpr(:List,perms[i]))
    end
    setfixed(mxpra(:List,nargs))
end

### IntegerPartitions

# @mkapprule IntegerPartitions
# @doap IntegerPartitions(n::Integer) = 

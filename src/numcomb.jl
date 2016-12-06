### BinarySearch

@mkapprule BinarySearch

@sjdoc BinarySearch """
    BinarySearch(list,item)

returns a list of the indices at which `item` appears in the sorted list `list`.
"""

@doap BinarySearch(a::ListT,x) = MListA(collect(Any,searchsorted(margs(a),x)))

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

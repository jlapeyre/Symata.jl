import Symata: unprotect, protect, @sjdoc, set_attributes

unprotect(:ExpToTrig)

@ex ( ExpToTrig(ex_) := ReplaceRepeated( ex , E^(x_) => Cosh(x) + Sinh(x)) )

protect(:ExpToTrig)

@sjdoc ExpToTrig """
    ExpToTrig(expr)

replace exponentials with trigonometric functions in `expr`.
"""

# We can't do all cases this way. For instance, Array(f, [n1,n2,n3,n4] )
# Now we can (maybe!). We have implemented `Evaluate` that works inside `Table`
# TODO: implementat Array(f, [n1,n2,n3,n4,...] )
# ALSO: Check dynamic vs. lexical scoping. What we have here might not be what we want.

@ex ( Array(f_, n_Integer) := Table(f(i), [i,n]) )
@ex ( Array(f_, [n_Integer, m_Integer]) := Table(f(i,j), [i,n], [j,m]))
@ex ( Array(f_, [n_Integer, m_Integer, p_Integer]) := Table(f(i,j,k), [i,n], [j,m], [k,p]))
@ex ( Array(f_, n_Integer, [a_,b_]) := Table(f(i), [i, Range(a,b,(b-a)/n)]))

@sjdoc Array """
    Array(f,n)

return a `List` of length `n` with elements `f(i)` for `i=1,...,n`

    Array(f,r,n)

return a `List` of length `n` with origin `r`.

    Array(f, n, [a,b])

return a `List` of length `n` of elements `f(v)` with `v` taking equally spaced values from `a` to `b`.

    Array(f, [n,m])

return a `List` of `List`s with elements `f(i,j)` for `i=1,...,n`, `j=1,...,m`
"""

protect(:Array)

# Maybe defined elsewhere
# unprotect(:StringQ)
# @ex StringQ = MatchQ(_AbstractString)
# protect(:StringQ)

# unprotect(:ListQ)
# @ex ListQ = MatchQ(_List)
# protect(:ListQ)

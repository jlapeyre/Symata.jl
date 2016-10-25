import Symata: unprotect, protect, @sjdoc

@sjdoc ExpToTrig """
    ExpToTrig(expr)

replace exponentials with trigonometric functions in `expr`.
"""

protect(:ExpToTrig)

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

@sjdoc Subdivide """
    Subdivide(n)

return a list that divides the unit interval into `n` intervals of equal length.

    Subdivide(xmax,n)

divide the interval `(0,xmax)` into `n` intervals

    Subdivide(xmin,xmax,n)

divide the interval `(xmin,xmax)` into `n` intervals.
"""

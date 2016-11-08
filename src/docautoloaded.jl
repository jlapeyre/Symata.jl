import Symata: unprotect, protect, @sjdoc

## These macro calls are also valid Symata. But, we do not include them in the
## autoloaded Symata code, because we want the text to be searchable before it
## is loadable. e.g. h"interval"

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

@sjdoc TakeDrop """
    TakeDrop(expr, seq)

returns `[Take(expr,seq), Drop(expr,seq)]`.
"""

protect(:TakeDrop)


@sjdoc UpTo """
    Take(expr, UpTo(n))

return `expr` keeping only the first `n` elements, or all elements
is there are less than `n` of them.
"""

protect(:UpTo)

@sjdoc ArrayDepth """
    ArrayDepth(m)

return the depth to which `m` is an array. If `m` is viewed as
a tensor, this is the tensor rank.

`ArrayDepth(m)` is equivalent to `Length(Dimensions(m))`.
"""

@sjdoc TensorRank """
    TensorRank(m)

return the depth to which `m` is an array. If `m` is viewed as
a tensor, this is the tensor rank.
"""


@sjdoc Divide """
    Divide(x,y)

is equivalent to `x * y^(-1)`.
"""

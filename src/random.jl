### RandomReal

@mkapprule RandomReal

@doap RandomReal() = return rand()

### Random

@sjdoc Random """
    Random(Integer)

return 0 or 1 with equal probability.

    Random(Integer, [lo,hi])

return a random integer between `lo` and `hi`, inclusive.

    Random(Real)

return a random floating-point number between 0 and 1.

    Random(Complex)

return a random complex floating-point number with parts between 0 and 1.
"""

@mkapprule Random

@doap Random() = rand()

@doap function Random(sym::SJSym)
    if sym == :Integer
        rand(0:1)
    elseif sym == :Real
        rand()
    elseif sym == :Complex
        complex(rand(),rand())
    else
        mx
    end
end

@doap function Random(sym::SJSym, lst::ListT)
    if sym == :Integer
        rand(UnitRange(margs(lst)...))
    end
end

### RandomInteger

@sjdoc RandomInteger """
    Random([nmin,nmax])

return a random integer between `nmin` and `nmax`.

    RandomInteger(nmax)

return a random integer between 0 and `nmax`.

    RandomInteger()

return 0 or 1 with equal probablility.

    RandomInteger(range, n)

return an array of `n` random integers
"""

@mkapprule RandomInteger

@doap RandomInteger() = rand(0:1)
@doap RandomInteger(n::Integer) = rand(0:n)

@doap RandomInteger(x::Mxpr{:List}) = _randintrange(margs(x)...)

_randintrange(n::Integer,m::Integer) = rand(n:m)

@doap function RandomInteger(x::Mxpr{:List},n::Integer)
    lims = x[1]:x[2]
    nargs = newargs(n)
    for i=1:n  nargs[i] = rand(lims) end
    mxpr(:List,nargs)
end 


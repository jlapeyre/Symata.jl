#### RandomReal

@mkapprule RandomReal

do_RandomReal(mx::Mxpr{:RandomReal}) = return rand()

#### Random

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

@doap function Random(sym::SJSym, lst::List)
    if sym == :Integer
        rand(UnitRange(margs(lst)...))
    end
end

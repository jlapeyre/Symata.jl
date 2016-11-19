## TODO: move this code somewhere more appropriate
function Base.convert(::Type{AbstractFloat}, mx::Mxpr{:DirectedInfinity})
    if length(mx) == 1
        margs(mx)[1] == 1 && return Inf
        margs(mx)[1] == -1 && return -Inf
    end
    symerror("Can't convert ", wrapout(mx), " to float.")  # symerror already does wrapout
end

#### NIntegrate

@mkapprule NIntegrate

@sjdoc NIntegrate """
    NIntegrate( expr, [x,x0,xf])

Integrate the Symata expression `expr` numerically  between `x0` and `xf`.

    NIntegrate( expr , [x,x0,x1,...,xf])

Specify singular points between the limitx `x0` and `xf`.

    NIntegrate( f, [x0,xf])
    NIntegrate( f , [x0,x1,...,xf])

Integrate the compiled function `f` numerically.

NIntegrate returns `[result, err]`.

!!! note
    Giving an uncompiled expression is slower than giving a compiled function. But, for
    many purposes, the difference in efficiency is not important.
    `f = Compile(expr)` compiles `expr` to the function `f`.
"""

quadgklist(f,args) = mxprcf(:List, quadgk(f, map( x -> float(doeval(x)), args)...)...)

@doap function NIntegrate(expr, range::Mxpr{:List})
    r = margs(range)
    (sym,x0,x1) = length(r) == 3 ? (r...) : length(r) == 2 ? (:x,r...) : symerror(range, " is not a valid integration ranges")
    f = doeval(expr)
    isa(f,Function) && return quadgklist(f,(x0,x1))
    quadgklist(wrap_symata(expr,sym), (x0,x1))
end

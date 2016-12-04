### Coefficient

@mkapprule Coefficient :nargs => 2:3

@doap function Coefficient(expr, x)
    res = coefficient0(mx,expr,x)
    res == :nothing ? mx : res
end

@doap function Coefficient(expr, x, pow)
    pow == 0 && return coefficient_free(mx,expr,x)
    res = coefficient0(mx,expr, mxpr(:Power, x,pow))
    res == :nothing ? mx : res
end

coefficient0(mx,expr,x::Number) = mx
coefficient0(mx,expr,x) = coefficient(expr,x)

coefficient(expr::TimesT, x) = coefficient_times(expr,[x])

function coefficient(expr::TimesT, x::TimesT)
    if isa(x[1],Number)
        fac = x[1]
        res = coefficient_times(expr,margs(x)[2:end])
        res == 0 && return 0
        return mmul(res, mpow(fac,-1))
    else
        coefficient_times(expr,x)
    end
end

function coefficient_times(expr::TimesT, coll)
    expr == coll && return 1
    posns = findin(expr,coll)
    isempty(posns) && return 0
    (length(posns) != length(coll)) && return 0
    nargs = deleteat!(copy(margs(expr)),posns)
    length(nargs) == 1 && return nargs[1]
    mxpra(mhead(expr), nargs)
end

coefficient(expr,x) = x == expr ? 1 : 0
coefficient(expr::PlusT,x::PlusT) = x == expr ? 1 : 0

function coefficient(expr::PlusT, x)
    nargs = newargs()
    for term in expr
        res = coefficient(term,x)
        if res != 0
            push!(nargs,res)
        end
    end
    isempty(nargs) && return 0
    MPlusA(nargs)
end

coefficient_free(mx,expr,x::Number) = mx
coefficient_free(mx,expr,x) = coefficient_free1(expr,x)

## Very, very strange bug. The use of macro @extomx sometimes causes segfault during compiling, sometimes not,
## even if all other code does not change.
## Eg.
## 1. Check out a particluar commit and compile and run all tests.
## 2. Edit some files
## 3. Check out same commit. Compiling causes segfault.
## Just adding these comments caused the compliation to succeed.
## Adding the previous line "Just..." now causes compilation to fail
function coefficient_free1(expr,x)
    return (freeq(expr,x) && freeq(expr, MPower(x,MBlank())))  ? expr : 0  # this works fine, as well
#    return (freeq(expr,x) && freeq(expr, @extomx( $x^_ ))) ? expr : 0   # This is too tempermental
end

function coefficient_free1(expr::PlusT,x)
    nargs = newargs()
    for term in expr
        res = coefficient_free1(term,x)
        if res != 0
            push!(nargs,res)
        end
    end
    isempty(nargs) && return 0
    MPlusA(nargs)
end

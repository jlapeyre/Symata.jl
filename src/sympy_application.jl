#using Symata.JSymPy
using PyCall

# TODO: refactor code here and in math_functions.jl.

# Wrap a sympy function with an Symata "function"
# Pass keyword arguments. In Symata, these are expressions with head 'Rule'
# We use this for more than just simplification functions.
# TODO: check number of args, etc.
# TODO: some do not take kwargs. don't waste time looking for them.
macro make_simplify_func(mxprsym, sympyfunc)
    smxprsym = string(mxprsym)[2:end]     # Symata symbol
    ssympyfunc = string(sympyfunc)        # SymPy function
    qsympyfunc = QuoteNode(sympyfunc)
    esc(quote
        function apprules(mx::Mxpr{$mxprsym})
              kws = Dict()                         # To hold keywords
              nargs = sjtopy_kw(mx,kws)        # extract keywords from args to mx into kws, return positional args
             if (length(kws) > 0 )
                 sres = sympy[$qsympyfunc](nargs...; kws...) |> pytosj
              else
                 sres = sympy[$qsympyfunc](nargs...) |> pytosj
              end
              deepsetfixed(sres)                   # sometimes this seems like a good idea.
              sres
        end
        set_pattributes( [$smxprsym], :Protected)
        register_sjfunc_pyfunc($smxprsym,$ssympyfunc)
    end)
end

macro make_simplify_func_postp(mxprsym, sympyfunc, postfunc)
    smxprsym = string(mxprsym)[2:end]     # Symata symbol
    ssympyfunc = string(sympyfunc)        # SymPy function
    qsympyfunc = QuoteNode(sympyfunc)
    esc(quote
        function apprules(mx::Mxpr{$mxprsym})
              kws = Dict()                         # To hold keywords
              nargs = sjtopy_kw(mx,kws)        # extract keywords from args to mx into kws, return positional args
             if (length(kws) > 0 )
                 sres = sympy[$qsympyfunc](nargs...; kws...) |> pytosj
              else
                 sres = sympy[$qsympyfunc](nargs...) |> pytosj
              end
              deepsetfixed(sres)                   # sometimes this seems like a good idea.
              $postfunc(sres)
        end
        set_pattributes( [$smxprsym], :Protected)
        register_sjfunc_pyfunc($smxprsym,$ssympyfunc)
    end)
end


#### Factor

@sjdoc Factor """
    Factor(expr)

factor `expr`. Options are,  `modulus`, `gaussian`, `extension`, and `domain`.

```
modulus => n, gaussian => False, extention => Sqrt(2).
```
"""

@make_simplify_func :Factor factor

#### Expand

@sjdoc Expand """
    Expand(expr)

expand powers and products in `expr`.
"""

@make_simplify_func :Expand expand

#### Limit

@sjdoc Limit """
    Limit(expr, var => lim)

give the `limit` of `expr` as `var` approaches `lim`.
"""

function apprules(mx::Mxpr{:Limit})
    (pymx,var,lim) = map(sjtopy, (mx[1],mx[2][1],mx[2][2]))
    pylimit = sympy[:limit](pymx,var,lim)
    return pytosj(pylimit)
end

register_sjfunc_pyfunc("Limit", "limit")

#### Integrate

@sjdoc Integrate """
    Integrate(expr, x)

gives the indefinite integral of `expr` with respect to `x`.

    Integrate(expr, [x,a,b])

gives the definite integral.
"""

#apprules(mx::Mxpr{:Integrate}) = do_Integrate(mx,margs(mx)...)
# Works for exp with one variable. Is supposed to integrate wrt all vars., but gives error instead.
function do_Integrate(mx::Mxpr{:Integrate},expr)
    pymx = sjtopy(expr)
    pyintegral = sympy[:integrate](pymx)
    return pytosj(pyintegral)
end

function do_Integrate(mx::Mxpr{:Integrate}, expr, varspecs...)
    pymx = sjtopy(expr)
    pyvarspecs = varspecs_to_tuples_of_sympy(collect(varspecs))
    pyintegral = sympy[:integrate](pymx,pyvarspecs...)
    sjres = pytosj(pyintegral)
    # This is done in sympy.jl now
    # if mhead(sjres) == :Integrate  # probably wrong wrt false positives and negatives
    #     deepsetfixed(sjres)  # we need this to avoid infinite eval
    # end
    sjres
end

function do_Integrate_kws(mx::Mxpr{:Integrate}, kws, expr)
    pymx = sjtopy(expr)
    pyintegral = sympy[:integrate](pymx; kws)
    return pytosj(pyintegral)
end

# We annotate Dict here to fix BoundsError bug in Integrate(x)
function do_Integrate_kws{T<:Dict}(mx::Mxpr{:Integrate}, kws::T, expr, varspecs...)
    pymx = sjtopy(expr)
    pyvarspecs = varspecs_to_tuples_of_sympy(collect(varspecs))
    pyintegral = sympy[:integrate](pymx,pyvarspecs...; kws...)
    sjres = pytosj(pyintegral)
    # if mhead(sjres) == :Integrate  # probably wrong wrt false positives and negatives
    #     deepsetfixed(sjres)  # we need this to avoid infinite eval
    # end
    sjres
end

# FIXME: we do deepsetfixed and a symbol Int is returned. If we pull it out,
# it is evaluated to Infinity[1] somehow. Maybe this is positive float Inf
function apprules(mx::Mxpr{:Integrate})
    kws = Dict()
    nargs = separate_rules(mx,kws)
    # Two reasons for following. 1. We prefer 'separate' as default. 2. works around inf eval loop
    # in case no conds are given
    # if ! haskey(kws, :conds)
    #     kws[:conds] = "separate"
    # end
    res = isempty(kws) ? do_Integrate(mx,margs(mx)...) : do_Integrate_kws(mx,kws,nargs...)
    res = list_to_conditional_expression(res)
    fix_integrate_piecewise(typeof(mx),res)
end

register_sjfunc_pyfunc("Integrate", "integrate")

## Sympy returns the unevaluated form as the last member of Piecewise. Mma does not do this.
## In Symata it causes an infinite evaluation loop. So we remove these final forms for Integrate and Sum
fix_integrate_piecewise(y,x) = x
function fix_integrate_piecewise(mxprtype, mx::Mxpr{:Piecewise})
    isempty(mx) && return mx
    length(mx) < 2 && return mx
    last = mx[end]
    isempty(last) && return mx
    if isa(last,Mxpr{:ConditionalExpression})
        isa(last[1], mxprtype) && return mx[1]
    end
#    isa(mx[1],mxprtype) && pop!(mx)
    return length(mx) == 1 ? mx[1] : mx
end

list_to_conditional_expression(mx::ListT) = mxpra(:ConditionalExpression, margs(mx))
list_to_conditional_expression(mx) = mx

#### LaplaceTransform

@sjdoc LaplaceTransform """
    LaplaceTransform(expr, t, s)

give the Laplace transform of `expr`.

This function returns `[F, a, cond]` where `F` is the Laplace transform of `f`,
`Re(s)>a` is the half-plane of convergence, and `cond` are auxiliary convergence conditions.
(This additional information is currently disabled.)
"""

function apprules(mx::Mxpr{:LaplaceTransform})
    kws = Dict( :noconds => true )
    nargs = sjtopy_kw(mx,kws)
    pyres = @try_sympyfunc sympy[:laplace_transform](nargs...; kws...)  "LaplaceTransform: unknown error."  mx
    res = pyres |> pytosj
    res = list_to_conditional_expression(res)
    fix_integrate_piecewise(typeof(mx),res)  #
end

#### InverseLaplaceTransform

@sjdoc InverseLaplaceTransform """
   InverseLaplaceTransform(expr, s, t)

gives the inverse Laplace transform of `expr`.
"""

function apprules(mx::Mxpr{:InverseLaplaceTransform})
    result = sympy[:inverse_laplace_transform](map(sjtopy, margs(mx))...)
    sjresult = pytosj(result)
    if is_Mxpr(sjresult) && mhead(sjresult) == :InverseLaplaceTransform
        setfixed(sjresult)
        if mhead(margs(sjresult)[end]) == :Dummy
            pop!(margs(sjresult)) # we may also want to strip the Dummy()
        end
    end
    sjresult = list_to_conditional_expression(sjresult)
    fix_integrate_piecewise(typeof(mx),sjresult)
end

#### FourierTransform
# TODO, pass options (rules)

@sjdoc FourierTransform """
    FourierTransform(expr, x, k)

gives the Fourier transform of `expr`.

This function returns `[F, cond]` where `F` is the Fourier transform of `f`,
and `cond` are auxiliary convergence conditions.
"""

apprules(mx::Mxpr{:FourierTransform}) = sympy[:fourier_transform](sjtopy(margs(mx))...) |> pytosj


#### InverseFourierTransform

function apprules(mx::Mxpr{:InverseFourierTransform})
    result = sympy[:inverse_fourier_transform](map(sjtopy, margs(mx))...)
    sjresult = pytosj(result)
    if mhead(sjresult) == :InverseFourierTransform
        setfixed(sjresult)
        if mhead(margs(sjresult)[end]) == :Dummy
            pop!(margs(sjresult)) # we may also want to strip the Dummy()
        end
    end
    sjresult = list_to_conditional_expression(sjresult)
    sjresult
end

#### Sum

@sjdoc Sum """
    Sum(expr, [x,a,b])

sums over `x` from `a` to `b`.
"""

apprules(mx::Mxpr{:Sum}) = do_Sum(mx,margs(mx)...)

function do_Sum(mx::Mxpr{:Sum}, expr, varspecs...)
    ## FIXME: only evaluate expr here if the range of the sum is infinite.
    pymx = sjtopy(doeval(expr))
    pyvarspecs = varspecs_to_tuples_of_sympy(reverse(collect(varspecs))) # Symata and Mma use the same convention for position of inner loop
    pysum = sympy[:summation](pymx,pyvarspecs...)
    res = pytosj(pysum)
    if mhead(res) == :Sum
        summand = res[1]
        specs = margs(res)[2:end]
        return mxpr(:Sum,summand,reverse(specs)...)
    end
    fix_integrate_piecewise(typeof(mx),res)
#    return is_Mxpr(res,:Piecewise) ? res[1] : res
end

#### Product

@sjdoc Product """
    Product(expr, [x,a,b])

the product of `expr` over `x` from `a` to `b`.
"""

apprules(mx::Mxpr{:Product}) = do_Product(mx,margs(mx)...)

function do_Product(mx::Mxpr{:Product}, expr, varspecs...)
    pymx = sjtopy(expr)
    pyvarspecs = varspecs_to_tuples_of_sympy(collect(varspecs))
    pysum = sympy[:product](pymx,pyvarspecs...)
    return pytosj(pysum)
end

register_sjfunc_pyfunc("Product", "product")

#### Series

@sjdoc Series """
    Series(expr,[x,x0,n])

give the Taylor series expansion of `expr`.
"""

apprules(mx::Mxpr{:Series}) = do_Series(mx,margs(mx)...)

function do_Series(mx::Mxpr{:Series}, expr, varspecs...)
    pymx = sjtopy(expr)
    pyspec = []
    for dspec in margs(mx)[2:end]    # Following is more than neccessary. Also, maybe we could use tuples instead of lists
        if is_Mxpr(dspec,:List)
            for xdspec in margs(dspec)
                push!(pyspec,sjtopy(xdspec))
            end
        else
            push!(pyspec,sjtopy(dspec))
        end
    end
    pyseries = sympy[:series](pymx,pyspec...)
    return pytosj(pyseries)
end

register_sjfunc_pyfunc("Series", "series")

#### D  (derivative)

## TODO: Implement D and Derivative in Symata. This will still fall through to SymPy
## TODO: In Mma the derivative of a function returns a pure function.

@sjdoc D """
    D(expr, x)

give the partial derivative of `expr` with respect to `x`.

    D(expr,[x,n])

give the `n`th partial derivative.

    D(expr,[x,n1],y,[z,n2])

give the mixed derivative.
"""

function apprules(mx::Mxpr{:D})
    pymx = sjtopy(mx[1])
    pyspec = []
    for dspec in margs(mx)[2:end]  # D(expr, [x,1], y, ...) --> diff(expr,x,1,y,...)
        if is_Mxpr(dspec,:List)
            for xdspec in margs(dspec)
                push!(pyspec,sjtopy(xdspec))
            end
        else
            push!(pyspec,sjtopy(dspec))
        end
    end
    pyderivative = sympy[:diff](pymx,pyspec...)
    return pytosj(pyderivative)
end

register_sjfunc_pyfunc("D", "diff")

#### Together

@sjdoc Together """
    Together(sum)

rewrite a sum of terms as a product.
"""

@make_simplify_func :Together together

#### Apart

@sjdoc Apart """
    Apart(product)

compute a partial fraction decomposition of `product`.
"""

apprules(mx::Mxpr{:Apart}) = sympy[:apart](map(sjtopy, margs(mx))...)[:doit]() |> pytosj

register_sjfunc_pyfunc("Apart", "apart")

#### Simplify

@sjdoc Simplify """
    Simplify(expr, kw1 => v1, ...)

rewrites `expr` in a simpler form using keyword options `kw1, ...`.
"""

@make_simplify_func :Simplify simplify
@make_simplify_func :TrigSimp trigsimp
@make_simplify_func :ExpTrigSimp exptrigsimp
@make_simplify_func :RatSimp ratsimp
@make_simplify_func :RadSimp radsimp
@make_simplify_func :PowSimp powsimp
@make_simplify_func :PowDenest powdenest
@make_simplify_func :LogCombine logcombine
@make_simplify_func :ExpandTrig expand_trig
@make_simplify_func :ExpandLog expand_log
@make_simplify_func :SeparateVars separatevars
@make_simplify_func :BesselSimp besselsimp
@make_simplify_func :HyperSimp hypersimp
@make_simplify_func :HyperExpand hyperexpand
@make_simplify_func :NSimplify nsimplify
@make_simplify_func :CombSimp combsimp
@make_simplify_func :SqrtDenest sqrtdenest
@make_simplify_func :Div div

@make_simplify_func_postp :Cse cse _cse_post

function _cse_post(lists)
    (rulelist,expr) = (reverse(margs(lists[1])),lists[2]) # have we defined splat for Mxpr ? we should do it.
    nargs = newargs()
    for x in rulelist
        push!(nargs, mxpr(:Rule, margs(x)))
    end
    rules = mxpr(:List,nargs)
    mxpr(:List, expr, rules)
end

# These apparently have been removed from SymPy
#@make_simplify_func :Separate separate
#@make_simplify_func :OptCse opt_cse  maybe renamed cse_opts
#@make_simplify_func :CollectSqrt collectsqrt apparently gone


@sjdoc TrigSimp """
    TrigSimp(expr)

does trigonometric simplification.
"""

@sjdoc RatSimp """
    RatSimp(expr)

rewrite `expr` with a common denominator, cancel and reduce.
"""

@sjdoc RadSimp """
    RadSimp(expr)

rationalize the denominator.
"""

#### FullSimplify

@sjdoc FullSimplify """
    FullSimplify(expr)

rewrite `expr` in a simpler form, algorithm is more extensive than `Simplify(expr)`,
but likely to be slower.
"""

apprules(mx::Mxpr{:FullSimplify}) = do_FullSimplify(mx)

function do_FullSimplify(mx::Mxpr{:FullSimplify})
    funcs = [sympy[:simplify], sympy[:expand], sympy[:fu], sympy[:powsimp], sympy[:sqrtdenest]]
    objective = pyeval("lambda x: len(str(x))")
    megasimp = sympy[:strategies][:tree][:greedy]((funcs, funcs), objective)
    mx[1] |> sjtopy |> megasimp |> pytosj
end

@sjdoc Cancel """
    Cancel(expr)

cancel common factors in the numerator and denominator.
"""

apprules(mx::Mxpr{:Cancel}) = mx[1] |> sjtopy |> sympy[:cancel] |> pytosj

register_sjfunc_pyfunc("Cancel", "cancel")

@sjdoc Collect """
    Collect(expr,x)

collect terms involving the same power of `x`.

    Collect(expr,[x,y])

collect terms involving first `x`, then `y`.
"""

@mkapprule Collect

@doap Collect(expr,x) = sympy[:collect](expr |> sjtopy, x |> sjtopy ) |> pytosj
@doap Collect(expr,x,lst::Mxpr{:List}) = sympy[:collect](expr |> sjtopy, x |> sjtopy , list |> sjtopy) |> pytosj

register_sjfunc_pyfunc("Collect", "collect")

#### Solve

@sjdoc Solve """
    Solve(expr)

solve `expr == 0` for one variable.

    Solve(expr,var)

solve `expr == 0` for `var`.

    Solve([expr1,expr2,...], [var1,var2,...])

solve a system of equations.
"""

@mkapprule Solve :nargs => 1:2


# TODO: find free symbols in expr and return rules as in following methods
@doap function Solve(expr)
    sres = (expr |> sjtopy |> sympy[:solve] |> pytosj)
#    mxpr(:List, (map(t -> mxpr(:List, mxpr(:Rule,sym,t)), margs(sres)))...)
end

@doap function Solve(expr, var::Symbol)
    pyexpr = expr |> sjtopy
    pyvar = var |> sjtopy
    res =  sympy[:solve](pyexpr,pyvar)
    sres = res |>  pytosj
    mxpr(:List, (map(t -> mxpr(:List, mxpr(:Rule,var,t)), margs(sres)))...)
end

@doap function Solve(eqs::Mxpr{:List}, vars::Mxpr{:List})
    peqs = eqs |> sjtopy
    pyvars = vars |> sjtopy
    sres = sympy[:solve](peqs,pyvars) |>  pytosj
    if isa(sres,Dict)  # why does pytosj sometimes return Dict and sometimes List ?
        nargs = newargs()
        for (k,v) in sres
            push!(nargs, mxpr(:Rule, k, v))
        end
        mxpr(:List, mxpr(:List, nargs...))
    else
        return sres  # TODO. convert these to Rules
    end
end

register_sjfunc_pyfunc("Solve", "solve")

# This is broken
apprules(mx::Mxpr{:DSolve}) = do_DSolve(mx,margs(mx)...)
do_DSolve(mx, expr) = expr |> sjtopy |> sympy[:dsolve] |> pytosj


#### Roots
@sjdoc Roots """
    Roots(expr)

solves for the roots of `expr`.

returns a `List` of `Lists`. The two elements of each sublist give the root and its multiplicity.
"""

apprules(mx::Mxpr{:Roots}) = mx[1] |> sjtopy |> sympy[:roots] |> pytosj  |> Symata.unpacktoList

register_sjfunc_pyfunc("Roots", "roots")

### RealRoots
@sjdoc RealRoots """
    RealRoots(expr)

solves for the real roots of `expr`.
"""

apprules(mx::Mxpr{:RealRoots}) = mx[1] |> sjtopy |> sympy[:real_roots] |> pytosj

register_sjfunc_pyfunc("RealRoots", "real_roots")


### Singularities

@mkapprule Singularities :nargs => 2

@doap function Singularities(expr,var)
    res = pytosj(sympy[:singularities](sjtopy(expr),sjtopy(var)))
    if isa(res,Mxpr{:EmptySet})
        return List()
    elseif isa(res,Mxpr{:FiniteSet})
        mxpr(:List, map( x -> mxpr(:Rule, var, x), margs(res))...)
    else
        return res
    end
end

### TODO: IsIncreasing, etc.

#### ToSymPy

@sjdoc ToSymPy """
    ToSymPy(expr)

convert `expr` to a (python) PyObject.
"""

function apprules(mx::Mxpr{:ToSymPy})
    res = sjtopy(margs(mx)...)
end

#### ToSymata

@sjdoc ToSymata """
    ToSymata(expr)

convert the python PyObject `expr` to a Symata expression. Normally, expressions computed
by SymPy are automatically converted to Symata expressions.
"""

apprules(mx::Mxpr{:ToSymata}) = do_ToSymata(mx,margs(mx)...)

do_ToSymata(mx::Mxpr{:ToSymata}, s::Symbol) = setfixed(_pytosj(symval(s)))
do_ToSymata(mx::Mxpr{:ToSymata}, e::PyCall.PyObject) = setfixed(_pytosj(e))
do_ToSymata(mx::Mxpr{:ToSymata}, x) = x

#### PossibleClosedForm

apprules(mx::Mxpr{:PossibleClosedForm}) = do_PossibleClosedForm(mx,margs(mx)...)

@sjdoc PossibleClosedForm """
    PossibleClosedForm(x)

attempts to find an exact formula for the floating point number `x`.
"""

do_PossibleClosedForm(mx::Mxpr{:PossibleClosedForm},x::AbstractFloat) =  x |>  mpmath.identify |> sympy[:sympify] |> pytosj

function do_PossibleClosedForm(mx::Mxpr{:PossibleClosedForm}, args...)
    kws = Dict()
    nargs = sjtopy_kw(mx,kws)
    pyargs = sjtopy(nargs...)
    setdps::Bool = false
    if haskey(kws,"dps")
        set_mpmath_dps(kws["dps"])
        delete!(kws, "dps")
        setdps = true
    end
    pyres = mpmath.identify(pyargs...; kws...)
    if setdps restore_mpmath_dps() end
    pyres |> sympy[:sympify] |> pytosj
end

##### ConditionalExpression

# There are several other properties to implement here.

@sjdoc ConditionalExpression """
    ConditionalExpression(expr,cond)

returns `expr` only when `cond` is true. If `cond` is false,
return `Undefined`.
"""

@mkapprule ConditionalExpression :nargs => 2
@doap ConditionalExpression(expr, cond::Bool) = cond ? expr : Undefined
@doap ConditionalExpression(expr, cond) = mx

#### Refine

@mkapprule Refine  :nodefault => true

@sjdoc Refine """
    Refine(expr)

simplifies expr using assumptions. For instance, `Assume(x,positive)`.
"""

@doap function Refine(args...)
    result = sympy[:refine](map(sjtopy, args)...)
    result |> pytosj
end

## utility

# input -- Array of Symata Lists and/or Symbols
# output -- Array of tuples (from Lists) of SymPy objects, or single SymPy objects
# Eg: For translating Integrate(expr,[x,a,b],y) --> integrate(expr,(x,a,b),y)
function varspecs_to_tuples_of_sympy(args::Array)
    oarr = []
    for x in args
        if is_Mxpr(x,:List)
            push!(oarr, tuple(map(sjtopy, margs(x))...))
        else
            push!(oarr,sjtopy(x))
        end
    end
    return oarr
end

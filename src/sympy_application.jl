#using SJulia.JSymPy
using PyCall

# TODO: refactor code here and in math_functions.jl.

# Wrap a sympy function with an SJulia "function"
# Pass keyword arguments. In SJulia, these are expressions with head 'Rule'
# We use this for more than just simplification functions.
# TODO: check number of args, etc.
# TODO: some do not take kwargs. don't waste time looking for them.
macro make_simplify_func(mxprsym, sympyfunc)
    smxprsym = string(mxprsym)[2:end]     # SJulia symbol
    ssympyfunc = string(sympyfunc)        # SymPy function
    esc(quote
        function apprules(mx::Mxpr{$mxprsym})
              kws = Dict()                         # To hold keywords
              nargs = sjtopy_kw(mx,kws)        # extract keywords from args to mx into kws, return positional args
              if (length(kws) > 0 )
                 sres = sympy.$sympyfunc(nargs...; kws...) |> pytosj
              else
                 sres = sympy.$sympyfunc(nargs...) |> pytosj
              end
              deepsetfixed(sres)                   # sometimes this seems like a good idea.
              sres
        end
        set_pattributes( [$smxprsym], :Protected)
        register_sjfunc_pyfunc($smxprsym,$ssympyfunc)
    end)
end


#### Factor

@sjdoc Factor "
Factor(expr) factors expr. Options are,  modulus, gaussian, extension, and domain.
For example modulus => n, gaussian => False, extention => Sqrt(2).
"

@make_simplify_func :Factor factor

#### Expand

@sjdoc Expand "
Expand(expr) expands powers and products in expr. This is the sympy version, which is more capable,
 but slower than ExpandA.
"

@make_simplify_func :Expand expand

#### Limit

@sjdoc Limit "
Limit(expr, var => lim) gives the limit of expr as var approaches to lim.
"

function apprules(mx::Mxpr{:Limit})
    (pymx,var,lim) = map(sjtopy, (mx[1],mx[2][1],mx[2][2]))
    pylimit = sympy.limit(pymx,var,lim)
    return pytosj(pylimit)
end

register_sjfunc_pyfunc("Limit", "limit")

#### Integrate

@sjdoc Integrate "
Integrate(expr, x) gives the indefinite integral of expr with respect to x.
Integrate(expr, [x,a,b]) gives the definite integral.
"

#apprules(mx::Mxpr{:Integrate}) = do_Integrate(mx,margs(mx)...)
# Works for exp with one variable. Is supposed to integrate wrt all vars., but gives error instead.
function do_Integrate(mx::Mxpr{:Integrate},expr)
    pymx = sjtopy(expr)
    pyintegral = sympy.integrate(pymx)
    return pytosj(pyintegral)
end

function do_Integrate(mx::Mxpr{:Integrate}, expr, varspecs...)
    pymx = sjtopy(expr)
    pyvarspecs = varspecs_to_tuples_of_sympy(collect(varspecs))
    pyintegral = sympy.integrate(pymx,pyvarspecs...)
    sjres = pytosj(pyintegral)
    if mhead(sjres) == :Integrate  # probably wrong wrt false positives and negatives
        deepsetfixed(sjres)  # we need this to avoid infinite eval
    end
    sjres
end

function do_Integrate_kws(mx::Mxpr{:Integrate}, kws, expr)
    pymx = sjtopy(expr)
    pyintegral = sympy.integrate(pymx; kws)
    return pytosj(pyintegral)
end

# We annotate Dict here to fix BoundsError bug in Integrate(x)
function do_Integrate_kws{T<:Dict}(mx::Mxpr{:Integrate}, kws::T, expr, varspecs...)
    pymx = sjtopy(expr)
    pyvarspecs = varspecs_to_tuples_of_sympy(collect(varspecs))
    pyintegral = sympy.integrate(pymx,pyvarspecs...; kws...)
    sjres = pytosj(pyintegral)
    if mhead(sjres) == :Integrate  # probably wrong wrt false positives and negatives
        deepsetfixed(sjres)  # we need this to avoid infinite eval
    end
    sjres
end

# FIXME: we do deepsetfixed and a symbol Int is returned. If we pull it out,
# it is evaluated to Infinity[1] somehow. Maybe this is positive float Inf
function apprules(mx::Mxpr{:Integrate})
    kws = Dict()
    nargs = separate_rules(mx,kws)
    # Two reasons for following. 1. We prefer 'separate' as default. 2. works around inf eval loop
    # in case no conds are given
    if ! haskey(kws, :conds)
        kws[:conds] = "separate"
    end
    if length(kws) == 0
        res = do_Integrate(mx,margs(mx)...)
    else
        res = do_Integrate_kws(mx,kws,nargs...)
    end
    if is_Mxpr(res,:List)
        return mxpr(:ConditionalExpression, margs(res)...)
    end
    res
end

register_sjfunc_pyfunc("Integrate", "integrate")

#### LaplaceTransform

@sjdoc LaplaceTransform "
LaplaceTransform(expr, t, s) gives the Laplace transform of expr.
This function returns (F, a, cond) where F is the Laplace transform of f, Re(s)>a is the half-plane of convergence, and cond are auxiliary convergence conditions. (This additional information is currently disabled.)
"

function apprules(mx::Mxpr{:LaplaceTransform})
    kws = Dict( :noconds => true )
    nargs = sjtopy_kw(mx,kws)
    pyres = @try_sympyfunc laplace_transform(nargs...; kws...)  "LaplaceTransform: unknown error."  mx
    res = pyres |> pytosj
    if is_Mxpr(res,:List)
        return mxpr(:ConditionalExpression, margs(res)...)
    end
    res
end

#### InverseLaplaceTransform

@sjdoc InverseLaplaceTransform "
InverseLaplaceTransform(expr, s, t) gives the inverse Laplace transform of expr.
"

function apprules(mx::Mxpr{:InverseLaplaceTransform})
    result = sympy.inverse_laplace_transform(map(sjtopy, margs(mx))...)
    sjresult = pytosj(result)
    if is_Mxpr(sjresult) && mhead(sjresult) == :InverseLaplaceTransform
        setfixed(sjresult)
        if mhead(margs(sjresult)[end]) == :Dummy
            pop!(margs(sjresult)) # we may also want to strip the Dummy()
        end
    end
    if is_Mxpr(sjresult,:List)
        return mxpr(:ConditionalExpression, margs(sjresult)...)
    end
    sjresult
end

#### FourierTransform
# TODO, pass options (rules)

@sjdoc FourierTransform "
FourierTransform(expr, x, k) gives the Fourier transform of expr.
This function returns (F, cond) where F is the Fourier transform of f, and cond are auxiliary convergence conditions.
"

apprules(mx::Mxpr{:FourierTransform}) = sympy.fourier_transform(sjtopy(margs(mx))...) |> pytosj


#### InverseFourierTransform

function apprules(mx::Mxpr{:InverseFourierTransform})
    result = sympy.inverse_fourier_transform(map(sjtopy, margs(mx))...)
    sjresult = pytosj(result)
    if mhead(sjresult) == :InverseFourierTransform
        setfixed(sjresult)
        if mhead(margs(sjresult)[end]) == :Dummy
            pop!(margs(sjresult)) # we may also want to strip the Dummy()
        end
    end
    if is_Mxpr(sjresult,:List)
        return mxpr(:ConditionalExpression, margs(sjresult)...)
    end
    sjresult
end

#### Sum

@sjdoc Sum "
Sum(expr, [x,a,b]) sums over x from a to b
"

apprules(mx::Mxpr{:Sum}) = do_Sum(mx,margs(mx)...)

function do_Sum(mx::Mxpr{:Sum}, expr, varspecs...)
    pymx = sjtopy(expr)
    pyvarspecs = varspecs_to_tuples_of_sympy(collect(varspecs))
    pysum = sympy.summation(pymx,pyvarspecs...)
    return pytosj(pysum)
end


#### Product

@sjdoc Product "
Product(expr, [x,a,b]) computes the product of expr over x from a to b
"

apprules(mx::Mxpr{:Product}) = do_Product(mx,margs(mx)...)

function do_Product(mx::Mxpr{:Product}, expr, varspecs...)
    pymx = sjtopy(expr)
    pyvarspecs = varspecs_to_tuples_of_sympy(collect(varspecs))
    pysum = sympy.product(pymx,pyvarspecs...)
    return pytosj(pysum)
end

register_sjfunc_pyfunc("Product", "product")

#### Series

@sjdoc Series "
Series(expr,[x,x0,n]) gives the Taylor series expansion of expr.
"

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
    pyseries = sympy.series(pymx,pyspec...)
    return pytosj(pyseries)
end

register_sjfunc_pyfunc("Series", "series")

#### D  (derivative)


@sjdoc D "
D(expr, x) gives the partial derivative of expr with respect to x.
D(expr,[x,n]) gives the nth partial derivative.
D(expr,[x,n1],y,[z,n2]) gives the mixed derivative.
"

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
    pyderivative = sympy.diff(pymx,pyspec...)
    return pytosj(pyderivative)
end

register_sjfunc_pyfunc("D", "diff")

#### Together

@sjdoc Together "
Together(sum) rewrites a sum of terms as a product.
"

@make_simplify_func :Together together

#### Apart

@sjdoc Apart "
Apart(product) computes a partial fraction decomposition of product
"
apprules(mx::Mxpr{:Apart}) = mx[1] |> sjtopy |> sympy.apart |> pytosj

register_sjfunc_pyfunc("Apart", "apart")

#### Simplify

@sjdoc Simplify "
Simplify(expr, kw1 => v1, ...) rewrites expr in a simpler form using keyword options kw1, ...
"

@make_simplify_func :Simplify simplify
@make_simplify_func :TrigSimp trigsimp
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
@make_simplify_func :Cse cse
@make_simplify_func :Div div

# These apparently have been removed from SymPy
#@make_simplify_func :Separate separate
#@make_simplify_func :OptCse opt_cse  maybe renamed cse_opts
#@make_simplify_func :CollectSqrt collectsqrt apparently gone


@sjdoc TrigSimp "
TrigSimp(expr) does trigonometric simplification.
"

@sjdoc RatSimp "
Put an expression over a common denominator, cancel and reduce.
"

@sjdoc RadSimp "
Rationalize the denominator.
"

#### FullSimplify

@sjdoc FullSimplify "
FullSimplify(expr) rewrites expr in a simpler form, algorithm is more extensive than Simplify(expr),
but likely to be slower.
"
apprules(mx::Mxpr{:FullSimplify}) = do_FullSimplify(mx)

function do_FullSimplify(mx::Mxpr{:FullSimplify})
    funcs = [sympy.simplify, sympy.expand, sympy.fu, sympy.powsimp, sympy.sqrtdenest]
    objective = pyeval("lambda x: len(str(x))")
    megasimp = sympy.strategies[:tree][:greedy]((funcs, funcs), objective)
    mx[1] |> sjtopy |> megasimp |> pytosj
end

@sjdoc Cancel "
Cancel(expr) cancels common factors in the numerator and denominator.
"

apprules(mx::Mxpr{:Cancel}) = mx[1] |> sjtopy |> sympy.cancel |> pytosj

register_sjfunc_pyfunc("Cancel", "cancel")

@sjdoc Collect "
Collect(expr,x) collects terms involving the same power of x.
Collect(expr,[x,y]) collects terms involving first x, then y.
"

@mkapprule Collect

@doap Collect(expr,x) = sympy.collect(expr |> sjtopy, x |> sjtopy ) |> pytosj
@doap Collect(expr,x,lst::Mxpr{:List}) = sympy.collect(expr |> sjtopy, x |> sjtopy , list |> sjtopy) |> pytosj

register_sjfunc_pyfunc("Collect", "collect")

#### Solve

@sjdoc Solve "
Solves(expr) solves expr == 0 for one variable.
Solves(expr,var) solves expr == 0 for var.
"
apprules(mx::Mxpr{:Solve}) = do_Solve(mx,margs(mx)...)

do_Solve(mx, expr) = expr |> sjtopy |> sympy.solve |> pytosj

function do_Solve(mx, expr, var::Symbol)
    pyexpr = expr |> sjtopy
    pyvar = var |> sjtopy
    res =     sympy.solve(pyexpr,pyvar)
    res |>  pytosj
end

function do_Solve(mx, eqs::Mxpr{:List}, vars::Mxpr{:List})
    peqs = eqs |> sjtopy
    pyvars = vars |> sjtopy
    sympy.solve(peqs,pyvars) |>  pytosj
end

register_sjfunc_pyfunc("Solve", "solve")

# This is broken
apprules(mx::Mxpr{:DSolve}) = do_DSolve(mx,margs(mx)...)
do_DSolve(mx, expr) = expr |> sjtopy |> sympy.dsolve |> pytosj


#### Roots
@sjdoc Roots "
Roots(expr) solves for the roots of expr. Roots returns a list
of lists. The two elements of each sublist give the root and its multiplicity.
"

apprules(mx::Mxpr{:Roots}) = mx[1] |> sjtopy |> sympy.roots |> pytosj  |> SJulia.unpack_to_List

register_sjfunc_pyfunc("Roots", "roots")

#### RealRoots
@sjdoc RealRoots "
RealRoots(expr) solves for the real roots of expr.
"
apprules(mx::Mxpr{:RealRoots}) = mx[1] |> sjtopy |> sympy.real_roots |> pytosj

register_sjfunc_pyfunc("RealRoots", "real_roots")

#### ToSymPy

@sjdoc ToSymPy "
ToSymPy(expr) converts expr to a (python) PyObject.
"

function apprules(mx::Mxpr{:ToSymPy})
    res = sjtopy(margs(mx)...)
end

#### ToSJulia

@sjdoc ToSJulia "
ToSJulia(expr) converts the python PyObject expr to an SJulia expression. Normally, expressions computed
by SymPy are automatically converted to SJulia expressions.
"

apprules(mx::Mxpr{:ToSJulia}) = do_ToSJulia(mx,margs(mx)...)

do_ToSJulia(mx::Mxpr{:ToSJulia}, s::Symbol) = setfixed(_pytosj(symval(s)))
do_ToSJulia(mx::Mxpr{:ToSJulia}, e::PyCall.PyObject) = setfixed(_pytosj(e))
do_ToSJulia(mx::Mxpr{:ToSJulia}, x) = x

#### PossibleClosedForm

apprules(mx::Mxpr{:PossibleClosedForm}) = do_PossibleClosedForm(mx,margs(mx)...)

@sjdoc PossibleClosedForm "
PossibleClosedForm(x) attempts to find an exact formula for the floating point number x.
"

function do_PossibleClosedForm(mx::Mxpr{:PossibleClosedForm},x::AbstractFloat)
   x |>  mpmath.identify |> sympy.sympify |> pytosj
end

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
    pyres |> sympy.sympify |> pytosj
end

##### ConditionalExpression

# There are several other properties to implement here.

@mkapprule ConditionalExpression :nargs => 2
@doap ConditionalExpression(expr, cond::Bool) = cond ? expr : Undefined
@doap ConditionalExpression(expr, cond) = mx

#### Refine

@mkapprule Refine

@sjdoc Refine "
Refine(expr) simplifies expr using assumptions. For instance, `Assume(x,positive)`.
"

@doap function Refine(args...)
    result = sympy.refine(map(sjtopy, args)...)
    result |> pytosj
end

## utility

# input -- Array of SJulia Lists and/or Symbols
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

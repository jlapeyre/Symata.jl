using SJulia.JSymPy
using PyCall

#### Factor

@sjdoc Factor "
Factor(expr) factors expr. This function calls SymPy.
"
apprules(mx::Mxpr{:Factor})  = mx[1] |> mxpr2sympy |> sympy.factor |> sympy2mxpr

#### Expand

@sjdoc Expand "
Expand(expr) expands powers and products in expr. This is the sympy version, which is more capable.
"
apprules(mx::Mxpr{:Expand}) = mx[1] |> mxpr2sympy |> sympy.expand  |> sympy2mxpr

#### Limit

@sjdoc Limit "
Limit(expr, var => lim) gives the limit of expr as var approaches to lim.
"

function apprules(mx::Mxpr{:Limit})
    (pymx,var,lim) = map(mxpr2sympy, (mx[1],mx[2][1],mx[2][2]))
    pylimit = sympy.limit(pymx,var,lim)
    return sympy2mxpr(pylimit)
end

#### Integrate

@sjdoc Integrate "
Integrate(expr, x) gives the indefinite integral of expr with respect to x.
Integrate(expr, [x,a,b]) gives the definite integral.
"

apprules(mx::Mxpr{:Integrate}) = do_Integrate(mx,margs(mx)...)

# Works for exp with one variable. Is supposed to integrate wrt all vars., but gives error instead.
function do_Integrate(mx::Mxpr{:Integrate},expr)
    pymx = mxpr2sympy(expr)    
    pyintegral = sympy.integrate(pymx)
    return sympy2mxpr(pyintegral)    
end

function do_Integrate(mx::Mxpr{:Integrate}, expr, varspecs...)
    pymx = mxpr2sympy(expr)
    pyvarspecs = varspecs_to_tuples_of_sympy(collect(varspecs))
    pyintegral = sympy.integrate(pymx,pyvarspecs...)
    return sympy2mxpr(pyintegral)
end

#### LaplaceTransform

@sjdoc LaplaceTransform "
LaplaceTransform(expr, t, s) gives the Laplace transform of expr.
This function returns (F, a, cond) where F is the Laplace transform of f, Re(s)>a is the half-plane of convergence, and cond are auxiliary convergence conditions.
"

apprules(mx::Mxpr{:LaplaceTransform}) = sympy.laplace_transform(mxpr2sympy(margs(mx))...) |> sympy2mxpr

#### InverseLaplaceTransform

@sjdoc InverseLaplaceTransform "
InverseLaplaceTransform(expr, s, t) gives the inverse Laplace transform of expr.
"

function apprules(mx::Mxpr{:InverseLaplaceTransform})
    result = sympy.inverse_laplace_transform(map(mxpr2sympy, margs(mx))...)
    sjresult = sympy2mxpr(result)
    if mhead(sjresult) == :InverseLaplaceTransform
        setfixed(sjresult)  
        if mhead(margs(sjresult)[end]) == :Dummy
            pop!(margs(sjresult)) # we may also want to strip the Dummy()
        end
    end
    sjresult
end

#### InverseLaplaceTransform

@sjdoc InverseLaplaceTransform "
InverseLaplaceTransform(expr, s, t) gives the inverse Laplace transform of expr.
"

function apprules(mx::Mxpr{:InverseLaplaceTransform})
    result = sympy.inverse_laplace_transform(map(mxpr2sympy, margs(mx))...)
    sjresult = sympy2mxpr(result)
    if mhead(sjresult) == :InverseLaplaceTransform
        setfixed(sjresult)  
        if mhead(margs(sjresult)[end]) == :Dummy
            pop!(margs(sjresult)) # we may also want to strip the Dummy()
        end
    end
    sjresult
end

#### FourierTransform
# TODO, pass options (rules)

@sjdoc FourierTransform "
FourierTransform(expr, x, k) gives the Fourier transform of expr.
This function returns (F, cond) where F is the Fourier transform of f, and cond are auxiliary convergence conditions.
"

apprules(mx::Mxpr{:FourierTransform}) = sympy.fourier_transform(mxpr2sympy(margs(mx))...) |> sympy2mxpr


#### InverseFourierTransform

function apprules(mx::Mxpr{:InverseFourierTransform})
    result = sympy.inverse_fourier_transform(map(mxpr2sympy, margs(mx))...)
    sjresult = sympy2mxpr(result)
    if mhead(sjresult) == :InverseFourierTransform
        setfixed(sjresult)  
        if mhead(margs(sjresult)[end]) == :Dummy
            pop!(margs(sjresult)) # we may also want to strip the Dummy()
        end
    end
    sjresult
end

#### Sum

@sjdoc Sum "
Sum(expr, [x,a,b]) sums over x from a to b
"

apprules(mx::Mxpr{:Sum}) = do_Sum(mx,margs(mx)...)

function do_Sum(mx::Mxpr{:Sum}, expr, varspecs...)
    pymx = mxpr2sympy(expr)
    pyvarspecs = varspecs_to_tuples_of_sympy(collect(varspecs))
    pysum = sympy.summation(pymx,pyvarspecs...)
    return sympy2mxpr(pysum)
end


#### Product

@sjdoc Product "
Product(expr, [x,a,b]) computes the product of expr over x from a to b
"

apprules(mx::Mxpr{:Product}) = do_Product(mx,margs(mx)...)

function do_Product(mx::Mxpr{:Product}, expr, varspecs...)
    pymx = mxpr2sympy(expr)
    pyvarspecs = varspecs_to_tuples_of_sympy(collect(varspecs))
    pysum = sympy.product(pymx,pyvarspecs...)
    return sympy2mxpr(pysum)
end


#### Series

@sjdoc Series "
Series(expr,[x,x0,n]) gives the Taylor series expansion of expr.
"

apprules(mx::Mxpr{:Series}) = do_Series(mx,margs(mx)...)

function do_Series(mx::Mxpr{:Series}, expr, varspecs...)
    pymx = mxpr2sympy(expr)
    pyspec = []
    for dspec in margs(mx)[2:end]    # Following is more than neccessary. Also, maybe we could use tuples instead of lists
        if is_Mxpr(dspec,:List)
            for xdspec in margs(dspec)
                push!(pyspec,mxpr2sympy(xdspec))
            end
        else
            push!(pyspec,mxpr2sympy(dspec))
        end
    end    
    pyseries = sympy.series(pymx,pyspec...)    
    return sympy2mxpr(pyseries)
end


#### D  (derivative)


@sjdoc D "
D(expr, x) gives the partial derivative of expr with respect to x.
D(expr,[x,n]) gives the nth partial derivative.
D(expr,[x,n1],y,[z,n2]) gives the mixed derivative.
"

function apprules(mx::Mxpr{:D})
    pymx = mxpr2sympy(mx[1])
    pyspec = []
    for dspec in margs(mx)[2:end]  # D(expr, [x,1], y, ...) --> diff(expr,x,1,y,...)
        if is_Mxpr(dspec,:List)
            for xdspec in margs(dspec)
                push!(pyspec,mxpr2sympy(xdspec))
            end
        else
            push!(pyspec,mxpr2sympy(dspec))
        end
    end
#    println("$pymx, $pyspec")
    pyderivative = sympy.diff(pymx,pyspec...)
#    println("$pyderivative")
    return sympy2mxpr(pyderivative)
end

#### Together

@sjdoc Together "
Together(sum) rewrites a sum of terms as a product.
"
apprules(mx::Mxpr{:Together}) = mx[1] |> mxpr2sympy |> sympy.together |> sympy2mxpr


#### Apart

@sjdoc Apart "
Apart(product) computes a partial fraction decomposition of product
"
apprules(mx::Mxpr{:Apart}) = mx[1] |> mxpr2sympy |> sympy.apart |> sympy2mxpr

#### Simplify

@sjdoc Simplify "
Simplify(expr) rewrites expr in a simpler form.
"
apprules(mx::Mxpr{:Simplify}) = mx[1] |> mxpr2sympy |> sympy.simplify |> sympy2mxpr

@sjdoc TrigSimp "
TrigSimp(expr) does trigonometric simplification.
"
apprules(mx::Mxpr{:TrigSimp}) = mx[1] |> mxpr2sympy |> sympy.trigsimp |> sympy2mxpr

@sjdoc RatSimp "
Put an expression over a common denominator, cancel and reduce.
"
apprules(mx::Mxpr{:RatSimp}) = mx[1] |> mxpr2sympy |> sympy.ratsimp |> sympy2mxpr

@sjdoc RadSimp "
Rationalize the denominator.
"
apprules(mx::Mxpr{:RadSimp}) = mx[1] |> mxpr2sympy |> sympy.radsimp |> sympy2mxpr

apprules(mx::Mxpr{:PowSimp}) = mx[1] |> mxpr2sympy |> sympy.powsimp |> sympy2mxpr
apprules(mx::Mxpr{:LogCombine}) = mx[1] |> mxpr2sympy |> sympy.logcombine |> sympy2mxpr
apprules(mx::Mxpr{:Separate}) = mx[1] |> mxpr2sympy |> sympy.separate |> sympy2mxpr

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
    mx[1] |> mxpr2sympy |> megasimp |> sympy2mxpr
end

@sjdoc Cancel "
Cancel(expr) cancels common factors in the numerator and denominator.
"
#apprules(mx::Mxpr{:Cancel}) = sympy.cancel(mx[1] |> mxpr2sympy, extension=true) |> sympy2mxpr
apprules(mx::Mxpr{:Cancel}) = mx[1] |> mxpr2sympy |> sympy.cancel |> sympy2mxpr

@sjdoc Collect "
Collect(expr,x) collects terms involving the same power of x.
Collect(expr,[x,y]) collects terms involving first x, then y.
"
apprules(mx::Mxpr{:Collect}) = do_Collect(mx,margs(mx)...)

do_Collect(mx,expr,x) = sympy.collect(expr |> mxpr2sympy, x |> mxpr2sympy ) |> sympy2mxpr
do_Collect(mx,expr,x,lst::Mxpr{:List}) = sympy.collect(expr |> mxpr2sympy, x |> mxpr2sympy , list |> mxpr2sympy) |> sympy2mxpr
do_Collect(mx,args...) = mx

#### Solve

@sjdoc Solve "
Solves(expr) solves expr == 0 for one variable.
Solves(expr,var) solves expr == 0 for var.
"
apprules(mx::Mxpr{:Solve}) = do_Solve(mx,margs(mx)...)

do_Solve(mx, expr) = expr |> mxpr2sympy |> sympy.solve |> sympy2mxpr

function do_Solve(mx, expr, var::Symbol)
    pyexpr = expr |> mxpr2sympy
    pyvar = var |> mxpr2sympy
    sympy.solve(pyexpr,pyvar) |>  sympy2mxpr
end

function do_Solve(mx, eqs::Mxpr{:List}, vars::Mxpr{:List})
    peqs = eqs |> mxpr2sympy
    pyvars = vars |> mxpr2sympy
    sympy.solve(peqs,pyvars) |>  sympy2mxpr
end

# This is broken
apprules(mx::Mxpr{:DSolve}) = do_DSolve(mx,margs(mx)...)
do_DSolve(mx, expr) = expr |> mxpr2sympy |> sympy.dsolve |> sympy2mxpr


#### Roots
@sjdoc Roots "
Roots(expr) solves for the roots of expr. Roots returns a list
of lists. The two elements of each sublist give the root and its multiplicity.
"
# could use a macro for these
#apprules(mx::Mxpr{:Roots}) = do_Solve(mx,margs(mx)...)
apprules(mx::Mxpr{:Roots}) = mx[1] |> mxpr2sympy |> sympy.roots |> sympy2mxpr  |> SJulia.unpack_to_List

#### RealRoots
@sjdoc RealRoots "
RealRoots(expr) solves for the real roots of expr.
"
apprules(mx::Mxpr{:RealRoots}) = mx[1] |> mxpr2sympy |> sympy.real_roots |> sympy2mxpr


#### ToSymPy

@sjdoc ToSymPy "
ToSymPy(expr) converts expr to a (python) PyObject.
"

apprules(mx::Mxpr{:ToSymPy}) = mxpr2sympy(mx[1])

#### ToSJulia

@sjdoc ToSJulia "
ToSJulia(expr) converts the python PyObject expr to an SJulia expression.
"

@mkapprule1 ToSJulia

do_ToSJulia(mx::Mxpr,expr::PyCall.PyObject) = sympy2mxpr(expr)
do_ToSJulia(mx::Mxpr,expr) = expr
do_ToSJulia(mx::Mxpr,x...) = mxpr(:List,x)



## utility

# input -- Array of SJulia Lists and/or Symbols
# output -- Array of tuples (from Lists) of SymPy objects, or single SymPy objects
# Eg: For translating Integrate(expr,[x,a,b],y) --> integrate(expr,(x,a,b),y)
function varspecs_to_tuples_of_sympy(args::Array)
    oarr = []
    for x in args
        if is_Mxpr(x,:List)
            push!(oarr, tuple(map(mxpr2sympy, margs(x))...))
        else
            push!(oarr,mxpr2sympy(x))
        end
    end
    return oarr
end

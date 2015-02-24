using SJulia.JSymPy

#### Factor

@sjdoc Factor "
Factor(expr) factors expr. This function calls SymPy.
"
apprules(mx::Mxpr{:Factor})  = mx[1] |> mxpr2sympy |> sympy.factor |> sympy2mxpr

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

function apprules(mx::Mxpr{:Integrate})
    args = margs(mx)
    pymx = mxpr2sympy(args[1])
    varspecs = args[2:end]
    pyvarspecs = varspecs_to_tuples_of_sympy(varspecs)
#    println(pyvarspecs)
    pyintegral = sympy.integrate(pymx,pyvarspecs...)
#    println(pyintegral)
    #    return pyintegral
    #    return mxpr(:List,pyintegral,sympy2mxpr(pyintegral))
    return sympy2mxpr(pyintegral)
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
    pyderivative = sympy.diff(pymx,pyspec...)
    return sympy2mxpr(pyderivative)
end

#### Together

@sjdoc Together "
Together(sum) rewrites a sum of terms as a product.
"
apprules(mx::Mxpr{:Together}) = mx[1] |> mxpr2sympy |> sympy.together |> sympy2mxpr


#### Apart

@sjdoc Apart "
Together(product) rewrites a product as a sum of terms with mininmal denominators.
"
apprules(mx::Mxpr{:Apart}) = mx[1] |> mxpr2sympy |> sympy.apart |> sympy2mxpr

#### Simplify

@sjdoc Simplify "
Simplify(expr) rewrites expr in a simpler form.
"
apprules(mx::Mxpr{:Simplify}) = mx[1] |> mxpr2sympy |> sympy.simplify |> sympy2mxpr

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

#### ExpandA

@sjdoc ExpandA "
ExpandA(expr) expands powers and products in expr.
"
apprules(mx::Mxpr{:ExpandA}) = mx[1] |> mxpr2sympy |> sympy.expand |> sympy2mxpr

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

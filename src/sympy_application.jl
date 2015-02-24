using SJulia.SymPy

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

# TODO, multiple integrals
function apprules(mx::Mxpr{:Integrate})
    varspec = mx[2]
    pymx = mxpr2sympy(mx[1])    
    if is_Mxpr(varspec,:List)
        pyintegral = sympy.integrate(pymx,tuple(map(mxpr2sympy,margs(varspec))))
    else
        pyintegral = sympy.integrate(pymx,mxpr2sympy(varspec))
    end
    println(pyintegral)
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
    thederivative = sympy.diff(pymx,pyspec...)
    return sympy2mxpr(thederivative)
end

#### Together

@sjdoc Together "
Together(sum) rewrites a sum of terms as a product.
"

function apprules(mx::Mxpr{:Together})
    mx[1] |> mxpr2sympy |> sympy.together |> sympy2mxpr
end

#### Apart

@sjdoc Apart "
Together(product) rewrites a product as a sum of terms with mininmal denominators.
"

function apprules(mx::Mxpr{:Apart})
    mx[1] |> mxpr2sympy |> sympy.apart |> sympy2mxpr
end

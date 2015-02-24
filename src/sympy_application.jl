using SJulia.SymPy

@sjdoc Factor "
Factor(expr) factors expr. This function calls SymPy.
"


function apprules(mx::Mxpr{:Factor})
    expr = mx[1]
    pymx = mxpr2sympy(expr)
    pyfactored = SJulia.SymPy.sympy.factor(pymx)
    return sympy2mxpr(pyfactored)
end

@sjdoc Limit "
Limit(expr, var => lim) gives the limit of expr as var approaches to lim.
"

function apprules(mx::Mxpr{:Limit})
    expr = mx[1]
    limrule = mx[2]
    var = limrule[1]
    lim = limrule[2]
    pymx = mxpr2sympy(expr)
    thelimit = SJulia.SymPy.sympy.limit(pymx,mxpr2sympy(var),mxpr2sympy(lim))
    return sympy2mxpr(thelimit)
end


function apprules(mx::Mxpr{:Integrate})
    expr = mx[1]
    varspec = mx[2]
    pymx = mxpr2sympy(expr)    
    if is_Mxpr(varspec,:List)
        theintegral = SJulia.SymPy.sympy.integrate(pymx,(mxpr2sympy(varspec[1]),mxpr2sympy(varspec[2]),mxpr2sympy(varspec[3])))
    else
        theintegral = SJulia.SymPy.sympy.integrate(pymx,mxpr2sympy(varspec))
    end
    println(theintegral)
    #    return theintegral
    #    return mxpr(:List,theintegral,sympy2mxpr(theintegral))
    return sympy2mxpr(theintegral)
end

#import SJulia: Mxpr
#module SJulia

using SJulia.SymPy

@sjdoc Factor "
Factor(expr) factors expr. This function calls SymPy.
"

#import SJuliaSymPy: sympy2mxpr, mxpr2sympy

function apprules(mx::Mxpr{:Factor})
    expr = mx[1]
    pymx = mxpr2sympy(expr)
    pyfactored = SJulia.SymPy.sympy.factor(pymx)
    return sympy2mxpr(pyfactored)
end

#end

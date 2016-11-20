@sjdoc Function """
    Function(x,body)

returns a function of a single variable.

    Function([x,y,...],body)

returns a function of multiple variables

Examples:
```
symata> Function(x,x^2)(3)
9

symata> Function([x,y],x+y)(3,4)
7
```
"""
# Handle Functions of a single, named, variable. Eg.
# Function(x,x^2)(3) --> 9
function do_GenHead(mx, f0::Mxpr{:Function})
    length(f0) != 2 && symerror("Expected two arguments to `Function`")
    dogenhead(f0[1], f0[2], mx)
end

function dogenhead(var0,body, mx)
    (var,f) = localize_variable(var0,body)
    setsymval(var,mx[1])
    doeval(f)
end

function dogenhead(vars0::ListT, body, mx)
    (vars,f) = localize_variables(margs(vars0),body)
    for (var,arg) in zip(vars,margs(mx))
        setsymval(var,arg)
    end
    doeval(f)
end

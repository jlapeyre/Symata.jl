@sjdoc Function """
    Function(x,body)

returns a function of a single variable.

Example:
```
symata> Function(x,x^2)(3)
9
```
"""

# Handle Functions of a single, named, variable. Eg.
# Function(x,x^2)(3) --> 9
function do_GenHead(mx, f0::Mxpr{:Function})
    var0 = f0[1]
    (var,f) = localize_variable(var0,f0)
    setsymval(var,mx[1])
    doeval(f[2])
end

global plots_inited = false

function _init_plots()
    if ! plots_inited
        try
            @eval using Plots
            return true
        catch
            error("Unable to load 'Plots'. If the module is not installed, try `Pkg.add(\"Plots\")`")
            return false
        end
    end
    true
end

@mkapprule Plot  :nodefault => true

@sjdoc Plot """
    Plot(args..., color => red, xlabel => "x label")

plot using `args` and keywords. Args may be lists, numbers, strings, etc.

    Plot( :( sin ) , Table(Pi * RandomReal(), [100]), color => red)

plot using a Julia function.

    Plot( Compile( Exp(-x) * x^(1/2) ) , Range(0.0,5.0,.01) )

plot a compiled Symata expression. Plotting Symata expressions directly is not yet supported.

Other examples.

```
Plot([1,2,3])
Plot([1,2,3], [3,1,2])
```

Only a part of the functionality of `Plot.jl` is implemented.
"""

# TODO: use expression_to_julia_function as in NIntegrate
# Unfortunatley, we need a lot of logic to mimic dispatch on types that plot relies on.

# ugh. freesyms only works on Mxpr Fix this.
function wrap_expr(expr::Mxpr)
    symerror("Plotting expressions unsupported.")
    println("Checking ", expr)
    if isa(expr, Mxpr)
        syms = freesyms(expr)  
        println("Got syms ", syms)
        length(syms) != 1 && symerror("Expected only one free symbol. Got ", syms)
        expr = expression_to_julia_function(syms[1],expr)
        println(" Nowdsf ", expr)
    end
    expr
end

@doap function Plot(args...)
    (! _init_plots()) && return
    (nonkeywords, keyword_dict) = separate_rules(mx)
    listargs = Any[]
    for arg in nonkeywords
        if isa(arg, Mxpr{:List})
            a = margs(arg)
            na = [ x for x in a ]  # force Array of specific type. Eg. Any[cos, sin] fails.
            push!(listargs, na)
        elseif isa(arg,Mxpr)
            push!(listargs, wrap_expr(arg))
        else
            push!(listargs, arg)
        end
    end
    for (k,v) in keyword_dict
        if is_Mxpr(v)
            keyword_dict[k] = [ x for x in margs(v)]
        end
    end
    # println("list args ", listargs)
    # println("keyword args ", keyword_dict)
    plot(listargs... ; keyword_dict...)
end

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

@sjdoc Plot "
Plot(args..., color => red, xlabel => \"x label\")
plot using args and keywords. Args may be lists, numbers, strings, etc.

Plot( :( sin ) , Table(Pi * RandomReal(), [100]), color => red)

Plot using a `Julia` function.

Plot( Compile( Exp(-x) * x^(1/2) ) , Range(0.0,5.0,.01) )

Plot a compiled Symata expression. Plotting Symata expressions directly is not yet supported.

Other examples.

Plot([1,2,3])

Plot([1,2,3], [3,1,2])



Only a part of the functionality of `Plot.jl` is implemented.
"

@doap function Plot(args...)
    (! _init_plots()) && return
    (nonkeywords, keyword_dict) = separate_rules(mx)
    listargs = Any[]
    for arg in nonkeywords
        if is_Mxpr(arg)
            a = margs(arg)
            na = [ x for x in a ]  # force Array of specific type. Eg. Any[cos, sin] fails.
            push!(listargs, na)
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

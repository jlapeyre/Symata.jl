## give lexical scope to local variables in module

# This is not good enough. It will fail for nested calls to a Module.
# We can revert to generating new syms each time it is entered. But, better to
# store the replacements and positions so that the entire Module is
# not traversed each time and replace new symbols on each call.
# Or, push values onto a stack on entry and pop on exit.

# This strips the head 'Module' and creates the local vars and returns just the compound
# expression. Instead, 'Module' should be preserved and when pattern matching and evaluation
# is done, the local variables should be cleared.

# I think we might as well make this into a compound expression rather than LModule.
# LModule has one argument, a CompoundExpression

function localize_module!(mx::Mxpr{:Module})
    length(mx) != 2 && error("Module: Module called with ", length(mx), " arguments, 2 arguments are expected")
    (locvars,body) = (mx[1],mx[2])
    (is_Mxpr(locvars) && symname(mhead(locvars)) == :List) ||
    error("Module: Local variable specification $locvars is not a list")
    (is_Mxpr(body) && symname(mhead(body)) == :CompoundExpression) ||
     error("Module: Second argument must be a CompoundExpression")
    lvtab = Dict{Symbol,SJSym}()
    setlines = []
    for v in margs(locvars)
        if is_Mxpr(v,:Set)
            vn = symname(v[1])
            locs = get_localized_symbol(vn)
            lvtab[vn] = locs
            push!(setlines, mxpr(:Set,locs,v[2]))
        else
            vn = symname(v)
            lvtab[vn] = get_localized_symbol(vn)
        end
    end
    body = substlocalvars!(mx[2],lvtab)
    for setline in setlines
        unshift!(margs(body), setline)  # These lines will set the initial values
    end
    unshift!(margs(body), mxpr(:Clear,collect(values(lvtab))...)) # first thing is clear existing local vars. Not neccessary ?
    return mxpr(:LModule, body)  # we need this, for the moment, to remove the temporary variables
#    return body  # Return just the compound expression
end

substlocalvars!(el,lvtab) = is_Mxpr(el) ? mxpr(mhead(el), [substlocalvars!(x,lvtab) for x in margs(el)]...) :
     is_SJSym(el) && haskey(lvtab, symname(el)) ? lvtab[symname(el)] : el

## give lexical scope to local variables in module

# This strips the head 'Module'. Creates a gensym variable for each variable in the
# local variable list. Substitutes these local variables where they occur in the
# body. Pushes 'Set' expressions onto the body to set any initial values that may have been
# specified. Pushes 'Clear' expressions to clear local variables. (These should not be neccessary)
# At this point we would like to return the body with head CompundExpression., which will then
# be sent again through doeval. But, we want to remove the temporary (lexically scoped) variables
# from the symbol table. So we instead use the head LModule. This acts like CompoundExpression except
# it removes variables. (Note that, as in Mma, if local vars occur in the returned expression, they
# will be created again.) A cleaner way would be to use CompundExpression, and the Temporary attribute.
# We, like Mma, make new local variable names each time the module is evaluated. But, we
# could obviously store data to make the second call more efficient.

# Older comment.
# This is not good enough. It will fail for nested calls to a Module.
# We can revert to generating new syms each time it is entered. But, better to <--- we are doing this ?!
# store the replacements and positions so that the entire Module is
# not traversed each time and replace new symbols on each call.
# Or, push values onto a stack on entry and pop on exit.

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
    return mxpr(:LModule, body)  # we need this, for the moment, to remove the temporary variables. Better to mark them
#    return body  # Return just the compound expression
end

substlocalvars!(el,lvtab) = is_Mxpr(el) ? mxpr(mhead(el), [substlocalvars!(x,lvtab) for x in margs(el)]...) :
     is_SJSym(el) && haskey(lvtab, symname(el)) ? lvtab[symname(el)] : el

## give lexical scope to local variables in module

# This is not good enough. It will fail for nested calls to a Module.
# We can revert to generating new syms each time it is entered. But, better to
# store the replacements and positions so that the entire Module is
# not traversed each time and replace new symbols on each call.
# Or, push values onto a stack on entry and pop on exit.

# This strips the head 'Module' and creates the local vars and returns just the compound
# expression. Instead, 'Module' should be preserved and when pattern matching and evaluation
# is done, the local variables should be cleared.
function localize_module!(mx::Mxpr{:Module})
    length(mx) != 2 && error("Module: Module called with ", length(mx), " arguments, 2 arguments are expected")
    (locvars,body) = (mx[1],mx[2])
    (is_Mxpr(locvars) && symname(mhead(locvars)) == :List) ||
    error("Module: Local variable specification $locvars is not a list")
    (is_Mxpr(body) && symname(mhead(body)) == :CompoundExpression) ||
     error("Module: Second argument must be a CompoundExpression")
    lvtab = Dict{Symbol,SJSym}()
    for v in margs(locvars)
        vn = symname(v)
        lvtab[vn] = getsym(gensym(string(vn)))
    end
    body = substlocalvars!(mx[2],lvtab)
    unshift!(margs(body), mxpr(:Clear,collect(values(lvtab))...)) # remove existing local vars
    body                                                             # from the symbol table.
end    

#    push!(margs(body), mxpr(:ClearAll,collect(values(lvtab))...))
# Note, the ClearAll above is a stop gap to remove gensyms from the
# symbol table. There may be other exits from the Module
# than at the end.

# We currently have no mechanism for removing the last set of gensymed local variables from the symbol table.
# They also print without the chars added by gensym.

substlocalvars!(el,lvtab) = is_Mxpr(el) ? mxpr(mhead(el), [substlocalvars!(x,lvtab) for x in margs(el)]...) :
     is_SJSym(el) && haskey(lvtab, symname(el)) ? lvtab[symname(el)] : el

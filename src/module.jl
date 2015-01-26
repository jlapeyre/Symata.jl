## give lexical scope to local variables in module

function localize_module(mx::Mxpr{:Module})
    length(mx) != 2 && error("Module: Module called with ", length(mx), " arguments, 2 arguments are expected")
    (locvars,body) = (mx[1],mx[2])
    (is_Mxpr(locvars) && symname(head(locvars)) == :List) ||
    error("Module: Local variable specification $locvars is not a list")
    (is_Mxpr(body) && symname(head(body)) == :CompoundExpression) ||
     error("Module: Second argument must be a CompoundExpression")
    lvtab = Dict{Symbol,SJSym}()
    for v in margs(locvars)
        vn = symname(v)
        lvtab[vn] = getsym(gensym(string(vn)))
    end
    body = substlocalvars!(mx[2],lvtab)
    unshift!(margs(body), mxpr(:Clear,collect(values(lvtab))...)) # reset local vars on entry
    body
end    

substlocalvars!(el,lvtab) = is_Mxpr(el) ? mxpr(el.head, [substlocalvars!(x,lvtab) for x in el.args]...) :
     is_SJSym(el) && haskey(lvtab, symname(el)) ? lvtab[symname(el)] : el

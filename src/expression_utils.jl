# This is applied at toplevel after expression has been constructed.
# It is applied now to binomial expansion. Needs to be applied more
# generally.
function apply_upvalues_to_args!(mx::Mxpr)
    syms = listsyms(mx)
    goodsyms = Array(Symbol,0)
    for sym in syms
        if has_upvalues(sym)
            push!(goodsyms,sym)
        end
    end
    args = margs(mx)
    for sym in goodsyms
        mx = deep_apply_upvalues!(mx,sym)
    end
    return mx
end

function deep_apply_upvalues!(mx::Mxpr,sym)
    args = margs(mx)
    for i in 1:length(mx)
        args[i] = deep_apply_upvalues!(args[i],sym)
    end
    return applyupvalues(mx,sym)
end
deep_apply_upvalues!(x,sym) = x

# return true if h occurs anywhere in tree as a head
# Decided not to use this after it was written
function has_head(mx::Mxpr, h::SJSym)
    mhead(mx) == h && return true
    for i in 1:length(mx)
        has_head(mx[i],h) && return true
    end
    return false
end
has_head(x,h::SJSym) = false
#has_head(x) = error("has_head: unimplemented")

# These should go somewhere else.
# This is applied at toplevel after expression has been constructed.
# It is applied now to binomial expansion. Needs to be applied more
# generally.
function apply_upvalues_to_args!(mx::Mxpr)
    syms = listsyms(mx)
    goodsyms = Array(SJSym,0)
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

# We use this for copying expression trees.
# Important details are not addressed. free symbol list ?
function recursive_copy(mx::Mxpr)
    nargs = newargs(mx)
    args = margs(mx)
    for i in 1:length(nargs)
        nargs[i] = recursive_copy(args[i])
    end
    mxpr(mhead(mx),nargs)
end
recursive_copy(x) = copy(x)
recursive_copy(x::SJSym) = x
recursive_copy(x::DataType) = x
recursive_copy(x::Number) = x

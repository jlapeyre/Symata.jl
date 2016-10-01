capitalize_first_character(s::AbstractString) = uppercase(string(s[1])) * s[2:end]

# we use this for copying expression trees.
recursive_copy(x) = deepcopy(x)

# Count the number of first-level elements of mx that are of type Mxpr{head}
function mxpr_count_heads(mx::Mxpr, head)
    cnt = 0
    for i in 1:length(mx)
        if is_Mxpr(mx[i],head) cnt += 1  end
    end
    cnt
end


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

macro mdebug(level, a...)
    if level <= MXDEBUGLEVEL
        :((println($(a...));println()))
    else
        nothing
    end
end

using Base.Test
using SJulia

# For use in ../test/
macro testex(expr)
    mx = Expr(:macrocall, Symbol("@ex"), expr)
    result = eval(mx)
    retresult::Bool = true
    if typeof(result) <: Bool
        retresult = result
    else
        retresult = false
    end
    retresult || warn("Test failed: ", mx, " evaluated to ", retresult)
    Expr(:macrocall,Symbol("@test"),retresult)
end




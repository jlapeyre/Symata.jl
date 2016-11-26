"""
    tolistoflists(a)

convert a Julia object that can be indexed on two levels to a `List` of `Lists`.
`a` might be, for instance, `Array{Array{T, 1}, 1}` or nested `Tuple`s.
"""
function tolistoflists(a)
    nargs = newargs(a)
    for i in 1:length(a)
        nargs[i] = mxpr(:List,a[i]...)
    end
    mxpr(:List,nargs)
end

# not used yet
#totuple(mx::Mxpr{:List}) = tuple(margs(mx)...)

"""
    recursive_copy(x)

just calls `deepcopy`.
"""
recursive_copy(x) = deepcopy(x)

sjcopy{T<:Union{AbstractString,Symbol}}(s::T) = identity(s)
sjcopy(x) = copy(x)

"""
    mxpr_count_heads(mx::Mxpr, head)

Count the number of first-level elements of mx that are of type Mxpr{head}.
"""
function mxpr_count_heads(mx::Mxpr, head)
    cnt = 0
    for x in mx
        if isa(x,Mxpr{head}) cnt += 1  end
    end
    cnt
end


###

for sym in ( :List, :Plus, :Times )
    f = Symbol(:M,sym)
    s = QuoteNode(sym)
    @eval ($f)(a) = mxpr($s,a...)
    @eval ($f)(a...) = mxpr($s,a...)    
end

###

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


####

macro mdebug(level, a...)
    if level <= MXDEBUGLEVEL
        :((println($(a...));println()))
    else
        nothing
    end
end

using Base.Test
using Symata

# For use in ../test/
macro testex(expr)
    mx = Expr(:macrocall, Symbol("@sym"), expr)
    result = eval(mx)
    retresult::Bool = true
    if typeof(result) <: Bool
        retresult = result
    else
        retresult = false
    end
    retresult || symwarn("Test failed: ", mx, " evaluated to ", retresult)
    Expr(:macrocall,Symbol("@test"),retresult)
end

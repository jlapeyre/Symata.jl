## NOTE: It appears that collect always copies. convert does nothing if they type is already correct.
## Goal here is apply collect when it is needed. But not needlessly copy an array
"""
    tomargs(a::Vector)

converts `a` to type `MxprArgs`. `tomargs` is the identity
if `a` is of type `MxprArgs`.
"""
tomargs(a::Vector) = convert(MxprArgs,a)
tomargs(a::AbstractVector) = convert(MxprArgs,collect(MxprArgT,a)) ## convert should be redundant here
tomargs(a::MxprArgs) = a

## Following includes iterables.
## TODO: We could collect directly into type Any rather than copying
## TODO: test if as is iterable or s.t. There was a discussion about this
tomargs(a) = collect(MxprArgT,a)

"""
    tolist(a::AbstractArray)

returns a Syamta list with elements in `a`. If `a` is already
of type `MxprArgs`, a copy is *not* made. If `a` is of type `ListT`,
`tolist` is the identity.
"""
tolist(a::AbstractArray) = MListA(tomargs(a))
tolist(p::Pair) = MListA(MxprArgType[first(p), last(p)])
tolist(a::ListT) = a

### TODO: decide whether this copies or not.
"""
    tolistoflists(a)

convert a Julia object that can be indexed on two levels to a `List` of `Lists`.
`a` might be, for instance, `Array{Array{T, 1}, 1}` or nested `Tuple`s.
"""
function tolistoflists(ain)
    a = collect(ain)
    nargs = newargs(a)
    for (i,v) in enumerate(a)
       nargs[i] = tolist(v)
    end
    MListA(nargs)
end

tolistfixed(x) = setfixed(tolist(x))

## Both levels are fixed. We may want more fine grained function some day.
function tolistoflistsfixed(ain)
    a = collect(ain)
    nargs = newargs(a)
    for (i,v) in enumerate(a)
       nargs[i] = tolistfixed(v)
    end
    setfixed(MListA(nargs))
end

function tolistoflistsfixed(dict::Dict)
    setfixed(MListA(tomargsoflistfixed(dict)))
end

function tomargsoflistfixed(dict::Dict)
    args = newargs(length(dict))
    i = 0
    for (k,v) in dict
        i += 1
        args[i] = mxprcf(:List,k,v)
    end
    return args
end


"""
   maplist(f, ls::ListT)

maps `f` over `ls`, returing a `ListT`
"""
function maplist(f, ls::ListT)
    nargs = newargs(ls)
    for (i,x) in enumerate(ls)
        nargs[i] = f(x)
    end
    MListA(nargs)
end

"""
    mapmargs(f, inargs::AbstractArray)

map `f` over `inargs` returning an `Array{Any}`.
Arguments to `Mxpr`s are stored in this type. Julia v0.6
no longer creates a container of the same type as `inargs`,
but rather a container of the least common supertype. We don't
want that behavior. So we use this function.
"""
function mapmargs(f, inargs::AbstractArray)
    nargs = newargs(length(inargs))
    map!(f, nargs, inargs)
    return nargs
end

function mapmargs(f, inargs::Tuple)
    nargs = newargs()
    for x in inargs
        push!(nargs, f(x))
    end
    return nargs
end

# not used yet
#totuple(mx::Mxpr{:List}) = tuple(margs(mx)...)

"""
    recursive_copy(x)

just calls `deepcopy`.
"""
recursive_copy(x) = deepcopy(x)

sjcopy(s::T) where {T<:Union{AbstractString,Symbol}} = identity(s)
sjcopy(x) = copy(x)

"""
    mxpr_count_heads(mx::Mxpr, head)

Count the number of first-level elements of mx that are of type Mxpr{head}.
"""
mxpr_count_heads(mx::Mxpr, head) = count(x -> isa(x,Mxpr{head}), mx)

### MList

@doc """
    MList(a...)

is equivalent to mxpr(:List,a...)
"""

@doc """
    MListA(a...)

is equivalent to mxpra(:List,a...)
"""

## This is not really utility, should be in mxpr.jl
@doc """
    MPlus(a...)

is equivalent to mxpr(:Plus,a...)
"""

for sym in ( :List, :Plus, :Times, :Power, :Blank )
    f = Symbol(:M,sym)
    fa = Symbol(:M,sym,:A)
    s = QuoteNode(sym)
    @eval ($f)(a) = mxpr($s,a...)  # Is this really what we want ?
    @eval ($f)(a...) = mxpr($s,a...)
    @eval ($fa)(a) = mxpra($s,a)
end

###

# This is applied at toplevel after expression has been constructed.
# It is applied now to binomial expansion. Needs to be applied more
# generally.
function apply_upvalues_to_args!(mx::Mxpr)
    syms = listsyms(mx)
    goodsyms = Array{SJSym}(undef, 0)
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


#using Base.Test
#import Base.Test
using Symata

# For use in ../test/
macro testex(expr)
    mx = Expr(:macrocall, Symbol("@symfull"), expr)
    result = eval(mx)
    retresult::Bool = true
    if typeof(result) <: Bool
        retresult = result
    else
        retresult = false
    end
    retresult || @symwarn("Test failed: ", mx, " evaluated to ", retresult)
    Expr(:macrocall, Meta.parse("Test.@test"), retresult)
end

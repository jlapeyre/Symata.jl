## The code in this file should replace other similar code.
## For setting and getting with Part, in Table, ...

### Position

@sjdoc Position """
    Position(expr,x)

return a list of part specifications (indices) of positions in
`expr` at which `x` occurs. Only literal values for `x` are supported, not patterns.
"""

@mkapprule Position

function do_Position(mx::Mxpr{:Position},expr,subx)
    tolistoflists(find_positions(expr,subx))
end

"""
    find_positions(ex,subx)

return a list of part indices at which subexpression `subx` occurs in expression `ex`.
"""
function find_positions(ex,subx)
    posns = Array(Array{Int,1},0)
    lev = Array(Int,100)
    clev::Int = 1
    lev[clev] = 0
    _find_positions(ex,subx,lev,posns,clev)
    return posns
end

function _find_positions(ex::Mxpr,subx,lev,posns,clev)
    if clev > length(lev) push!(lev,0) end
    args = margs(ex)
    @inbounds for i in 1:length(args)
        lev[clev] = i
        _find_positions(args[i],subx,lev,posns,clev+1)
        end
    if mhead(ex) == subx
        lev[clev] = 0
        nlev = copy(lev)
        push!(posns,view(nlev,1:clev))
    end
    if ex == subx
        nlev = copy(lev)
        push!(posns,view(nlev,1:clev-1))
    end
end

function _find_positions(ex,subx,lev,posns,clev)
    if ex == subx
        nlev = copy(lev)
        push!(posns,view(nlev,1:clev-1))
    end
end


#### getpart

"""
     getpart(mx::Mxpr,ind1::Integer,ind2::Integer,...)

return part of expression `mx`. `ind1,...` indices into `mx`.
An index value `0` refers to the head of `mx` or a subexpression, that is the part returned by `mhead`.
Index values greater than `0` refer to elements.

Note that `0` only makes sense as the last index.

Negative indices are not supported here.
"""
getpart(mx::Mxpr,ind) = (ind == 0 ? mhead(mx) : margs(mx)[ind])
getpart(mx::Mxpr, ind1, ind2) = getpart(getpart(mx,ind1),ind2)
getpart(mx::Mxpr, ind1, ind2, inds...) = getpart(getpart(getpart(mx,ind1),ind2), inds...)

# This gives error with a single index. Why ?
Base.getindex(mx::Mxpr, inds...) = getpart(mx,inds...)

#### setpart

"""
    setpart!(mx::Mxpr,val,inds...)

set the part of `mx` indexed by `inds` to `val`.

The last index equal to zero means the head of the subexpression specified by the previous indices.
Because most heads are effectively immutable, we must replace the final subexpression in this case.
"""
function setpart!(mx::Mxpr,val,inds...)
    if length(inds) == 0
        mxpr(val,margs(mx))
    elseif length(inds) == 1
        margs(mx)[inds[1]] = val
    elseif inds[end] == 0
        ex = getpart(mx, inds[1:end-2]...)
        ex1 = margs(ex)[inds[end-1]]
        margs(ex)[inds[end-1]] = mxpr(val, margs(ex1))
    else
        ex = getpart(mx, inds[1:end-1]...)
        margs(ex)[inds[end]] = val
    end
end

Base.setindex!(mx::Mxpr, val, inds...) = setpart!(mx,val,inds...)

#### localize variable

"""
    maybe_localize_variable(var::Symbol,expr)

If `var` is unbound (that is, evaluates to itself), return `(var,expr)`.
If `var` is bound, return `(var1,expr1)`, where `var1` is a new symbol (from `gensym`)
and `expr1` is a copy of `expr` with `var` replaced everywhere by `var1`.
"""
function maybe_localize_variable(var,expr::Mxpr)
    isbound(var) || return (var,expr)
    localize_variable(var,expr)
end

"""
    localize_variable(var::Symbol,expr)

return `(var1,expr1)`, where `var1` is a new symbol (from `gensym`)
and `expr1` is a copy of `expr` with `var` replaced everywhere by `var1`.
"""
function localize_variable(var,expr::Mxpr)
    sym = get_localized_symbol(var)  # not really necessary that it be named like var
    positions = find_positions(expr,var)
    nexpr = deepcopy(expr)
    for pos in positions
        setpart!(nexpr, sym, pos...)
    end
    (sym, nexpr)
end

function localize_variable(var,expr::SJSym)
    sym = get_localized_symbol(var)
    var == expr && return (sym,sym)
    return (sym, expr)
end

### Take

@sjdoc Take """
    Take(expr,n)

returns `expr` keeping only the first `n` arguments.

    Take(expr,-n)

returns `expr` dropping all but the last `n` arguments.

    Take(expr,[n])

returns `expr` keeping only the `n`th arguments.

    Take(expr,[m,n])

returns `expr` keeping only the `m`th through `n`th arguments.

    Take(expr,[m,n,s])

returns `expr` keeping only the `m`th through `n`th arguments, with step `s`.

    Take(expr,spec1,spec2,...)

apply `spec1` at level `1`, `spec2` at level `2`...
"""
@mkapprule Take
@doap Take(x::Mxpr, rawspecs...) = take(x,map(sequencespec, rawspecs)...)
take(x, onespec, specs...) = mxpr(mhead(x),map(t -> take(t,specs...), margs(take(x,onespec)))...)
#take(x,spec::SequenceLastN) =  mxpr(mhead(x), margs(x)[(length(x)+spec.n+1):end]...)
take(x,spec::SequenceN)     = mxpr(mhead(x), margs(x)[spec.n>=0?(1:spec.n):(length(x)+spec.n+1:end)]...)
take(x,spec::SequenceUpToN) = mxpr(mhead(x), margs(x)[1:min(spec.n,length(x))]...)
take(x,spec::SequenceNOnly) = mxpr(mhead(x), margs(x)[posnegi(x,spec.n)])
take(x,spec::SequenceMN)    = mxpr(mhead(x), margs(x)[posnegi(x,spec.m):posnegi(x,spec.n)]...)
take(x,spec::SequenceMNS)   = mxpr(mhead(x), margs(x)[posnegi(x,spec.m):spec.s:posnegi(x,spec.n)]...)
take(x,spec::SequenceNone)  = mxpr(mhead(x))
take(x,spec::SequenceAll)   = mxpr(mhead(x), margs(x)...)

posnegi(x::Mxpr,n::Integer) = n > 0 ? n : length(x) + n + 1

### Drop


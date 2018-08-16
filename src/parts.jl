## TODO consolidate low-level part code. I thought newer code was better. Now, it seems the older code is useful

posnegi(x::Mxpr,n::Integer) = n >= 0 ? n : length(x) + n + 1
## WARNING: The following method is not the same as the others, it won't work in the general case (not for sequence specs)
## we may need to do something about this.
## In posnegi(obj,n) in general, obj is the object a part of which we want to get
## In posnegi below nmax is not this object
posnegi(nmax::Integer,n::Integer) = n >= 0 ? n : nmax + n + 1
posnegi(x,n::Integer) = n
posnegi(x,a) = a

# This *is* used. Eg 1[0] --> Integer
# FIXME: throw Symata error, not Julia
function get_part_one_ind(x, ind::Integer)
    ind != 0 && throw(BoundsError("part specification $ind is longer than the depth of the object."))
    return head(x)
end

function get_part_one_ind(texpr::V,ind::Integer) where V<:Union{Mxpr,Array}
    ind = posnegi(texpr,ind)
    texpr = ind == 0 ? mhead(texpr) : texpr[ind]
    return texpr
end
get_part_one_ind(texpr::Dict,tind) = texpr[tind]  # Part 0 can't return "Dict" because it could be a key.
get_part_one_ind(texpr::Tuple,tind) = texpr[tind]
function get_part_one_ind(texpr::Mxpr,tind::Mxpr{:Span})
    spanargs = margs(tind)
    lsp = length(spanargs)
    if lsp == 2
        nargs = view(margs(texpr),spanargs[1]:spanargs[2]) # need function to do this translation
    elseif lsp == 3
        nargs = view(margs(texpr),spanargs[1]:spanargs[3]:spanargs[2])
    end
    texpr = mxpr(mhead(texpr),nargs...) # we need splice to copy Int Array to Any Array
    return texpr
end

### Part

# Get part of expression. Julia :ref is mapped to :Part
# a[i] parses to Part(a,i), and a[i][j] to Part(Part(a,i),j)
# a[i,j] parses to Part(a,i,j).

@mkapprule Part  """
    Part(expr,n) or expr[n]

returns the `n`th element of expression `expr`.

    Part(expr,n1,n2,...) or expr[n1,n2,...]

return the subexpression at index `[n1,n2,...]`.

The same can be achieved less efficiently with `expr[n1][n2]...`.

    expr[n] = val

set the `n`th part of `expr` to `val`. `n` and `val` are evaluated
normally. `expr` is evaluated once.

`n` less than zero counts backward from the end of the expression or subexpression.

`expr[n]` also returns the `n`th element of instances of several
Julia types such as `Array`, or the element with key `n` for `Dict`'s.

`Part` also indexes into some sympy objects, such as sympy Tuples.
"""

@doap Part(texpr,tinds...) = getpart2(texpr,tinds...)

function getpart2(texpr,tinds...)
    for tind in tinds
        texpr = get_part_one_ind(texpr,tind)
    end
    return texpr
end

### Span

@sjdoc Span """
    Span(a,b) or a:b

represents elements `a` through `b`.

    Span(a,b,c) or a:b:c

represents elements `a` through `b` in steps of `c`.

`expr[a:b]` returns elements a through `b` of expr, with the same head as `expr`.
"""

### Position

@mkapprule Position """
    Position(expr,x)

return a list of part specifications (indices) of positions in
`expr` at which `x` occurs.
"""

@doap Position(expr,subx) = tolistoflists(find_positions(expr,subx))
@curry_second Position

"""
    find_positions(ex,subx)

return a list of part indices at which subexpression `subx` occurs in expression `ex`.
"""
function find_positions(ex,subx)
    posns = Array{Array{Int,1}}(undef, 0)
    lev = Array{Int}(undef, 100)
    clev::Int = 1
    lev[clev] = 0
    capt = capturealloc()
    psubx = patterntoBlank(subx)
    _find_positions(ex,psubx,lev,posns,clev,capt)
    return posns
end

# TODO: split out literal pattern code: This routine will be the main routine for
# finding positions, inlcuding when we know that the pattern is literal.
# The literal pattern code is commented out below
function _find_positions(ex::Mxpr,psubx,lev,posns,clev,capt)
    if clev > length(lev) push!(lev,0) end
    args = margs(ex)
    @inbounds for i in 1:length(args)
        lev[clev] = i
        _find_positions(args[i],psubx,lev,posns,clev+1,capt)
    end
    (gotmatch, capt) = match_and_capt(mhead(ex),psubx,capt)
    if gotmatch
#    if mhead(ex) == subx      # Use this for literal pattern
        lev[clev] = 0
        nlev = copy(lev)
        push!(posns,view(nlev,1:clev))
    end
    (gotmatch, capt) = match_and_capt(ex,psubx,capt)
    if gotmatch
#    if ex == subx
        nlev = copy(lev)
        push!(posns,view(nlev,1:clev-1))
    end
end

function _find_positions(ex,psubx,lev,posns,clev,capt)
    (gotmatch, capt) = match_and_capt(ex,psubx,capt)
    if gotmatch
#    if ex == subx
        nlev = copy(lev)
        push!(posns,view(nlev,1:clev-1))
    end
end

#### setpart

## setpart2! does the same, but I think it  is better and simpler and doesnt seem to be broken

## getpart is used only here. it is defined in mxpr.jl. It is probably not fast
## maybe getpart2 above is faster and handles more cases.
"""
    setpart!(mx::Mxpr,val,inds...)

set the part of `mx` indexed by `inds` to `val`.

The last index equal to zero means the head of the subexpression specified by the previous indices.
Because most heads are effectively immutable, we must replace the final subexpression in this case.
"""
function setpart!(mx::Mxpr,val,inds...)
    if length(inds) == 0
#        return mxpr(val,margs(mx))
        return mxprnewhead(mx, val)
    elseif length(inds) == 1
        if inds[1] == 0  # replacing head
#            mx = mxpr(val,margs(mx)...)
            mx = mxprnewhead(mx,val)
            # Function application is very slow if this branch is taken.
            return mx
        else
            margs(mx)[inds[1]] = val
        end
    elseif inds[end] == 0
        ninds = inds[1:end-2]
        if length(ninds) == 0
            symerror("Not implemented getting no part of $mx")
        end
        ex = getpart(mx, ninds...)
        ex1 = margs(ex)[inds[end-1]]
#        margs(ex)[inds[end-1]] = mxpr(val, margs(ex1))
        margs(ex)[inds[end-1]] = mxprnewhead(ex1,val)
    else
        ex = getpart(mx, inds[1:end-1]...)
        margs(ex)[inds[end]] = val
    end
    mx
end

## This is older, but seems better.
## This checks for negative indices. If calling setpart!, this checking must be done before calling
## setpart! is intended for dev code where negative indices won't appear. But, it is probably not
## necessary to have setpart!
##
## A complication arises because the head (Head) of an Mxpr cannot be changed because the head is
## also in the type Mxpr{sym}. If the head needs to be changed, a copy must be made. The exception
## is if the head is not a symbol. In this case, the type is Mxpr{GenHead}. The head is always stored
## in a field as well. It is possible, but undesirable for the type and field disagree.
## This is why the previous level expression is stored in setpart2!
function setpart2!(mx::Mxpr, val, inds...)
    ex = mx
    exlast = ex
    ninds = length(inds)
    for j in 1:ninds-1
        exlast = ex
        ex = get_part_one_ind(ex,inds[j])
    end
    ind = posnegi(ex,inds[ninds])
    if ind == 0
        exlast[inds[ninds-1]] = mxprnewhead(ex,val)
    else
        ex[ind] = val
    end
    val
end

## either setpart! or setpart2! works here.
Base.setindex!(mx::Mxpr, val, inds...) = setpart2!(mx,val,inds...)

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
    sym = get_localized_symbol(var)  # not really necessary that named contains var
    positions = find_positions(expr,var)
    nexpr = deepcopy(expr)
    for pos in positions
        nexpr = setpart!(nexpr, sym, pos...)
    end
    (sym, nexpr)
end

function localize_variable(var,expr::SJSym)
    sym = get_localized_symbol(var)
    var == expr && return (sym,sym)
    return (sym, expr)
end

localize_variable(var,expr) = (get_localized_symbol(var), expr)

function localize_variables(vars::Array,expr::Mxpr)
    syms = map(get_localized_symbol, vars)
    nexpr = deepcopy(expr)
    for (sym,var) in zip(syms,vars)
        positions = find_positions(expr,var)
        for pos in positions
            setpart!(nexpr, sym, pos...)
        end
    end
    (syms, nexpr)
end

function localize_variables(vars::Array,expr::SJSym)
    syms = map(get_localized_symbol, vars)
    for (sym,var) in zip(syms,vars)
        var == expr && return (syms,sym)
    end
    return (syms, expr)
end

localize_variables(vars::Array,expr) = (map(get_localized_symbol, vars), expr)

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
take(x, onespec, specs...) = mxpr(mhead(x),mapmargs(t -> take(t,specs...), margs(take(x,onespec)))...)
take(x,spec::SequenceN)     = mxpr(mhead(x), margs(x)[spec.n>=0 ? (1:spec.n) : (length(x)+spec.n+1:end)]...)
take(x,spec::SequenceUpToN) = mxpr(mhead(x), margs(x)[1:min(spec.n,length(x))]...)
take(x,spec::SequenceNOnly) = mxpr(mhead(x), margs(x)[posnegi(x,spec.n)])
take(x,spec::SequenceMN)    = mxpr(mhead(x), margs(x)[posnegi(x,spec.m):posnegi(x,spec.n)]...)
take(x,spec::SequenceMNS)   = mxpr(mhead(x), margs(x)[posnegi(x,spec.m):spec.s:posnegi(x,spec.n)]...)
take(x,spec::SequenceNone)  = mxpr(mhead(x))
take(x,spec::SequenceAll)   = mxpr(mhead(x), margs(x)...)

### Drop

# With a little work, we could raise an error if the user tries to drop more elements
# than exists. In fact, the behavior is as if we always use UpTo.
@sjdoc Drop """
    Drop(expr,n)
    Drop(expr,-n)
    Drop(expr,[m,n])

!!! note
    `Drop(expr,UpTo(n))` is not necessary because `Drop(expr,n)` already gives the
    desired behavior.
"""

@mkapprule Drop
@doap Drop(x::Mxpr, rawspecs...) = drop(x,map(sequencespec, rawspecs)...)
drop(x, onespec, specs...) = mxpr(mhead(x),map(t -> drop(t,specs...), margs(drop(x,onespec)))...)
drop(x,spec::SequenceN) = mxpr(mhead(x), margs(x)[spec.n>0 ? (((spec.n)+1):end) : (1:length(x)+spec.n)]...)
drop(x,spec::SequenceUpToN) = drop(x,SequenceN(spec.n))

function drop(x,spec::SequenceNOnly)
    n = posnegi(x,spec.n)
    ma = margs(x)
    mxpr(mhead(x), ma[1:n-1]..., ma[n+1:end]...)
end

function drop(x,spec::SequenceMN)
    m = posnegi(x,spec.m)
    n = posnegi(x,spec.n)
    ma = margs(x)
    mxpr(mhead(x), ma[1:m-1]..., ma[n+1:end]...)
end


### Extract

@sjdoc Extract """
    Extract(expr, list)

returns the subexpression specified by `list`.


    Extract(expr, [list1, list2, ...])

returns a list of subexpressions.

    Extract(expr, [list1, list2, ...], h)

wraps each subexpression with head `h`.

    Extract(list)

operator form of `Extract`.

`list` is a part specification as returned by `Part`.
"""
@mkapprule Extract

@doap function Extract(expr, p::ListT)
    isa(p[1],Integer) && return expr[margs(p)...]
    ! isa(p[1],ListT) && symerror("Extract: Bad part specification ", p)
    nargs = newargs(length(p))
    for i in 1:length(p)
        nargs[i] = expr[margs(p[i])...]
    end
    return MList(nargs)
end

@doap function Extract(expr, p::ListT, h)
    isa(p[1],Integer) && return mxpr(h,expr[margs(p)...])
    ! isa(p[1],ListT) && symerror("Extract: Bad part specification ", p)
    nargs = newargs(length(p))
    for i in 1:length(p)
        nargs[i] = mxpr(h,expr[margs(p[i])...])
    end
    return MList(nargs)
end

@curry_last Extract

### Replace

@sjdoc Replace """
    Replace(expr,rule)

replace parts in expr according to `rule`.

    Replace(expr,rule,n)

replace at levels less than or equal to `n`.

    Replace(expr,rule,[n])

replace at level `n` only.
"""

@sjseealso_group(Replace,ReplaceAll)
@sjexamp(Replace,
         ("Clear(a,b,c)",""),
         ("Replace( Cos(a+b)^2 + Sin(a+c)^2, Cos(x_)^2 + Sin(x_)^2 => 1)",
          "Cos(a + b)^2 + Sin(a + c)^2", "This expression does not match the pattern."),
         ("Replace( Cos(a+b)^2 + Sin(a+b)^2, Cos(x_)^2 + Sin(x_)^2 => 1)",
          "1", "This expression does match the pattern."))

@mkapprule Replace

# function apprules(mx::Mxpr{:Replace})
#     doreplace(mx,margs(mx)...)
# end

# function doreplace(mx,expr,r::Rules)
#     (success, result) = replace(expr,r)
#     result
# end

@doap function Replace(expr,r::Rules)
    (success, result) = replace(expr,r)
    result
end

@doap function Replace(expr,r::Rules,inlevelspec)
    levelspec = make_level_specification(expr,inlevelspec)
    (success, result) = replace(levelspec,expr,r)
    result
end

@curry_second Replace

#### ReplaceAll

@sjdoc ReplaceAll """
    ReplaceAll(expr,rule)

replace parts at all levels in expr according to `rule`. This includes
the `Head` of `expr`, and the entire `expr`.

    ReplaceAll(expr,List(rule1,rule2,...))

replace parts at all levels according to the
list of rules. If given explicitly, the rules should be given as `List(...)` rather than
`[...]` because of a parsing error.

    op = ReplaceAll(rule)

Define function `op(expr)` that returns `ReplaceAll(expr,rule)`.
"""

@mkapprule ReplaceAll

@doap ReplaceAll(expr,r::Rules) =replaceall(expr,r)

@doap function ReplaceAll(expr,rs::Mxpr{:List})
    if listoflistsq(rs)
        maplist( r ->  _doreplaceall(mx,expr,r), rs)
    else
        _doreplaceall(mx,expr,rs::Mxpr{:List})
    end
end

function _doreplaceall(mx,expr,rs::Mxpr{:List})
    rsa = Array{Any}(undef, 0)
    for i in 1:length(rs)
        if isa(rs[i],Rules)
            push!(rsa, rs[i])
        else
            @symwarn("ReplaceAll expected Rule  got ", rs[i])
            nothing  # do something better here, like return mx
        end
    end
    replaceall(expr,rsa)
end

@sjexamp( ReplaceAll,
         ("ClearAll(zz,b,c)",""),
         ("zz = 10 * b^2 * (c+d)","zz = 10 * b^2 * (c+d)"),
         ("ReplaceAll(zz, List(c => 3,d => 2) )", "50*b^2"))

@curry_second ReplaceAll

### ReplaceRepeated

@mkapprule ReplaceRepeated :options => Dict( :MaxIterations => 65536 )

@sjdoc ReplaceRepeated """
    ReplaceRepeated(expr,rules)

perform `ReplaceAll(expr,rules)` repeatedly until `expr` no longer changes.
"""

@doap ReplaceRepeated(expr,r::T; kws...) where {T<:Rules} = replacerepeated(expr,r; kws...)

## FIXME: use get to get keywords
#function do_ReplaceRepeated(mx::Mxpr{:ReplaceRepeated},expr,rs::Mxpr{:List}; kws...)
@doap function ReplaceRepeated(expr,rs::Mxpr{:List}; kws...)
    rsa = Array{Any}(undef, 0)
    for r in rs
        if isa(r,Rules)
            push!(rsa, r)
        else
            @symwarn("ReplaceRepeated expected Rule, got ", r)
            nothing  # do something better here, like return mx
        end
    end
    replacerepeated(expr,rsa; kws...)
end

@doap ReplaceRepeated(a,b; kws...) = mx

### MatchQ

@sjdoc MatchQ """
    MatchQ(expr,pattern)

return `True` if `expr` matches `pattern`.

    op = MatchQ(pattern)

Define a function `op(expr)` that returns `MatchQ(expr,pattern)`.
For example, `myintq = MatchQ(_Integer)`.
"""
@mkapprule MatchQ
@doap MatchQ(expr,pat) = matchq(expr,pat)
matchq(expr,pat) = match_no_capture(expr,patterntoBlank(pat))
@curry_last MatchQ

@sjexamp( MatchQ,
         ("MatchQ( 1, _Integer)", "true"),
         ("ClearAll(gg,xx,b)",""),
         ("MatchQ( gg(xx) , _gg)", "true"),
         ("MatchQ( b^2, _^2)","true"))

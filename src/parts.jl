## The code in this file should replace other similar code.
## For setting and getting with Part, in Table, ...

#### Position

@sjdoc Position """
    Position(expr,x)

return a list of part specifications (indices) of positions in
`expr` at which `x` occurs. Only literal values for `x` are supported, not patterns.
"""

@mkapprule Position

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
function getpart(mx::Mxpr,ind) ind == 0 ? mhead(mx) : mx[ind] end
getpart(mx::Mxpr, ind1, ind2) = getpart(getpart(mx,ind1),ind2)
getpart(mx::Mxpr, ind1, ind2, inds...) = getpart(getpart(getpart(mx,ind1),ind2), inds...)

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

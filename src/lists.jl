using DataStructures

### List

# TODO. Find where rules for List are put this doc there.
@sjdoc List """
    [e1, e2, ...],  List(e1, e2, ...)

creates a list. Lists are printed delimited by square brackets. The head
of the internal form is `List`.
"""

### Args

@sjdoc Args """
    Args(ex)

replace the `Head` of expression `ex` with `List`.

`Args(ex)` is equivalent to `Apply(List,ex)`.
"""
@mkapprule Args :nargs => 1
@doap Args(ex::Mxpr) = MList(copy(margs(ex)))

### First

@mkapprule First :nargs => 1
@doap First(x::Mxpr) = isempty(x) ? mx : x[1]
@doap First(x) = mx

### Join

@mkapprule Join

@sjdoc Join """
    Join(expr1, expr2, ...)

concatenate arguments of expressions with the same `Head`, returning an expression with the same `Head`.
"""

@doap function Join(args::Mxpr{T}...) where T
    nargs = newargs()
    foreach( x -> append!(nargs,margs(x)), args)
    mxpra(mhead(args[1]),nargs)
end

### Rest

@mkapprule Rest :nargs => 1

@doap function Rest(x::Mxpr)
    isempty(x) && return mx
    nargs = newargs(length(x)-1)
    for i in 1:length(nargs)
        nargs[i] = deepcopy(x[i+1])
    end
    mxpra(mhead(x),nargs)
end

@doap Rest(x) = mx

### Most

@mkapprule Most :nargs => 1

@doap function Most(x::Mxpr)
    isempty(x) && return mx
    nargs = newargs(length(x)-1)
    for i in 1:length(nargs)
        nargs[i] = deepcopy(x[i])
    end
    mxpra(mhead(x),nargs)
end

@doap Most(x) = mx

### Last

@mkapprule Last :nargs => 1:2

@doap function Last(x::Mxpr)
    isempty(x) && return mx  # FIXME add warning
    x[end]
end

@doap function Last(x::Mxpr,default)
    isempty(x) && return doeval(default)  # Last has Attribute HoldRest
    x[end]
end


### Fold

@mkapprule Fold :nargs => 2:3

@sjdoc Fold """
    Fold(f,x,[a,b,c,...])

return  `f(f(f(x,a),b),c)...`

    Fold(f,lst)

return  `f(First(lst),Rest(lst))`.

`f` may be a `Symbol`, or a function, or Julia function. Pure functions are not yet implemented.
"""

@mkapprule FoldList :nargs => 2:3

for head in (:Fold, :FoldList)
    fl = (head == :FoldList)
@eval begin
    @doap function ($head)(f, x, lst::Mxpr)
                local res
                n = length(lst)
                n == 0 && return x
                $(fl ? :(nargs = newargs(n+1)) : nothing)
                $(fl ? :(nargs[1] = x) : nothing)
                if isa(f,Function)
                    res = doeval(Base.invokelatest(f,x,lst[1]))
                    $(fl ? :(nargs[2] = res) : nothing)
                    for i in 2:n
                        res = invokelatest(f,res,lst[i])
                        $(fl ? :(nargs[i+1] = res) : nothing)
                    end
                else
                    res = doeval(mxpr(f,x,lst[1]))
                    $(fl ? :(nargs[2] = res) : nothing)
                    for i in 2:n
                        res = doeval(mxpr(f,res,lst[i]))
                        $(fl ? :(nargs[i+1] = res) : nothing)
                    end
                end
                $(fl ? :(mxpra(mhead(lst), nargs)) : :( res ))
         end

   @doap function ($head)(f,lst::Mxpr)
           isempty(lst) && return mx
           mxpr($(QuoteNode(head)), f, lst[1], mxpr(mhead(lst),lst[2:end]))
         end
end
end

@mkapprule Nest :nargs => 3
@mkapprule NestList :nargs => 3

## FIXME: Using invokelatest makes a typical call 10x slower.
## But, it seems we have to do this in v0.6

for head in (:Nest, :NestList)
    nl = (head == :NestList)
@eval begin
    @doap function ($head)(f, x, n::Integer)
                local res
                n == 0 && return $(nl ? :(mxpr(:List,x)) : :(x))
                $(nl ? :(nargs = newargs(n+1)) : nothing)
                $(nl ? :(nargs[1] = x) : nothing)
                if isa(f,Function)
                    res = doeval(Base.invokelatest(f,x))
                    if is_throw() return res end
                    $(nl ? :(nargs[2] = res) : nothing)
                    for i in 2:n
                        res = invokelatest(f,res)
                        if is_throw() return res end
                        $(nl ? :(nargs[i+1] = res) : nothing)
                    end
                else
                    res = doeval(mxpr(f,x))
                    if is_throw() return res end
                    $(nl ? :(nargs[2] = res) : nothing)
                    for i in 2:n
                        res = doeval(mxpr(f,res))
                        if is_throw()
                            return res
                        end
                        $(nl ? :(nargs[i+1] = res) : nothing)
                    end
                end
                $(nl ? :(mxpra(:List, nargs)) : :( res ))
         end
end
end

### Range

@sjdoc Range """
    Range(n)

return the `List` of integers from `1` through `n`.

    Range(n1,n2)

return the `List` of numbers from `n1` through `n2`.

    Range(n1,n2,di)

return the `List` of numbers from `n1` through `n2` in steps of `di`.
`di` may be negative. Floats and some symbolic arguments are supported.

Similar `Lists` can be created by importing Julia `Array`s with `Unpack(:([1.0:10^5]))`.
This uses embedded Julia to create a typed `Array` and then unpacks it to a List.
"""

# Need to check for uprules for free symbols
function apprules(mx::Mxpr{:Range})
    iter = make_sjitera(margs(mx))
    args = do_Range(iter)
    r = mxpra(:List,args)
    setfixed(r)
    setcanon(r)
    mergesyms(r,:nothing)  # not correct if we have symbols.
    return r
end

function do_Range(iter::SJIterA1)  # iter is parameterized, so we hope type of n is inferred.
    n = iter.num_iters
    args = newargs(n);
    _do_Range_fill(args,n,typeof(iter.imax))
    return args
end

function _do_Range_fill(args, n, ::Type{Int})
    copy!(args,1:n)
  # @inbounds @simd for i in 1:n
  #                    args[i] = i
  #                 end
    return args
end

function _do_Range_fill(args, n, ::Type{T}) where T<:Real
#    j = one(T)
    n1 = one(T)
    n2 = convert(T,n)
    copy!(args,n1:n2)
    # @inbounds for i in 1:n
    #    args[i] = j
    #    j += 1
    # end
    return args
end


# Fails for rationals. and counting is wrong
function do_Range(iter::SJIterA2{T,V}) where {T<:Real,V<:Real}
    nd = round(Int,mplus(iter.imax,-iter.imin) + 1)  # Try bug fix!
    if nd > 1
        args = newargs(iter.num_iters)
        @inbounds for i in 0:nd-1
            args[i+1] = mplus(i,iter.imin)  # Bug here, if iter.imin is not of type Int.
        end
    else  # Mma does not allow this second branch: eg Range(5,1) implies di = -1
        nd = -nd + 2
        args = newargs(iter.num_iters)
        @inbounds for i in zero(iter.imin):(nd - 1)
            args[i+1] = mplus(iter.imin, -i)
        end
    end
    return args
end

# Symbolic values
# FIXME. We don't record free symbols and check for upvalues.
# This is about as fast as Mma 3 (running on a somewhat slower cpu)
function do_Range(iter::SJIterA2)
    args = newargs(iter.num_iters)
    imin = iter.imin
    args[1] = imin
    s = imin
    if is_Mxpr(imin,:Plus)  # imin is a sum
        if is_Number(imin[1])  # number is always first in canon order.
            b = imin[1]  # extract number
            r = imin[2:end]  # the rest of the sum
            _do_Range_A2(args,b,r,iter.num_iters)
        else  # imin is a sum with no numbers, so we put a number in front
            sargs = margs(s)
            for i in 2:iter.num_iters
                args[i] = mxpr(:Plus,i-1,sargs...)
                setfixed(args[i])
            end
        end
    else  # imin is not a sum
        @inbounds for i in 2:iter.num_iters
            args[i] = mxpr(:Plus,i-1,s)
            setfixed(args[i])
        end
    end
    #  we don't handle counting down case.
    return args
end

function _do_Range_A2(args,b,r,n)
    @inbounds for i in 2:n
        args[i] = mxpr(:Plus,b+i-1,r...) # only a little slower than Mma if disable gc
        setfixed(args[i])
    end
    return args
end

# seems to be little penalty for mplus instead of +
function do_Range(iter::SJIterA3{T,V,W}) where {T<:Real,V<:Real,W<:Real}
    n = iter.num_iters
    args = newargs(n)
    j = iter.imin
    @inbounds for i in 1:n
        args[i] = j
        j = mplus(j,iter.di)
    end
    return args
end

# Symbolic again
function do_Range(iter::SJIterA3)
    args = newargs(iter.num_iters)
    imin = iter.imin
    args[1] = imin
    s = imin
    if is_Number(iter.di)
        if true
            if is_Mxpr(imin,:Plus)
                if is_Number(imin[1])  # number is always first in canon order.
                    b = imin[1]        # extract number
                    r = imin[2:end]    # the rest of the sum
                    for i in 2:iter.num_iters
                        b = b + iter.di
                        if b == 0  # more efficient to move this branch out
                            if length(r) == 1
                                args[i] = r[1]
                            else
                                args[i] = mxpr(:Plus,r...)
                            end
                        else
                            args[i] = mxpr(:Plus,b,r...)
                        end
                        setfixed(args[i])
                    end
                else  # imin is a sum with no numbers, so we put a number in front
                    sargs = margs(s)
                    j = zero(iter.di)
                    for i in 2:iter.num_iters
                        j += iter.di
                        args[i] = mxpr(:Plus,j,sargs...)
                        setfixed(args[i])
                    end
                end
            else # imin is not a sum, so just create one
                j = zero(iter.di)
                @inbounds for i in 2:iter.num_iters
                    j += iter.di
                    args[i] = mxpr(:Plus,j,s)
                    setfixed(args[i])
                end
            end
        else  #  iter.di < 0
            error("unimplemented")
        end
    else # di is not a number
        error("unimplemented")
    end
    return args
end

# Some is implemented here that is not in the new Range yet
function apprules(mx::Mxpr{:OldRange})
    if length(mx) == 1
        n = mx[1]
        args = range_args1(n) # use function for optimization on type
    elseif length(mx) == 2
        n0 = mx[1] - 1
        n = mx[2]
        args = range_args2(n0,n)
    elseif length(mx) == 3
        n0 = mx[1]
        n = mx[2]
        di = mx[3]
        off = n > n0 ? 1 : -1
        args = range_args3(n0,n,di,off)
    else
        return mx
    end
    r = mxpra(:List,args)
    setfixed(r)
    setcanon(r)
    mergesyms(r,:nothing)
    return r
end

# separate functions are *essential* for type stability and efficiency.
function range_args1(n::T) where T<:Integer
    args = newargs(n);
    @inbounds for i in one(n):n
        args[i] = i
    end
    return args
end

function range_args1(n::T) where T<:AbstractFloat
    ni = floor(Int,n)
    args = newargs(ni);
    @inbounds for i in 1:ni
        args[i] = convert(T,i)
    end
    return args
end

function range_args2(n0,n)
    nd = n - n0
    args = newargs(nd);
    @inbounds for i in one(n0):nd
        args[i] = i+n0
    end
    return args
end

function range_args3(n0,n,di,off)
    args = newargs(div(n-n0+off,di));
    len = length(args) # cheap
    s = n0
    @inbounds for i in one(n0):len
        args[i] = s
        s += di
    end
    args
end

### ConstantArray

@sjdoc ConstantArray """
    ConstantArray(expr,n)

create a `List` of `n` copies of `expr`.
"""

# We take only attribute to be Protected. So expr is evaled already

@mkapprule ConstantArray

# function apprules(mx::Mxpr{:ConstantArray})
#     do_ConstantArray(mx,margs(mx)...)
# end
#do_ConstantArray(mx,args...) = mx

# The annotation for Number is needed, because deepcopy tries
# to do something very slow with numbers.
# Copying a small Mxpr is extremely slow
# 'c^2' is 200 times slower than Symbol 'c'.

@doap function ConstantArray(expr,n::Integer)
    _constantarray(expr,n)
end

function _constantarray(expr,n::Integer)
    nargs = newargs(n)
    @inbounds for i in 1:n
        nargs[i] = recursive_copy(expr)
    end
    mxpra(:List,nargs)
end

function _constantarray(expr::Mxpr,n::Integer)
    nargs = newargs(n)
     @inbounds for i in 1:n
        nargs[i] = setfixed(recursive_copy(expr))         
     end
    mxpra(:List,nargs)
end

@doap function ConstantArray(expr,ns::Mxpr{:List})
    all(x->isa(x,Integer), margs(ns)) || return mx
    _constantarray(expr,reverse(margs(ns)))
end

function _constantarray(expr,ns::Array)
    isempty(ns) && return Null
    a = _constantarray(expr,ns[1])
    length(ns) == 1 && return a
    _constantarray(a,ns[2:end])
end

function _constantarray(expr::T,n::Integer) where T<:Union{Number,SJSym,String}
    nargs = newargs(n)
    fill!(nargs,expr)
    a = mxpra(:List,nargs)
    expr == :Nothing && return a
    setfixed(a)
end


### Nothing

# Nothing is removed where it appears in a List, but not from expressions with other Heads.
# This is too expensive to implement as a rule. Better to write special code in evaluation.jl

@mkapprule Nothing :nodefault => true

@sjdoc Nothing """
     Nothing(args...)

is replaced by `Nothing`.

Instances of `Nothing` as arguments to `List` are removed.

```
[a,b,Nothing,c] == [a,b,c].
```

`Nothing` is only removed from the arguments of expressions with `Head` `List`.
"""

@doap Nothing(args...) = :Nothing

### Keys

@sjdoc Keys """
    Keys(d)

returns a list of the keys in `Dict` `d`.
"""

apprules(mx::Mxpr{:Keys}) = do_keys(mx,mx[1])
do_keys(mx,d::Associative) = mxpr(:List,collect(Any,keys(d))...)
do_keys(mx,x) = (symwarn("Can't return keys of $x"); mx)

### Values

@sjdoc Values """
    Values(d)

return a list of the values in `Dict` `d`.
"""

apprules(mx::Mxpr{:Values}) = do_values(mx,mx[1])
do_values(mx,d::Associative) = mxpr(:List,collect(Any,values(d))...)
do_values(mx,x) = (symwarn("Can't return values of $mx"); mx)

### Splat

@mkapprule Splat :nargs => 1

@sjdoc Splat """
    Splat(expr)

equivalent to `Apply(Sequence, expr)`.

In a list of arguments `Splat(expr)` is replaced by the arguments of `expr`.

```
symata> f(a,b, Splat([c,d]))
 f(a,b, c, d)
```
"""

@doap Splat(x::Mxpr) = mxprcf(:Sequence, margs(x))
@doap Splat(x) = x

### Sort

@mkapprule Sort

@sjdoc Sort """
    Sort(expr)

sort the elements of `expr`.
"""

@doap Sort(expr::Mxpr{:List}) = mxpr(:List,sort(margs(expr)))
@doap Sort(a::AbstractArray) = sort(a)

### Split

@sjdoc Split """
    Split(lst)

splits `lst` into runs of identical elements.

    Split(lst,test)

apply `test` to determine identical elements.
"""
@mkapprule Split

@doap function Split(lst::ListT)
    isempty(lst) && return MList()
    a = margs(lst)
    a0 = newargs()
    a1 = newargs()
    for i in 1:length(a)
        if length(a1) != 0 && a[i-1] != a[i]
            push!(a0, MList(a1))
            a1 = newargs()
        end
        push!(a1, a[i])
    end
    push!(a0,MList(a1))
    MList(a0)
end

@doap function Split(lst::ListT, test)
    isempty(lst) && return MList()
    a = margs(lst)
    a0 = newargs()
    a1 = newargs()
    for i in 1:length(a)
        if length(a1) != 0
            t = isa(doeval(mxpr(test, a[i-1], a[i])),Bool)
            if ! t
                push!(a0, MList(a1))
                a1 = newargs()
            end
        end
        push!(a1, a[i])
    end
    push!(a0,MList(a1))
    MList(a0)
end


### Partition

@sjdoc Partition """
    Partition(list,n)

partitions `list` into lists of `n` elements.

    Partition(list,n,m)

partitions `list` into lists of `n` elements with offset `m`.

`Partition(list,n)` is equivalent to `Partition(list,n,n)`
"""
@mkapprule Partition :nargs => 2:3

@doap Partition(x::Mxpr, n::Integer) = partition(x,n,n)
@doap Partition(x::Mxpr, n::Integer, m::Integer) = partition(x,n,m)

function partition(x::Mxpr, n, m)
    m < 1 && symerror("Partition, m must be greater than zero")
    h = mhead(x)
    a = margs(x)
    na = newargs()
    i = 1
    while true
        na1 = newargs(n)
        if i + n - 1 > length(a) break end
        for j in 1:n
            na1[j] = a[i]
            i += 1
        end
        i -= (n - m)
        push!(na, mxpr(h,na1))
    end
    mxpra(h,na)
end

### Riffle

@mkapprule Riffle

## TODO: implement all features. document
@doap function Riffle(x::Mxpr{:List},ex)
    n = 2 * length(x)
    nargs = newargs(n)
    j = 0
    for i in 1:2:n
        j += 1
        nargs[i] = x[j]
        nargs[i+1] = ex
    end
    MListA(nargs)
end

@doap function Riffle(x::Mxpr{:List},ex::Mxpr{:List})
    nargs = newargs()
    j = 1
    n = length(ex)
    for i in 1:length(x)
        push!(nargs,x[i])
        push!(nargs,ex[j])
        j = j >= n ? 1 : j + 1
    end
    MListA(nargs)
end

### Union

@mkapprule Union

@doap function Union(lists::Mxpr...)
    isempty(lists) && return mxpr(:List)
    seen = Dict{Any,Bool}()
    nargs = newargs()
    for list in lists
        a = margs(list)
        for x in a
            if ! haskey(seen,x)
                push!(nargs,x)
                seen[x] = true
            end
        end
    end
    mxpr(mhead(lists[1]),nargs...)
end

### Thread

@sjdoc Thread """
    Thread(f(args))

threads `f` over any lists appearing in `args`.

    Thread(f(args),h)

threads `f` over any expressions with head `h`

    Thread(f(args),h,spec)

threads only over elments specified by the standard seuence specification `spec.
"""
@mkapprule Thread :nargs => 1:3

@doap Thread(x::Mxpr) = threadlistable(x)
@doap Thread(x::Mxpr,head) = threadlistable(x,head)

## TODO: generate this with a macro or something, there is a copy in evaluation.jl with :List hardcoded
function threadlistable(mx::Mxpr, head)
    pos = Array{Int}(0)      # should avoid this
    lenmx = length(mx)
    lenlist::Int = -1
    h = mhead(mx)
    @inbounds for i in 1:lenmx
        if is_Mxpr(mx[i],head)
            nlen = length(mx[i])
            if lenlist >= 0 && nlen != lenlist
                error("Can't thread over lists of different lengths.")
            end
            lenlist = nlen
            push!(pos,i)
        end
    end
    lenp = length(pos)
    lenp == 0 && return mx      # Nothing to do. return input array
    largs = newargs(lenlist)
    @inbounds for i in 1:lenlist
        nargs = newargs(lenmx)
        p = 1
        @inbounds for j in 1:lenmx
            if p <= lenp && pos[p] == j
                nargs[j] = mx[j][i]
                p += 1
            else
                nargs[j] = mx[j]
            end
        end
        largs[i] = mxpr(h,nargs)
    end
    nmx = mxpr(head,largs)
    return nmx
end

@doap function Thread(x::Mxpr,head,inspec)
    seqspec = sequencespec(inspec,length(x))
    threadlistable(x,head,seqspec)
end

threadlistable(x,head,s::SequenceAll) = threadlistable(x,head)
threadlistable(x,head,s::SequenceNone) = x  # copy ?

function threadlistable(mx, head, seqspec::SequenceSpec)
    pos = Array{Int}(0)      # should avoid this
    lenmx = length(mx)
    lenlist::Int = -1  # length of lists that we will thread over
    h = mhead(mx)
    (n1,n2,di) = seqiter(seqspec,lenmx)
    @inbounds for i in n1:di:n2
        if is_Mxpr(mx[i],head)
            nlen = length(mx[i])
            if lenlist >= 0 && nlen != lenlist
                error("Can't thread over lists of different lengths.")
            end
            lenlist = nlen
            push!(pos,i)
        end
    end
    lenp = length(pos)
    lenp == 0 && return mx      # Nothing to do. return input array
    largs = newargs(lenlist)
    @inbounds for i in 1:lenlist
        nargs = newargs(lenmx)
        p = 1
        @inbounds for j in 1:lenmx
            if p <= lenp && pos[p] == j
                nargs[j] = mx[j][i]
                p += 1
            else
                nargs[j] = mx[j]
            end
        end
        largs[i] = mxpr(h,nargs)
    end
    nmx = mxpr(head,largs)
    return nmx
end

### MapThread

@mkapprule MapThread nargs => 2  """
    MapThread(f,list)

threads `f` over lists in `list` and evaluates the result.
"""

@doap MapThread(f,x) = threadlistable(mxpr(f,margs(x)))

### Counts

@mkapprule Counts nargs => 1 """
    Counts(list)

return a dictionary of the number of times each distinct element of list
occurs.
"""

@doap function Counts(list::ListT)
    d = OrderedDict{Any,Int}()
    for el in list
        val = get!(d,el,0)
        d[el] = val + 1
    end
    d
end

### Select

@mkapprule Select nargs => 1:3

@doap function Select(x::Mxpr, crit)
    nargs = newargs()
    for y in x
        if trueq(doeval(mxpr(crit,y))) push!(nargs, y) end
    end
    mxpra(mhead(x),nargs)
end

@doap function Select(x::Mxpr, crit, n::Integer)
    nargs = newargs()
    c = 1
    for y in x
        if trueq(doeval(mxpr(crit,y)))
            push!(nargs, y)
            c += 1
            c > n && break
        end
    end
    mxpra(mhead(x),nargs)
end

@curry_second Select

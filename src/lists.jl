#### Args

@sjdoc Args "
Args(ex) replaces the Head of expression ex with List. Args(ex) is equivalent to Apply(List,ex).
"
@mkapprule Args :nargs => 1

do_Args(mx::Mxpr{:Args}, ex::Mxpr) = mxpr(:List, copy(margs(ex)))

#### First

@mkapprule First :nargs => 1

@doap function First(x::Mxpr)
    length(x) == 0 && return mx
    return x[1]
end

@doap First(x) = mx

#### Rest

@mkapprule Rest :nargs => 1

@doap function Rest(x::Mxpr)
    length(x) == 0 && return mx
    nargs = newargs(length(x)-1)
    for i in 1:length(nargs)
        nargs[i] = deepcopy(x[i+1])
    end
    mxpr(mhead(x),nargs)
end

@doap Rest(x) = mx

#### Most

@mkapprule Most :nargs => 1

@doap function Most(x::Mxpr)
    length(x) == 0 && return mx
    nargs = newargs(length(x)-1)
    for i in 1:length(nargs)
        nargs[i] = deepcopy(x[i])
    end
    mxpr(mhead(x),nargs)
end

@doap Most(x) = mx

#### Last

@mkapprule Last :nargs => 1:2

# FIXME. add warning message
@doap Last(x) = mx

@doap function Last(x::Mxpr)
    length(x) == 0 && return mx  # FIXME add warning
    x[end]
end

@doap function Last(x::Mxpr,default)
    length(x) == 0 && return doeval(default)  # Last has Attribute HoldRest
    x[end]
end


#### Fold

@mkapprule Fold :nargs => 2:3

@sjdoc Fold "
Fold(f,x,[a,b,c,...]) returns  f(f(f(x,a),b),c)...
Fold(f,lst) returns  f(First(lst),Rest(lst))
f may be a Symbol, or a function, or Julia function. Pure functions are not yet implemented
"

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
                    res = doeval(f(x,lst[1]))
                    $(fl ? :(nargs[2] = res) : nothing)
                    for i in 2:n
                        res = f(res,lst[i])
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
                $(fl ? :(mxpr(mhead(lst), nargs)) : :( res ))
         end

   @doap function ($head)(f,lst::Mxpr)
           length(lst) == 0 && return mx
           mxpr($(QuoteNode(head)), f, lst[1], mxpr(mhead(lst),lst[2:end]))
         end
end
end

@mkapprule Nest :nargs => 3
@mkapprule NestList :nargs => 3

for head in (:Nest, :NestList)
    nl = (head == :NestList)
@eval begin
    @doap function ($head)(f, x, n::Integer)
                local res
                n == 0 && return $(nl ? :(mxpr(:List,x)) : :(x))
                $(nl ? :(nargs = newargs(n+1)) : nothing)
                $(nl ? :(nargs[1] = x) : nothing)
                if isa(f,Function)
                    res = doeval(f(x))
                    if is_throw() return res end
                    $(nl ? :(nargs[2] = res) : nothing)
                    for i in 2:n
                        res = f(res)
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
                $(nl ? :(mxpr(:List, nargs)) : :( res ))
         end
end
end

#### Range

@sjdoc Range "
Range(n) returns the List of integers from 1 through n.
Range(n1,n2) returns the List of numbers from n1 through n2.
Range(n1,n2,di) returns the List of numbers from n1 through n2 in steps of di
di may be negative. Floats and some symbolic arguments are supported.
You can get also get SJulia lists like using Unpack(:([1.0:10^5])).
This uses embedded Julia to create a typed Array and then unpacks it to a List.
"

# Need to check for uprules for free symbols
function apprules(mx::Mxpr{:Range})
    iter = make_sjitera(margs(mx))
    args = do_Range(iter)
    r = mxpr(:List,args)
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
  @inbounds @simd for i in 1:n
                     args[i] = i
                  end
    return args
end

function _do_Range_fill{T<:Real}(args, n, ::Type{T})
   j = one(T)
    @inbounds for i in 1:n
       args[i] = j
       j += 1
    end
    return args
end


# Fails for rationals. nd counting is wrong
function do_Range{T<:Real,V<:Real}(iter::SJIterA2{T,V})
    nd = round(Int,mplus(iter.imax,-iter.imin) + 1)  # Try bug fix!
    if nd > 1
        args = newargs(iter.num_iters)
        @inbounds for i in 0:nd-1
            args[i+1] = mplus(i,iter.imin)  # Bug here, if iter.imin is not of type Int.
        end
    else  # Mma does not allow this second branch: eg Range(5,1) implies di = -1
        nd = -nd + 2
        args = newargs(iters.num_iters)
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
function do_Range{T<:Real,V<:Real,W<:Real}(iter::SJIterA3{T,V,W})
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
    r = mxpr(:List,args)
    setfixed(r)
    setcanon(r)
    mergesyms(r,:nothing)
    return r
end

# separate functions are *essential* for type stability and efficiency.
function range_args1{T<:Integer}(n::T)
    args = newargs(n);
    @inbounds for i in one(n):n
        args[i] = i
    end
    return args
end

function range_args1{T<:AbstractFloat}(n::T)
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

#### ConstantArray

@sjdoc ConstantArray "
ConstantArray(expr,n) creates a list of n copies of expr.
"

# We take only attribute to be Protected. So expr is evaled already
function apprules(mx::Mxpr{:ConstantArray})
    do_ConstantArray(mx,margs(mx)...)
end

do_ConstantArray(mx,args...) = mx

# The annotation for Number is needed, because deepcopy tries
# to do something very slow with numbers.
# Copying a small Mxpr is extremely slow
# 'c^2' is 40 times slower than Symbol 'c'.
function do_ConstantArray(mx,expr,n)
    nargs = newargs(n)
    @inbounds for i in 1:n
        nargs[i] = recursive_copy(expr)
    end
    setfixed(mxpr(:List,nargs))
end

# We need to find something more efficient than deepcopy
function do_ConstantArray(mx,expr::Mxpr,n)
    nargs = newargs(n)
     @inbounds for i in 1:n
        nargs[i] = setfixed(recursive_copy(expr))
    end
    setfixed(mxpr(:List,nargs))
end

function do_ConstantArray{T<:Union{Number,SJSym}}(mx,expr::T,n)
    nargs = newargs(n)
    @inbounds for i in 1:n
        nargs[i] = expr
    end
    setfixed(mxpr(:List,nargs))
end

function do_ConstantArray(mx,expr::AbstractString,n)
    nargs = newargs(n)
    @inbounds for i in 1:n
        nargs[i] = copy(expr)
    end
    setfixed(mxpr(:List,nargs))
end

#### Nothing

# Nothing is removed where it appears in a List, but not from expressions with other Heads.
# This is too expensive to implement as a rule. Better to write special code in evaluation.jl

@mkapprule Nothing :nodefault => true

@sjdoc Nothing "
Nothing(args...) evaluates to Nothing
Instances of Nothing as arguments to List are removed.
[a,b,Nothing,c] == [a,b,c].
Nothing is only removed from the arguments of expressions with head List.
"

# I can't find the attributes of Mma's Nothing
@doap Nothing(args...) = :Nothing

#### Table

## Newer Table.
# NOTE: we need to change to dynamic scoping
# Instead of localizing the iterating var iter in expr, we
# find and record the positions in expr where iter occurs
# and set all occurence in a loop each time before evaluating.
# Testing with the type NSYM, we see that a simpler structure
# can be copied much more quickly.
# It runs TableNew(a(i),[i,10^5]) twice as fast as the usual
# Table, but it is still slow. Probably creating new mxprs
# is expensive. But, older code had free sym lists and was
# faster. Still don't understand why.

@sjdoc Table "
Table(expr,[imax]) returns a list of imax copies of expr.
Table(expr,[i,imax]) returns a list of expr evaluated imax times with
i set successively to 1 through imax.
Table(expr,iter) iter can be any standard iterator
Table(expr,iter1,iter2,...)  is equivalent to Table(Table(expr,iter2),iter1)...

This calls an anonymous Julia function. It is currently very slow
Table( (:((x)->(x^2))(i) ),[i,10])
This is much faster
f = :( g(x) = x^2 )
Table( f(i), [i,10])
"

function apprules(mx::Mxpr{:Table})
    expr = mx[1]
    if is_Mxpr(expr,:Jxpr)
        expr = eval(expr)
    end
    excopy = deepcopy(expr)
    iters = mx[2:end]
    if length(iters) == 1
        iter = make_sjiter(iters[1])
        args = do_Table(excopy,iter)
        mx1 = mxpr(:List,args)
        return mx1
    else
        riters = reverse(iters)
        mxout = mxpr(:Table,expr,riters[1])
        for i in 2:length(riters)
            mxout = mxpr(:Table,mxout,riters[i])
        end
        return mxout
    end
    # The following prevent 'Nothing' from being removed from 'List'.
    # At some point, we can try to optimize this.
    # setcanon(mxout)
    # setfixed(mxout)
end

# Making this a kernel is not only useful, but faster.
# Set part in expr given by spec to val
function set_part_spec(expr,spec,val)
    p = expr
    @inbounds for k in 2:(length(spec)-1)
        p = p[spec[k]]
    end
    @inbounds p[spec[end]] = val
end

function set_all_part_specs(expr,specs,val)
    @inbounds for j in 1:length(specs)
        set_part_spec(expr,specs[j],val)
    end
end

function do_Table(expr,iter::SJIter2)
    exprpos = expression_positions(expr,iter.i)
    imax = iter.imax # meval(plainiter[2])
    do_Table(imax,iter.i,expr,exprpos)
end

function do_Table(expr,iter::SJIterList)
    len = length(iter.list)
    args = newargs(len)
    if iter.i == expr
        for i in 1:len
            args[i] =  iter.list[i]
        end
        return args
    end
    exprpos = expression_positions(expr,iter.i)
    for i in 1:len
        item = iter.list[i]
#        println("Doing item $item")
        set_all_part_specs(expr,exprpos,item)
#        println("Done artpd  $i")
        unsetfixed(expr)
        args[i] = doeval(expr)
    end
    return args
end

function do_Table(expr,iter::SJIter1)
    imax = iter.imax
    args = newargs(imax)
    @inbounds for i in 1:imax
        args[i] = doeval(expr)
    end
    return args
end

function do_Table{T<:Real,V<:Real}(expr,iter::SJIter3{T,V})
    exprpos = expression_positions(expr,iter.i)
    imax = doeval(iter.imax) # maybe this should be done earlier. When iter is created ?
    imin = doeval(iter.imin)
    args = newargs(imax-imin+1)
    if iter.i == expr
        @inbounds for i in imin:imax
            args[i-imin+1] = i
        end
    else
        @inbounds for i in imin:imax
            set_all_part_specs(expr,exprpos,i)
            unsetfixed(expr)
            args[i-imin+1] = doeval(expr)
        end
    end
    return args
end

function do_Table{T<:Real, V<:Real, W<:Real}(expr, iter::SJIter4{T,V,W})
    exprpos = expression_positions(expr,iter.i)
    imax = iter.imax
    imin = iter.imin
    di = iter.di
    args = newargs(iter.num_iters)
    j::Int = 0
    if iter.i == expr
      @inbounds for i in imin:di:imax
            j += 1
            args[j] = i
        end
    else
      @inbounds for i in imin:di:imax
            j += 1
            set_all_part_specs(expr,exprpos,i)
            unsetfixed(expr)
            args[j] = doeval(expr)
        end
    end
    return args
end

function do_Table{T<:Integer}(imax::T,isym,exin::Mxpr,exprpos)
    args = newargs(imax)
    clearsyms(exin) # Clear the iterator variable
    ex = exin
    @inbounds for i in 1:imax
        set_all_part_specs(ex,exprpos,i)
        unsetfixed(ex)   # force re-evaluation
        args[i] = doeval(ex)
        setfixed(args[i])
        setcanon(args[i])
    end
    return args
end

# For symbols, either the iterator, or not.
function do_Table{T<:Integer}(imax::T,isym,ex::SJSym,exprpos)
    args = newargs(imax)
    if isym == ex
        @inbounds for i in 1:imax
            args[i] = doeval(i)  # Many ways to make this faster.
            setfixed(args[i])
            setcanon(args[i])
        end
    else
        symv = doeval(ex)
        if is_Mxpr(symv)
            do_table_set_arg_const_copy(args,symv,imax)
        else
            do_table_set_arg_const(args,symv,imax) # much faster to use kernel function here
        end
    end
    return args
end

function do_table_set_arg_const(args,val,imax)
    @inbounds for i in 1:imax
        args[i] = doeval(val)
    end
end

function do_table_set_arg_const_copy(args,val,imax)
    @inbounds for i in 1:imax
        args[i] = setfixed(recursive_copy(val))
    end
end

# ex is anything other than Mxpr or Symbol
function do_Table{T<:Integer}(imax::T,isym,ex,exprpos)
    args = newargs(imax)
    @inbounds for i in 1:imax
        args[i] = ex
    end
    return args
end

# Broken. we need to replace this in Table with find_positions in parts.jl
## Return positions in ex at which subx is a subexpression
# Returns an array of arrays representing part specifications
# ie each array returned is a list of positions at levels.
# The first index refers to the top level and is always zero.
# It essentially means nothing unless the postion spec is
# just [0], in which case, it means the head matchs.
# We actually only know that this works in a few cases used
# by Table

# This seems to be broken, but somehow it works for Table ?
# Maybe it works for everything but heads.
function expression_positions(ex,subx)
    posns = Array(Array{Int,1},0)
    lev = Array(Int,10)
    clev::Int = 1
    lev[clev] = 0
    _expr_positions(ex,subx,lev,posns,clev)
    return posns
end

# Looks broken, but is apparently working for Table
function _expr_positions(ex,subx,lev,posns,clev)
    if is_Mxpr(ex)
        args = margs(ex)
        @inbounds for i in 1:length(args)
            lev[clev+1] = i
            _expr_positions(args[i],subx,lev,posns,clev+1)
        end
        if mhead(ex) == subx  # this is only found in toplevel case
            lev[clev] = 0     # so, move it out of here.
            nlev = copy(lev)
            push!(posns,view(nlev,1:clev))
        end
    end
    if ex == subx
        nlev = copy(lev)
        push!(posns,view(nlev,1:clev))
    else
        nothing
    end
end

#### Keys

@sjdoc Keys "
Keys(d) returns a list of the keys in Dict d
"
apprules(mx::Mxpr{:Keys}) = do_keys(mx,mx[1])
do_keys{T<:Dict}(mx,d::T) = mxpr(:List,collect(Any,keys(d))...)
do_keys(mx,x) = (warn("Can't return keys of $x"); mx)

#### Values

@sjdoc Values "
Values(d) returns a list of the values in Dict d
"

apprules(mx::Mxpr{:Values}) = do_values(mx,mx[1])
do_values{T<:Dict}(mx,d::T) = mxpr(:List,collect(Any,values(d))...)
do_values(mx,x) = (warn("Can't return values of $mx"); mx)

#### Sort

@mkapprule Sort

do_Sort(mx::Mxpr{:Sort},expr::Mxpr{:List}) = mxpr(:List,sort(margs(expr)))

#### Transpose

# TODO implement the rest of the methods; nargs > 1
#@mkapprule Transpose :nargs => 1


#@doap Transpose(lst
#Transpose[{{a, b, c}, {x, y, z}}]

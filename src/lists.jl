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
    args = do_range(iter)
    r = mxpr(:List,args)
    setfixed(r)
    setcanon(r)
    mergesyms(r,:nothing)  # not correct if we have symbols.
    return r
end

function do_range(iter::SJIterA1)  # iter is parameterized, so we hope type of n is inferred.
    n = iter.num_iters
    args = newargs(n);
    j = one(iter.imax)
    @inbounds for i in 1:n
        args[i] = j
        j += 1
    end
    return args    
end

# Fails for rationals. nd counting is wrong
function do_range{T<:Real,V<:Real}(iter::SJIterA2{T,V})
    nd = mplus(iter.imax,-iter.imin) + 1
    if nd > 1
#        args = newargs(abs(nd))
        args = newargs(iter.num_iters)
        for i in zero(iter.imin):nd-1
            args[i+1] = mplus(i,iter.imin)
        end
    else  # Mma does not allow this second branch: eg Range(5,1) implies di = -1
        nd = -nd + 2
        args = newargs(iters.num_iters)
        for i in zero(iter.imin):(nd - 1)
            args[i+1] = mplus(iter.imin, -i)
        end
    end
    return args
end

# Symbolic values
# FIXME. We don't record free symbols and check for upvalues.
# This is about as fast as Mma 3 (running on a somewhat slower cpu)
function do_range(iter::SJIterA2)
    args = newargs(iter.num_iters)
    imin = iter.imin
    args[1] = imin
    s = imin
    if is_Mxpr(imin,:Plus)  # imin is a sum
        if is_Number(imin[1])  # number is always first in canon order.
            b = imin[1]  # extract number
            r = imin[2:end]  # the rest of the sum
            for i in 2:iter.num_iters
                args[i] = mxpr(:Plus,b+i-1,r...) # only a little slower than Mma if disable gc
                setfixed(args[i])
            end
        else  # imin is a sum with no numbers, so we put a number in front
            sargs = margs(s)
            for i in 2:iter.num_iters
                args[i] = mxpr(:Plus,i-1,sargs...)
                setfixed(args[i])
            end
        end
    else  # imin is not a sum
        for i in 2:iter.num_iters
            args[i] = mxpr(:Plus,i-1,s)
            setfixed(args[i])
        end
    end
    #  we don't handle counting down case.
    return args
end    

# seems to be little penalty for mplus instead of +
function do_range{T<:Real,V<:Real,W<:Real}(iter::SJIterA3{T,V,W})
    n = iter.num_iters
    args = newargs(n)
    j = iter.imin
    for i in 1:n
        args[i] = j
        j = mplus(j,iter.di)
    end
    return args
end

# Symbolic again
function do_range(iter::SJIterA3)
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
                for i in 2:iter.num_iters
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

function range_args1{T<:FloatingPoint}(n::T)
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
    for i in 1:n
#        nargs[i] = deepcopy(expr)
        nargs[i] = recursive_copy(expr)
    end
    setfixed(mxpr(:List,nargs))
end

# We need to find something more efficient than deepcopy
function do_ConstantArray(mx,expr::Mxpr,n)
    nargs = newargs(n)
    for i in 1:n
        nargs[i] = setfixed(recursive_copy(expr))
#        nargs[i] = setfixed(deepcopy(expr))  slow 
#        nargs[i] = setfixed(copy(expr))  incorrect
    end
    setfixed(mxpr(:List,nargs))
end

function do_ConstantArray(mx,expr::Union(Number,Symbol),n)
    nargs = newargs(n)
    for i in 1:n
        nargs[i] = expr
    end
    setfixed(mxpr(:List,nargs))
end

function do_ConstantArray(mx,expr::String,n)
    nargs = newargs(n)
    for i in 1:n
        nargs[i] = copy(expr)
    end
    setfixed(mxpr(:List,nargs))
end

#### Table

## Newer Table.
# Instead of localizing the iterating var iter in expr, we
# find and record the positions in expr where iter occurs
# and set all occurence in a loop each time before evaluating.
# Testing with the type NSYM, we see that a simpler structure
# can be copied much more quickly.
# It runs TableNew(a(i),[i,10^5]) twice as fast as the usual
# Table, but it is still slow. Probably creating new mxprs
# is expensive. But, older code had free sym lists and was
# faster. Still don't understand.

@sjdoc Table "
Table(expr,[i,imax]) returns a list of expr evaluated imax times with
i set successively to 1 through imax. Other Table features are not implemented.
Unusual examples:
This calls an anonymous Julia function. It is currently very slow
Table( (:((x)->(x^2))(i) ),[i,10])
This is much faster
f = :( g(x) = x^2 )
Table( f(i), [i,10])
"

# This is also rather limited
#set_attribute(:TableNew, :HoldAll)
function apprules(mx::Mxpr{:Table})
    expr = mx[1]
    if is_Mxpr(expr,:Jxpr)
        expr = eval(expr)
    end
    iter = mx[2]
    exprpos = expression_positions(expr,iter[1])
    imax = meval(iter[2])
    ex = deepcopy(expr)
    args = do_table_new(imax,iter[1],ex,exprpos) # creating a symbol is pretty slow
    mx1 = mxpr(:List,args) # takes no time
#    mergesyms(mx1,:nothing) # not correct, but stops the merging
    setcanon(mx1)
    setfixed(mx1)
    return mx1
end

# Making this a kernel is not only useful, but faster.
# Set part in expr given by spec to val
function set_part_spec2(expr,spec,val)
    p = expr
    @inbounds for k in 2:(length(spec)-1)
        p = p[spec[k]]
    end
    @inbounds p[spec[end]] = val
end

function set_all_part_specs2(expr,specs,val)
    @inbounds for j in 1:length(specs)
        set_part_spec2(expr,specs[j],val)
    end    
end

function do_table_new{T<:Integer}(imax::T,isym,exin::Mxpr,exprpos)
    args = newargs(imax)
    clearsyms(exin) # Clear the iterator variable
    ex = exin
    @inbounds for i in 1:imax
        #        ex = copy(exin)
        set_all_part_specs2(ex,exprpos,i)
        unsetfixed(ex)   # force re-evaluation
        args[i] = doeval(ex)
#        args[i] = meval1(ex)
#        args[i] = meval(ex)
        setfixed(args[i])
        setcanon(args[i])
    end
    return args
end

# For symbols, either the iterator, or not.
function do_table_new{T<:Integer}(imax::T,isym,ex::SJSym,exprpos)
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
        args[i] = val
    end    
end

function do_table_set_arg_const_copy(args,val,imax)
    @inbounds for i in 1:imax
        args[i] = setfixed(recursive_copy(val))
    end    
end

# ex is anything other than Mxpr or Symbol
function do_table_new{T<:Integer}(imax::T,isym,ex,exprpos)
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
            push!(posns,slice(nlev,1:clev))
        end
    end
    if ex == subx
        nlev = copy(lev)
        push!(posns,slice(nlev,1:clev))
    else
        nothing
    end
end

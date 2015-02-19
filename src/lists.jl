
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

## quickly hacked Table, just for testing other parts of evaluation

# Create lexical scope for Table.
# Replace symbol os with ns in ex
function replsym(ex,os,ns)
    if is_Mxpr(ex)
        args = margs(ex)
        @inbounds for i in 1:length(args)
            args[i] = replsym(args[i],os,ns)
        end
    end
    if ex == os
        return ns
    else
        return ex
    end
end

# We only do Table(expr,[i,imax])
# Our Table is rather slow. Slower than Maxima makelist.
# Table(a(i),[i,10000]) is 2 to 3 x slower than makelist( i, i, 1, 10000)
# If we cheat and only do a single evaluation,
# and setfixed() then the speed is the same.
# But, the 3rd party 'table' for Maxima is 4 times faster than makelist
# in this example.
# Mma 3.0  Timing[mm = Table[a[i],{i,10^5}];]  {0.05 Second, Null}
# 3rd pary maxima: table(a(i),[i,10^5]);  0.14 s
# At commit 3478f83ee403238704ad6af81b42e1c7f4d07cc8
# Table(a(i),[i,10000]) takes bout 0.45--0.49 s, makelist reports 0.503.

set_attribute(:TableOld, :HoldAll)

function apprules(mx::Mxpr{:TableOld})
    expr = mx[1]
    iter = mx[2]
    isym = get_localized_symbol(iter[1])
    imax = meval(iter[2])
#    issym = createssym(isym,Int)  ## Trying out Typed variable
    ex = replsym(deepcopy(expr),iter[1],isym) # takes no time, for simple expression
    args = do_table(imax,isym,ex) # creating a symbol is pretty slow
    removesym(isym)
    mx1 = mxpr(:List,args) # takes no time
    # This mergesyms no longer makes this much faster, because overall Table has slowed by more than 5x
    # syms are being merged alot somewhere else
    mergesyms(mx1,:nothing) # not correct, but stops the merging
    setcanon(mx1)
    setfixed(mx1)
    return mx1
end

# These tests concern not Table per se, but the efficiency
# of evaluation, in particular assigning values to symbols.
# commit 3478f83ee403238704ad6af81b42e1c7f4d07cc8
# Testing Table(a(i),[i,10^5])
# changes                   Time
# normal                    0.49
# disable setsymval         0.35
# 
# commit 0d2332b17eb7e5c518a86b1b3044b691fddb5b87
# setsymval is now much faster because we changed data structure in SSJSym
# Testing Table(a(i),[i,10^5])
# normal                    0.36
# *sigh* Cannot reproduce the performance in the previous line. Even if
# I return to that commit. Time is closer to 0.5 than 0.35
# setsymval still seems to be an expensive operation
#
# 4f7d9f6dff207ae2c95736ab058627f503fbad26
# With this commit, Time is 1.83 s. For same code as above.  Table is
# becoming slower. We try using a stripped-down meval1 from
# alteval.jl, time is .43 with no gc.

function do_table{T<:Integer}(imax::T,isym,ex)
    args = newargs(imax)
    sisym = getssym(isym)
#    setsymval(sisym,1)    
    @inbounds for i in 1:imax
        setsymval(sisym,i)
#        args[i] = meval(ex)
        args[i] = doeval(ex)
#        args[i] = meval1(ex)
        setfixed(args[i])  # no difference ?
    end
    return args
end

# The speed penalty for val::Any instead of val::Int, is factor of 10^4.
# Setting isym.val is by far the slowest step in Table(i,[i,10^6])
# Much slower than Mma and SBCL
# If we us val::Array{Any,1}, the speed penalty is better by a couple
# orders of magnitude

# type NSYM
# #    val::Int
#     val::Array{Any,1}
# end

# function testset()
#     ss = NSYM(Any[0])
#     sum0 = 0
#     for i in 1:10^6
#         ss.val[1] = i
#         sum0 += ss.val[1]
#     end
#     sum0
# end

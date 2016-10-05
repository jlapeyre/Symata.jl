#### ExpandA

function _doexpand(x)
    ! is_Mxpr(x) && return x
    n = length(x)
    args = margs(x)
    nargs = newargs(n)
    @inbounds for i in 1:n
        nargs[i] = doexpand(args[i])
    end
    return doexpand(mxpr(mhead(x),nargs))
end

doexpand(p::Mxpr{:Power}) = do_expand_power(p,base(p),expt(p))

function doexpand(prod::Mxpr{:Times})
    a = margs(prod)
    len = length(a)
    @inbounds for i in 1:len
        a[i] = doexpand(a[i])
    end
    have_sum = false
    j = 0
    @inbounds for i in 1:len # check if we have anything to do
        j += 1
        if is_Mxpr(a[i],:Plus)
            have_sum = true
            break
        end
    end
    ! have_sum && return prod
    nonsums = newargs()
    @inbounds for i in 1:j-1  # none of these are sums
        push!(nonsums,a[i])
    end
    sums = newargs()
    push!(sums,a[j]) # already know its a sum
    @inbounds for i in j+1:len   # push more sums, if there are any
        if is_Mxpr(a[i],:Plus)
            push!(sums,a[i])
        else
            push!(nonsums,a[i])
        end
    end
    sumres = length(sums) == 1 ? sums[1] : mulfacs(sums...)
    nlen = length(nonsums)
    if nlen == 0
        mxout = sumres
    elseif nlen == 1
        mxout = mulfacs(nonsums[1],sumres)
    else
        mxout = mulfacs(mxpr(:Times,nonsums),sumres)
    end
    setfixed(mxout)
    return mxout
end

doexpand(mx) = mx

do_expand_power(p,b::Mxpr{:Plus}, n::Integer) =
    length(b) != 2 ? p : do_expand_binomial(p,b[1],b[2],n)
do_expand_power(p,b,ex) = p

do_expand_binomial(p,a, b, n::Integer) = expand_binomial(a,b,n)
do_expand_binomial(a,b,n) = p

function doexpand(s::Mxpr{:Plus})
    args = margs(s)
    @inbounds for i in 1:length(args)
        args[i] = doexpand(args[i])
    end
    return s
end

## expand product of two sums
# a an b are the factors
# In test cases, this is fast. later, canonicalizing each term is slowest thing.
function mulfacs(a::Mxpr{:Plus},b::Mxpr{:Plus})
    terms = newargs(length(a)*length(b))
    i = 0
    for ax in a.args
        for bx in b.args
            i += 1
            t = flatcanon!(mxpr(:Times, ax, bx)) # TODO specialize for types of ax, bx
            mergesyms(t,ax)
            mergesyms(t,bx)
            setfixed(t)
           @inbounds  terms[i] = t
        end
    end
    mx = flatcanon!(mxpr(:Plus,terms))
    for t in terms
        mergesyms(mx,t)
    end
    setfixed(mx)
    mx
end

# Not the right way to do this. We need to expand each term, as well.
mulfacs(a,b,c) = mulfacs(mulfacs(a,b),c)
mulfacs(a,b,c,xs...) = mulfacs(mulfacs(mulfacs(a,b),c),xs...)

# Should probably write this
#function mulfacs(a::Mxpr{:Plus}, b::SJSym)
#end

function _pushfacs!(facs,mx::Mxpr{:Times})
    append!(facs,margs(mx))
end

function _pushfacs!(facs,b)
    push!(facs,b)
end

function mulfacs(a::Mxpr{:Plus},b)
    terms = newargs(length(a))
    i = 0
    for ax in a.args
        i += 1
        if is_Mxpr(ax) && mxprtype(ax) == :Times
            facs = copy(margs(ax))
            _pushfacs!(facs,b)  # I hope type inference optimizes this.
            terms[i] = mxpr(:Times,facs)
        else
            terms[i] = b * ax
        end
        canonexpr!(terms[i])
        mergeargs(terms[i])
    end
    mx = mxpr(:Plus,terms)
    mergeargs(mx)
    mx
end

function mulfacs(a, b::Mxpr{:Plus})
    mulfacs(b,a)
end

## construct Power. Decide whether to canonicalize according to types of args
function canonpower{T<:Real}(base,expt::T)
    canonexpr!(base^expt)
end
function canonpower(base::SJSym,expt)
    base^expt
end
function canonpower(base,expt)
    base^expt
end

##

# optimize a bit for types
function _expand_binomial_aux1(a::SJSym,b::SJSym,n)
    mxpr(:Times,n,canonpower(a,(n-1)),b)
end
function _expand_binomial_aux1(a,b,n)
    flatcanon!(mxpr(:Times,n,canonpower(a,(n-1)),b))
end
function _expand_binomial_aux2(a::SJSym,b::SJSym,n)
    mxpr(:Times,n,a,canonpower(b,(n-1)))
end
function _expand_binomial_aux2(a,b,n)
    flatcanon!(mxpr(:Times,n,a,canonpower(b,(n-1))))
end


# Be careful to construct expression in canonical form.
# Lots of ways to go wrong.
# Assume a < b in canonical order
# This is the only place we are testing meta data in Mxpr giving which
# symbols it depends on.
function expand_binomial{T<:Integer}(a,b,n::T)
    args = newargs(n+1)
    args[1] = canonpower(a,n)
    mergesyms(args[1],a)
    setfixed(args[1])
    args[n+1] =  canonpower(b,n)
    mergesyms(args[n+1],b)
    setfixed(args[n+1])
    if n == 2
        args[2] = flatcanon!(mxpr(:Times,2,a,b))  # we have to flatcanon
    else
        # TODO optimize for symbols a,b, as in general case below. No flatcanon.
        args[2] = _expand_binomial_aux1(a,b,n)
        args[n] = _expand_binomial_aux2(a,b,n)
        mergesyms(args[2],a)
        mergesyms(args[2],b)
        mergesyms(args[n],a)
        mergesyms(args[n],b)
        setfixed(args[2])
        setfixed(args[n])
        fac = n
        k = n
        l = one(n)
        expand_binomial_aux(k,l,n,fac,a,b,args)
    end
    mx = mxprcf(:Plus,args)
    mergesyms(mx,a)
    mergesyms(mx,b)
    apply_upvalues_to_args!(mx)  # takes some time
    setage(mx)
    setfixed(mx)
    mx
end

# Big increase in efficiency (> 10x) for both these types
# typealias ExpNoCanon Union{SJSym,Number} move to symataconstants.jl

# Expand((a+b*c)^n) is 10x slower than Expand((a+b)^n)
function _expand_mulpowers(fac,b1,e1,b2,e2)
    m1 = canonpower(b1,e1)  # adds 10-15% time
    m2 = canonpower(b2,e2)
    setfixed(m1)
    setfixed(m2)
    mergesyms(m1,b1)
    mergesyms(m2,b2)
    return flatcanon!(mxpr(:Times, fac, m1, m2)) # flatcanon adds 10x time !, even if nothing is done
end

function _expand_mulpowers{T<:ExpNoCanon, V<:ExpNoCanon}(fac,b1::T,e1,b2::V,e2)
    m1 = b1^e1
    m2 = b2^e2
    setfixed(m1)
    setfixed(m2)
    mergesyms(m1,b1)
    mergesyms(m2,b2)
    return mxpr(:Times, fac, m1, m2)
end

function _expand_mulpowers{T<:ExpNoCanon}(fac,b1::T,e1,b2,e2)
    m1 = b1^e1
    m2 = canonpower(b2,e2)
    setfixed(m1)
    setfixed(m2)
    mergesyms(m1,b1)
    mergesyms(m2,b2)
    return flatcanon!(mxpr(:Times, fac, m1, m2))
end

function _expand_mulpowers{T<:ExpNoCanon}(fac,b1,e1,b2::T,e2)
    m1 = canonpower(b1,e1)
    m2 = b2^e2
    setfixed(m1)
    setfixed(m2)
    mergesyms(m1,b1)
    mergesyms(m2,b2)
    return flatcanon!(mxpr(:Times, fac, m1, m2))
end

function expand_binomial_aux(k,l,n,fac,a,b,args)
        @inbounds for j in 2:n-2
            k = k - 1
            l = l + 1
            fac *= k
            fac = div(fac,l)
            args[j+1] = _expand_mulpowers(fac,a,n-j,b,j)
            mergesyms(args[j+1],a)
            mergesyms(args[j+1],b)
            setfixed(args[j+1])
        end
end

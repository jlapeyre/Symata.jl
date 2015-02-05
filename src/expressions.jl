## expand product of two sums

## In test cases, this is fast. later, canonicalizing each term is
# slowest thing.
function mulsums(a::Mxpr{:Plus},b::Mxpr{:Plus})
#    println("1:  $a,  $b")
    terms = newargs(length(a)*length(b))
    i = 0
    for ax in a.args
        for bx in b.args
            i += 1
#            println("  1:  $ax,  $bx")
            t = flatcanon!(mxpr(:Times, ax, bx))
            mergesyms(t,ax)
            mergesyms(t,bx)
            setfixed(t)
            terms[i] = t
        end
    end
    mx = flatcanon!(mxpr(:Plus,terms))
    for t in terms   # all these merging don't take time. s.t. else is slow:  orderexpr! is not slowest
        mergesyms(mx,t)
    end
    setfixed(mx)
    mx
end

# Not the right way to do this. We need to expand each term, as well.
mulsums(a,b,c) = mulsums(mulsums(a,b),c)
mulsums(a,b,c,xs...) = mulsums(mulsums(mulsums(a,b),c),xs...)

#mulsums(a::Mxpr{:Plus},b::Mxpr{:Plus},c::Mxpr{:Plus}) = mulsums(mulsums(a,b),c)
#mulsums(a::Mxpr{:Plus},b::Mxpr{:Plus},c::Mxpr{:Plus}, xs::Mxpr{:Plus}...) = mulsums(mulsums(mulsums(a,b),c),xs...)

function mulsums(a::Mxpr{:Plus},b)
#    println("2:  $a,  $b")    
    terms = newargs(length(a))
    i = 0
    for ax in a.args
        i += 1
        if is_Mxpr(ax) && mxprtype(ax) == :Times
            facs = copy(margs(ax))
            push!(facs,b)
            terms[i] = mxpr(:Times,facs)
        else
            terms[i] = b * ax
        end
    end
    mxpr(:Plus,terms)
end

function mulsums(a, b::Mxpr{:Plus})
#    println("3:  $a,  $b")
    mulsums(b,a)
end


function canonpower(base::SJSym,expt)
    base^expt
end

function canonpower{T<:Real}(base,expt::T)
    canonexpr!(base^expt)
end

function canonpower(base,expt)
    base^expt
end

## 

# Be careful to construct expression in canonical form.
# Lots of ways to go wrong.
# Assume a < b in canonical order
# This is the only place we are testing meta data in Mxpr giving which
# symbols it depends on.
function expand_binomial(a,b,n::Integer)
    args = newargs(n+1)
    args[1] = canonpower(a,n)
    mergesyms(args[1],a)
    setfixed(args[1])
    args[n+1] =  canonpower(b,n)
    mergesyms(args[n+1],b)
    setfixed(args[n+1])    
    if n == 2
        args[2] = mxpr(:Times,2,a,b)  # don't use 2 * a * b; it won't be flat
    else 
        args[2] = flatcanon!(mxpr(:Times,n,canonpower(a,(n-1)),b)) # TODO optimize for symbols a,b. No flatcanon.
        args[n] = flatcanon!(mxpr(:Times,n,a,canonpower(b,(n-1))))
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
    setage(mx)
    setfixed(mx)
    mx
end

function _expand_mulpowers(fac,b1,e1,b2,e2)
    m1 = canonpower(b1,e1)
    m2 = canonpower(b2,e2)
    setfixed(m1)
    setfixed(m2)
    mergesyms(m1,b1)
    mergesyms(m2,b2)            
    return flatcanon!(mxpr(:Times, fac, m1, m2))
end

# Does not seem to help efficiency
function expand_binomial_aux(k,l,n,fac,a,b,args)
        @inbounds for j in 2:n-2
            k = k - 1
            l = l + 1
            fac *= k
            fac = div(fac,l)
            # m1 = canonpower(a,n-j) #   mxpr(:Power, a, (n-j))
            # m2 = canonpower(b,j) # mxpr(:Power,b,j)
            # setfixed(m1)
            # setfixed(m2)
            # mergesyms(m1,a)
            # mergesyms(m2,b)            
            # args[j+1] = flatcanon!(mxpr(:Times, fac, m1, m2))
            args[j+1] = _expand_mulpowers(fac,a,n-j,b,j)
            mergesyms(args[j+1],a)
            mergesyms(args[j+1],b)
            setfixed(args[j+1])
        end
end

@sjdoc Apply "
Apply(f,expr) replaces the Head of expr with f.
"
function apprules(mx::Mxpr{:Apply})
    doapply(mx,mx[1],mx[2])
end
doapply(mx::Mxpr,h::SJSym,mxa::Mxpr) = mxpr(h,(mxa.args))

# Apply operation to a typed numeric array.
# We can build these functions with a macro and
# mapping from  :Times -> mmul
# :Cos -> cos, etc.
function doapply{T<:Number}(mx::Mxpr,h::SJSym,arr::Array{T})
    if h == :Plus
        s = zero(T)
        for i in 1:length(arr)
            s += arr[i]
        end
        return s
    end
    return mx
end

doapply(mx,x,y) = mx

function Base.reverse(mx::Mxpr)
    mx1 = copy(mx)
    Base.reverse!(margs(mx1))
    return mx1
end

@sjdoc Reverse "
Reverse(expr) reverses the order of the arguments in expr.    
"
function apprules(mx::Mxpr{:Reverse})
    do_reverse(mx[1])
end

# they would only be resorted
do_reverse(mx::Orderless) = mx

function do_reverse(mx::Mxpr)
    mxpr(mx.head,reverse(margs(mx)))
end

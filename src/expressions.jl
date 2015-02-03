## expand product of two sums

## In test cases, this is fast. later, canonicalizing each term is
# slowest thing.
function mulsums(a::Mxpr{:Plus},b::Mxpr{:Plus})
    terms = newargs(length(a)*length(b))
    i = 0
    for ax in a.args
        for bx in b.args
            i += 1
            t = mxpr(:Times, ax, bx)
            mergesyms(t,ax)
            mergesyms(t,bx)
            setfixed(t)
            terms[i] = t
        end
    end
    mx = mxpr(:Plus,terms)
    for t in terms   # all these merging don't take time. s.t. else is slow:  orderexpr! is not slowest
        mergesyms(mx,t)
    end
    setfixed(mx)
    mx
end

mulsums(a::Mxpr{:Plus},b::Mxpr{:Plus},c::Mxpr{:Plus}) = mulsums(mulsums(a,b),c)
mulsums(a::Mxpr{:Plus},b::Mxpr{:Plus},c::Mxpr{:Plus}, xs::Mxpr{:Plus}...) = mulsums(mulsums(mulsums(a,b),c),xs...)

#function mulsums(a::Mxpr{:Plus},b::Mxpr{:Plus})
#end

# Be careful to construct expression in canonical form.
# Lots of ways to go wrong.
# Assume a < b in canonical order
# This is the only place we are testing meta data in Mxpr giving which
# symbols it depends on.
function expand_binomial(a,b,n::Integer)
    args = newargs(n+1)
    args[1] = a^n
    mergesyms(args[1],a)
    setfixed(args[1])
    args[n+1] =  b^n
    mergesyms(args[n+1],b)
    setfixed(args[n+1])    
    if n == 2
        args[2] = mxpr(:Times,2,a,b)  # don't use 2 * a * b; it won't be flat
    else 
        args[2] = mxpr(:Times,n,a^(n-1),b)
        args[n] = mxpr(:Times,n,a,b^(n-1))
        mergesyms(args[2],a)
        mergesyms(args[2],b)
        mergesyms(args[n],a)
        mergesyms(args[n],b)
        setfixed(args[2])
        setfixed(args[n])
        fac = n
        k = n
        l = one(n)
        for j in 2:n-2
            k = k - 1
            l = l + 1
            fac *= k
            fac = div(fac,l)
            m1 = mxpr(:Power, a, (n-j))
            m2 = mxpr(:Power,b,j)
            setfixed(m1)
            setfixed(m2)
            mergesyms(m1,a)
            mergesyms(m2,b)            
            args[j+1] = mxpr(:Times, fac, m1, m2)
            mergesyms(args[j+1],a)
            mergesyms(args[j+1],b)
            setfixed(args[j+1])
#            args[j+1] = mxpr(:Times, fac , mxpr(:Power, a, (n-j)), mxpr(:Power,b,j))            
        end
    end
    mx = mxprcf(:Plus,args)
    mergesyms(mx,a)
    mergesyms(mx,b)
    setage(mx)
    setfixed(mx)
    mx
end


function apprules(mx::Mxpr{:Apply})
    doapply(mx,mx[1],mx[2])
end
doapply(mx::Mxpr,h::SJSym,mxa::Mxpr) = mxpr(h,(mxa.args)...)
doapply(mx,x,y) = mx

function Base.reverse(mx::Mxpr)
    mx1 = copy(mx)
    Base.reverse!(margs(mx1))
    return mx1
end

function apprules(mx::Mxpr{:Reverse})
    do_reverse(mx[1])
end

# they would only be resorted
do_reverse(mx::Orderless) = mx

function do_reverse(mx::Mxpr)
    mxpr(mx.head,reverse(margs(mx)))
end

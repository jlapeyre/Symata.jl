## expand product of two sums

## This code is not used yet.
function mulsums(a::Mxpr{:Plus},b::Mxpr{:Plus})
    terms = newargs()
    for ax in a.args
        for bx in b.args
            push!(terms,ax * bx)
        end
    end
    # efficiency, we need to limit this to two levels deep.
    # i.e., we need level specifications
    deepcanonexpr!(mxpr(:Plus,terms...))
end

mulsums(a::Mxpr{:Plus},b::Mxpr{:Plus},c::Mxpr{:Plus}) = mulsums(mulsums(a,b),c)
mulsums(a::Mxpr{:Plus},b::Mxpr{:Plus},c::Mxpr{:Plus}, xs::Mxpr{:Plus}...) = mulsums(mulsums(mulsums(a,b),c),xs...)

function apprules(mx::Mxpr{:Apply})
    doapply(mx,mx[1],mx[2])
end

doapply(mx::Mxpr,h::SJSym,mxa::Mxpr) = mxpr(h,(mxa.args)...)
doapply(mx,x,y) = mx

# Be careful to construct expression in canonical form.
# Lots of ways to go wrong.
# Assume a < b in canonical order
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

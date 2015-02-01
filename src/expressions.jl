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
    args[n+1] =  b^n
    if n == 2
        args[2] = mxpr(:Times,2,a,b)  # don't use 2 * a * b; it won't be flat
#        return mxprcf(:Plus,args)
    else 
        args[2] = mxpr(:Times,n,a^(n-1),b)
        args[n] = mxpr(:Times,n,a,b^(n-1))
        fac = n
        k = n
        l = one(n)
        for j in 2:n-2
            #        args[j+1] = binomial(n,j) * a^(j) * b^(n-j)  # slower.
            k = k - 1
            l = l + 1
            fac *= k
            fac = div(fac,l)
            #        args[j+1] = fac * a^(j) * b^(n-j)  #  maybe a bit slower
            args[j+1] = mxpr(:Times, fac , mxpr(:Power, a, (n-j)), mxpr(:Power,b,j))
        end
    end
    mx = mxprcf(:Plus,args)
    is_SJSym(a) ? mergesyms(mx,a) : nothing
    is_SJSym(b) ? mergesyms(mx,b) : nothing
#    println("expand setting age of $mx")
    setage(mx)
    setfixed(mx)
#    print("******* dumping syms: ")
#    dump(mx.syms)
    mx
end

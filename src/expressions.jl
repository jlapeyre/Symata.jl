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

function expand_binomial(b,a,n::Integer)
    args = newargs(n+1)
    args[1] = b^n
    args[n+1] =  a^n
    if n == 2
        args[2] = 2 * a * b
        return mxprcf(:Plus,args)
    end
    args[2] = n * a * b^(n-1)
    args[n] =  n * a^(n-1) * b
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
        args[j+1] = mxpr(:Times, fac , mxpr(:Power,a,j),  mxpr(:Power, b, (n-j)))
    end
    mxprcf(:Plus,args)
end

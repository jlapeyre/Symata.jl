## Code for investigating why Table slowed down a factor of 5.

# This is stripped down evaluation

@inline meval1(x::Complex) = x.im == 0 ? x.re : x
@inline meval1(x) = x
@inline meval1(s::SJSym) = symval(s)

function meval1(mx::Mxpr)
    args = margs(mx)
    len = length(args)
    nargs = newargs(len)
    @inbounds for i in 1:len
        res = meval1(args[i])
        nargs[i] = res
    end
    mxpr(mhead(mx),nargs)
end

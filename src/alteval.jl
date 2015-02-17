## Code for investigating why Table slowed down a factor of 5.

# This is stripped down evaluation

@inline meval1(x::Complex) = x.im == 0 ? x.re : x
@inline meval1(x) = x
@inline meval1(s::SJSym) = symval(s)

# This does not help
function evalargs(args,nargs)
    @inbounds for i in 1:length(args)
        res = meval1(args[i])
        nargs[i] = res
    end
end

function meval1(mx::Mxpr)
    args = margs(mx)
    len = length(args)
    nargs = newargs(len)
#   evalargs(args,nargs)
    @inbounds for i in 1:len
        res = meval1(args[i])
        nargs[i] = res
    end
#    mx.args = nargs
    nmx = mxpr(mhead(mx),nargs)
    apprules(nmx)
end

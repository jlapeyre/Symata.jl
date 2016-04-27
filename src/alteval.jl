#  Fast, stripped down expression evaluation code for investigating
#  speed of doeval, meval.  In particular, Table takes unevaled args
#  and calls doeval. So we call meval1 instead in Table (for testing)

# This is stripped down evaluation

@inline meval1(x::Complex) = x.im == 0 ? x.re : x
@inline meval1(x) = x
@inline meval1(s::SJSym) = symval(s)

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
    nmx = flatcanon!(nmx)
    nmx = ev_upvalues(nmx)
    nmx = ev_downvalues(nmx)
    merge_args_if_emtpy_syms(nmx)
    apprules(nmx)
end

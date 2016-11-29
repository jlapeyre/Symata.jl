### Now

## TODO take 1 arg as well
@mkapprule Now :nargs => 0

@doap Now() = now()

### Timing

@sjdoc Timing """
    Timing(expr)

evaluate `expr` and return a `List` of the elapsed CPU time
and the result.
"""
@mkapprule Timing  :nodefault => true

@sjseealso_group(Timing,Allocated,Time,Trace)

@doap function Timing(exprs...)
    local mxnew
    t = @elapsed begin
        reset_meval_count()
        for x in exprs
            mxnew = doeval(x)
        end
        setsymval(:ans,mxnew)
    end
    mxpr(:List,t,mxnew)
end

### Pause

@sjdoc Pause """
    Pause(x)

pauses (i.e.sleeps) for `x` seconds.
"""
@mkapprule Pause  :nargs => 1
@doap Pause{T<:Real}(x::T) = sleep(x)

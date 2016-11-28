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

@sjseealso_group(Timing,Allocated,Time,Trace)

@mkapprule Timing  :nodefault => true

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

# function apprules(mxt::Mxpr{:Timing})
#     t = @elapsed begin
#         reset_meval_count()
#         mx = doeval(mxt[1])
#         setsymval(:ans,mx)
#     end
#     mxpr(:List,t,mx)
# end

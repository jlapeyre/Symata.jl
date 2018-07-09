### Now

## TODO take 1 arg as well
@mkapprule Now :nargs => 0

@doap Now() = Dates.now()

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
@doap Pause(x::T) where {T<:Real} = sleep(x)

## Partial implementations of these...

### UnixTime

@mkapprule UnixTime :nargs => 0

@doap UnixTime() = round(Int,time())

### AbsoluteTime

#mma2datetime(t) = 

@mkapprule AbsoluteTime :nargs => 0

## FIXME: works in v0.6. Will break for earlier versions
@doap AbsoluteTime() = Dates.value(Dates.now() - Dates.DateTime(1900))/1000

### DateList

## Note: we can use DateParser.jl at some point.

function _datelist(t)
    MList(Dates.year(t),Dates.month(t),Dates.day(t),Dates.hour(t),Dates.minute(t),Dates.second(t))
end

@mkapprule DateList

@doap DateList(x::ListT) = _datelist(Dates.DateTime(margs(x)...))

@doap DateList() = _datelist(Dates.now())

#@doap DateList(t::AbstractFloat) = _datelist(Dates.DateTime(t))

### Run

@sjdoc Run """
    Run(\`cmd\`)

runs the symstem command `cmd`. Note the backticks around `cmd`.
"""
@mkapprule Run :nargs => 1

# eg Run(`echo hello`)
@doap Run(x::Cmd) = run(x)

### Environment

@mkapprule Environment :nargs => 1

## Mma returns $Failed on failure
@doap Environment(x::String) = get(ENV,x,Null)

### GetEnvironment

@mkapprule GetEnvironment

## Mma returns a rule. We just return the val here.
@doap GetEnvironment(x::String) = get(ENV,x,Null)

## We return a Dict, not a list of Rules
@doap GetEnvironment() = ENV

## Mma gets and receives objects somehow

### CopyToClipboard

@mkapprule CopyToClipboard :nargs => 1
@doap CopyToClipboard(x) = clipboard(wrapout(x))

### Paste

@mkapprule Paste :nargs => 0
@doap function Paste()
    try
        clipboard()
    catch
        Null
    end
end

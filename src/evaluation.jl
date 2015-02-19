## This file contains mostly meval, infseval.

# Choose infinite or single evaluation.
# The test suite assumes infseval is used.
@inline doeval(x) = infseval(x)  # infinite evaluation
#@inline doeval(x) = meval(x)   # single evaluation

# Enable or disable hashing expressions here.
# We hash expressions so that only unique copies are stored.
# This slows code. It could be faster in some circumstances.
# Eg. if we continue to store the dependent variables of an
# expression as metadata, then we don't need to regenerate them,
# if we find the cached copy of an expression.
#
#lcheckhash(x) = checkhash(x)  # compute hash and look for cached copy of expr
@inline lcheckhash(x) = x      # do nothing

const MXDEBUGLEVEL = -1 # debug level, larger means more verbose. -1 is off

function get_localized_symbol(s::Symbol)
    return gensym(string(s))
end

# putting this in mxpr_type is better.
function ==(ax::Mxpr, bx::Mxpr)
    mhead(ax) != mhead(bx)  && return false
    a = margs(ax)
    b = margs(bx)
    (na,nb) = (length(a),length(b))
    na != nb && return false
    @inbounds for i in 1:na
        a[i] != b[i] && return false
    end
    true
end

## Macro for translation and evaluation, at repl or from file

# Data structure for monitoring evaluation
type Meval
    entrycount::Int  # For trace
    trace_ev_flag::Bool    # TraceOn()
    trace_upvalues_flag::Bool  # TrUpOn()
    trace_downvalues_flag::Bool  # TrDownOn()
    timingon::Bool   # TimeOn() does @time on every user input
    try_downvalue_count::Int
    try_upvalue_count::Int    
end
const MEVAL = Meval(0,false,false,false,false,0,0)

reset_meval_count() = MEVAL.entrycount = 0
get_meval_count() = MEVAL.entrycount
increment_meval_count() = MEVAL.entrycount += 1
decrement_meval_count() = MEVAL.entrycount -= 1

set_meval_trace() = MEVAL.trace_ev_flag = true
unset_meval_trace() = MEVAL.trace_ev_flag = false
is_meval_trace() = MEVAL.trace_ev_flag

set_down_trace() = MEVAL.trace_downvalues_flag = true
unset_down_trace() = MEVAL.trace_downvalues_flag = false
is_down_trace() = MEVAL.trace_downvalues_flag

set_up_trace() = MEVAL.trace_upvalues_flag = true
unset_up_trace() = MEVAL.trace_upvalues_flag = false
is_up_trace() = MEVAL.trace_upvalues_flag

set_timing() = MEVAL.timingon = true
unset_timing() = MEVAL.timingon = false
is_timing() = MEVAL.timingon

reset_try_downvalue_count() = MEVAL.try_downvalue_count = 0
reset_try_upvalue_count() = MEVAL.try_upvalue_count = 0
get_try_downvalue_count() = MEVAL.try_downvalue_count
get_try_upvalue_count() = MEVAL.try_upvalue_count
increment_try_downvalue_count() = MEVAL.try_downvalue_count += 1
increment_try_upvalue_count() = MEVAL.try_upvalue_count += 1

# Read a line of user input, translate Expr to Mxpr, but don't evaluate result
macro exnoeval(ex)
    mx = extomx(ex)
    :(($(esc(mx))))
end

# ex is called by the repl on each user input line.
macro ex(ex)
    check_doc_query(ex) && return nothing  # Asking for doc? Currently this is:  ?, SomeHead
    res = extomx(ex)  # Translate to Mxpr
    reset_meval_count()
    reset_try_downvalue_count()
    reset_try_upvalue_count()    
    if is_timing()
        mx = @time doeval(res) # doeval is calls either infseval or meval
        println("tryrule count: downvalue ", get_try_downvalue_count(),", upvalue ", get_try_upvalue_count())
    else
        mx = doeval(res)
    end
    if is_SJSym(mx) mx = getssym(mx) end # must do this otherwise Julia symbol is returned
    setsymval(:ans,mx)  # Like Julia and matlab, not Mma
    :(($(esc(mx))))  # Let the repl display the result
end

#################################################################################
#                                                                               #
#  infseval                                                                     #
#  repeats meval till we reach a fixed point                                    #
#                                                                               #
#################################################################################

# We use infinite or fixed point evaluation: the Mxpr is evaled repeatedly until it does
# not change. Actually Mma, and SJulia try to detect and avoid more evaluations.
# Also try to detect if the expression is simplified, (fixed or canonical).
# This is also complicated by infinite evaluation because whether an expression is
# simplified depends on the current environment. We try to solve this with lists of 'free' symbols.
# Note: lcheckhash is the identity (ie disabled)
# doeval is infseval: ie, we use 'infinite' evaluation. Evaluate till expression does not change.

# Diagnostic. Count number of exits from points in infseval
global const exitcounts = Int[0,0,0,0]

function infseval(mxin::Mxpr)
    @mdebug(2, "infseval ", mxin)
#    println("infseval ", mxin)
    neval = 0  # We cut off infinite eval at 100. Probably our bug, or bad user input.
    if checkdirtysyms(mxin) # is timestamp on any free symbol in mxin more recent than on mxin ?
#        println("got dirty syms $mxin")
        unsetfixed(mxin) # flag mxin as not being at its fixed point in this environment.
                         # we should check first if *any* user symbol has changed. this can be a single flag.
    end                  # This might be good for iterating over list of args in Mxpr.
    if is_fixed(mxin)  # If mxin was already fixed and none of its free vars changed, just return.
        # if is_Mxpr(mx) setage(mx) ; println("2 setting age of $mx") end
        exitcounts[1] += 1
#        println("1 Returning ckh $mxin")
        return lcheckhash(mxin)
    end
    mx = meval(mxin) # Do the first evaluation
    if is_Mxpr(mx)
        if is_fixed(mx)         # The first meval may have set the fixed point flag. 
            exitcounts[2] += 1  # Eg, an Mxpr with only numbers, does not need another eval.
#            println("2 Returning ckh $mx")
            return lcheckhash(mx)  # Only a few exits here
        elseif mx == mxin  # meval did not set fixed flag, but we see that it is at fixed point.
            setfixed(mx)    # They may be equal but we need to set fixed bit in mx !
            setfixed(mxin)  # Do we need to do this to both ?
            exitcounts[3] += 1
#            println("3 Returning ckh $mx")
            return lcheckhash(mx)
        end
    end
    local mx1    
    while true # After 1 eval fixed bit is not set and input not equal to result of first eval
        mx1 = meval(mx)  # So, we do another eval.
        if (is_Mxpr(mx1) && is_fixed(mx1))  || mx1 == mx  # The most recent eval was enough, we are done
            #  setfixed(mx) # Not correct. Why ?
            mx = mx1        
            break
        end
        neval += 1
        if neval > 2
            println(mx)
            println(meval(mx))
            error("infseval: Too many, $neval, evaluations. Expression still changing")
        end
        mx = mx1        
    end
    if is_Mxpr(mx) && mx == mxin
        setfixed(mxin)
        setfixed(mx)
    end
    exitcounts[4] += 1
#    println("4 Returning ckh $mx")
    return lcheckhash(mx)  # checking hash code is disbled.
end
function infseval(s::SJSym)
#    infseval(s,getssym(s))
    mx = meval(s)
    return mx == s ? s : infseval(mx)
end
# Any type that other than SJSym (ie Symbol) or Mxpr is not meval'd.
@inline infseval(x) = x
@inline infseval(x::Complex) = x.im == 0 ? x.re : x
infseval(x::Rational) = x.den == 1 ? x.num : x

# This stuff is maybe a bit more efficient ? But it breaks abstraction.
# never called
# infseval{T<:Number}(s::SSJSym{T}) = (println("ssjsym") ; symval(s))
# function infseval(s::SJSym, ss::SSJSym)
#     return s == symval(ss) ? s : infseval(symval(ss))
# end
# function infseval{T<:Number}(s::SJSym, ss::SSJSym{T})
#     return symval(ss)
# end


#################################################################################
#                                                                               #
#  meval  Evaluation of Mxpr                                                    #
#  main evaluation routine. Call doeval on head                                 #
#  and some of the arguments. Then apply rules and other things on the result.  #
#                                                                               #
#################################################################################

# These are normally not called, but rather are caught by infseval.
@inline meval(x::Complex) = x.im == 0 ? x.re : x  # probably never called because
meval(x::Rational) = x.den == 1 ? x.num : x
@inline meval(x) = x

@inline meval(s::SJSym) = symval(s) # this is where var subst happens
function meval(mx::Mxpr)
#    println("$mx")
    increment_meval_count()
    if get_meval_count() > 200
        error("Too many meval entries ", get_meval_count())
    end
    local ind = ""  # some places get complaint that its not defined. other places no !?
    if is_meval_trace()
        ind = " " ^ get_meval_count()
        println(ind,"<<", get_meval_count(), " " , mx)
    end
    nhead = doeval(mhead(mx))
    local nargs
    mxargs = margs(mx)
    len = length(mxargs)
    if get_attribute(nhead,:HoldFirst)
        nargs = newargs(len)
        nargs[1] = mxargs[1]
        @inbounds for i in 2:length(mxargs)
            nargs[i] = doeval(mxargs[i])
        end
    elseif get_attribute(nhead,:HoldAll)
        nargs = copy(mxargs) # need to copy these, I think!
    elseif get_attribute(nhead,:HoldRest)
        nargs = copy(mxargs) # need to copy these, I think!
        nargs[1] = doeval(nargs[1])
    else  # Evaluate all arguments
        nargs = newargs(len)
        @inbounds for i in 1:len
            nargs[i] = doeval(mxargs[i])
        end
    end
    ! (get_attribute(nhead, :SequenceHold) || get_attribute(nhead, :HoldAllComplete)) ?
    splice_sequences!(nargs) : nothing
#    println("nmx $nhead, $nargs")    
    nmx = mxpr(nhead,nargs)   # new expression with evaled args
#    println("nmx $nmx")    
    setfreesyms(nmx,revisesyms(mx)) # set free symbol list in nmx
    if  ! is_canon(nmx)
        nmx = flatten!(nmx)
        if get_attribute(nmx,:Listable)  nmx = threadlistable(nmx) end        
        nmx = canonexpr!(nmx)
#        if is_Mxpr(nmx,:Plus) setfixed(nmx) end  This cannot be done, in general
    end
    res = apprules(nmx)
    if res == nothing
        is_meval_trace()  && println(ind,">> " , res)
        decrement_meval_count()  # decrement at every exit point
        return nothing
    end
    res = ev_upvalues(res)    
    res = ev_downvalues(res)
    merge_args_if_emtpy_syms(res)
    is_meval_trace() && println(ind,get_meval_count(), ">> ", res)
    decrement_meval_count()
    return res
end

# Similar to checkdirtysyms. The original input Mxpr had a list of free symbols.
# That input has been mevaled at least once and the result is mx, the argument
# to revisesyms. Here, we make a free-symbol list for mx. We look at its current
# free symbol list, which is inherited, and identify those that are no longer
# free. Eg. the environment changed. E.g The user set a = 1. Or 'a' may
# evaluate to an expression with other symbols.
#
# move this to mxpr_type
function revisesyms(mx::Mxpr)
    s = getfreesyms(mx)
#    println("revising $mx:  $s")
    mxage = getage(mx)
    nochange = true     # Don't create a new symbol list if nothing changed
    for sym in keys(s)  # Check if changes. Does this save or waste time ?
        if symage(sym) > mxage
            nochange = false
            break
        end
    end
    # Need to return a copy, or Table(x^i + x*i + 1,[i,10]) shows a bug.
    nochange == true && return copy(s)
    nsyms = newsymsdict()
    for sym in keys(s)
 #        mergesyms(nsyms,symval(sym))
        if symage(sym) > mxage
            mergesyms(nsyms,symval(sym))
        else
            mergesyms(nsyms,sym)  # just copying from the old symbol list
        end
    end
    return nsyms
end

## Try applying downvalues

@inline function ev_downvalues(res::Mxpr)
    if length(downvalues(mhead(res))) != 0  res = applydownvalues(res)  end
    return res
end
@inline ev_downvalues(x) = x

## Applying upvalues. This has to be efficient, we must not iterate over args.
#  Instead, we check free-symbol list.

@inline function ev_upvalues(res::Mxpr)
    merge_args_if_emtpy_syms(res) # do upvalues are for free symbols in res.
    for s in listsyms(res)
        if has_upvalues(s)
            res = applyupvalues(res,s)
            break # I think we are supposed to only apply one rule
        end
    end
    return res
end
@inline ev_upvalues(x) = x


## Build list of free syms in Mxpr if list is empty.
@inline function merge_args_if_emtpy_syms(res::Mxpr)
    if isempty(getfreesyms(res)) # get free symbol lists from arguments
#        println("Merging in meval $res")
        mergeargs(res) # This is costly if it is not already done.
        add_nothing_if_no_syms(res)  # if there no symbols, add :nothing, so this is not called again.
    end
    
end
@inline merge_args_if_emtpy_syms(res) = nothing


## Thread Listable over Lists
# If any arguments to mx are lists, thread over them. If there are
# more than one list, they must be of the same length.  Eg.
# f([a,b,c],d) -> [f(a,d),f(b,d),f(c,d)].
#
# This is general, but is not the most efficient way. Some Specific
# cases, or classes should be handled separately. Eg Adding two large
# lists of numbers, is slow this way. The problem is that
# threadlistable is called early in the evaluation sequence, as per
# Mma. So we can't add lists of numbers at that point.

function threadlistable(mx::Mxpr)
    pos = Array(Int,0) # should avoid this
    lenmx = length(mx)
    lenlist::Int = -1
    h = mhead(mx)
    @inbounds for i in 1:lenmx
        if is_Mxpr(mx[i],:List)
            nlen = length(mx[i])
            if lenlist >= 0 && nlen != lenlist
                error("Can't thread over lists of different lengths.")
            end
            lenlist = nlen
            push!(pos,i)
        end
    end
    lenp = length(pos)
    lenp == 0 && return mx   # Nothing to do. return input array
    largs = newargs(lenlist)
    @inbounds for i in 1:lenlist
        nargs = newargs(lenmx)
        p = 1
        @inbounds for j in 1:lenmx
            if p <= lenp && pos[p] == j
                nargs[j] = mx[j][i]
                p += 1
            else
                nargs[j] = mx[j]
            end
        end
        largs[i] = mxpr(h,nargs)
    end
    nmx = mxpr(:List,largs)
    return nmx
end

## Splice expressions with head Sequence into argument list

# f(a,b,Sequence(c,d),e,f) -> f(a,b,c,d,e,f)
# args are args of an Mxpr
function splice_sequences!(args)
    length(args) == 0 && return
    i::Int = 1
    while true
        if is_Mxpr(args[i], :Sequence)
            sargs = margs(args[i])
            splice!(args,i,sargs)
            i += length(sargs) # skip over new arguments. 
        end                      # splicing in lower levels should be done already.
        i += 1
        i > length(args) && break
    end
end

function set_pattributes{T<:AbstractString}(syms::Array{T,1},attrs::Array{Symbol,1})
    for s in syms
        for a in attrs
            set_attribute(symbol(s),a)
        end
        set_attribute(symbol(s),:Protected)  # They all are Protected
    end
end

function mkapprule(head::String)
    s1 = ":" * head
    s2 = "do_" * head
    a1 = "apprules(mx::Mxpr{$s1}) = $s2(mx,margs(mx)...)"
    a2 = "$s2(mx::Mxpr{$s1},args...) = mx"
#    println(a1)
    #    println(a2)
    set_pattributes([head],[:Protected])
    eval(parse(a1))
    eval(parse(a2))
end

apprules(mx::Mxpr{GenHead}) = do_GenHead(mx, mhead(mx))
do_GenHead(mx,h) = mx

# Head is a Julia function. Apply it to the arguments
function do_GenHead(mx,f::Function)
    f(margs(mx)...)
end

# This feature was added to Mma sometime after 3.0
# Assume operator version of an SJulia "function". Eg, Map
# Map(q)([1,2,3])
# But, not all functions use the first operator. Eg for MatchQ it is
# the second
function do_GenHead(mx,head::Mxpr)
    mxpr(mhead(head),margs(head)...,copy(margs(mx))...)
end

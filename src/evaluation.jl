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
lcheckhash(x) = x      # do nothing

const MXDEBUGLEVEL = -1 # debug level, larger means more verbose. -1 is off

# We should generate our own
function get_localized_symbol(s::Symbol)
    gsym = gensym(string(s))
    set_attribute(gsym, :Temporary)
    return gsym
end

@mkapprule ClearTemporary  :nargs => 0

@sjdoc ClearTemporary "
Remove temporary symbols, ie all beginnig with \"##\", from the symbol table.
"

# The Temporary attribute is not working. the symbols that escape are just gensysms
@doap function ClearTemporary()
    syms = usersymbols()
    for sym in syms
        ss = string(sym)
        if length(ss) > 2 && ss[1:2] == "##"
            delete_sym(Symbol(sym))
        end
    end
    Null
end

## Macro for translation and evaluation, at repl or from file


# Read a line of user input, translate Expr to Mxpr, but don't evaluate result
macro exnoeval(ex)
    mx = extomx(ex)
    :(($(esc(mx))))
end

# This is called from the REPL
macro ex(ex)   # use this macro from the julia prompt
    mx = exfunc(ex)
    :(($(esc(mx))))
end


const number_of_Os = 10

const Os = Array(SJSym,0)
for i in 1:number_of_Os
    push!(Os, Symbol("O"^i))
end

# Put this in the System symbol table
# so that they are imported into Global
for i in 1:number_of_Os
    set_system_symval(Os[i], Null)
end


# Bind O to O(n), where n is the most recent line number.
# Bind OO to O(n-1), etc.  When O evaluates to Out(n), the Out rule evaluate
# the expression bound to Out(n).
macro bind_Os()
    expr = Expr(:block)
    for i in 1:number_of_Os
        sym =  string(Os[i])
        newex = :(
                  if (length(Output) - $i + 1) >= 1
                       oexp = mxpr(:Out, get_line_number() - $i + 1)
                       set_system_symval(parse($sym), oexp)
                       set_pattributes($sym, :Protected)
                  end 
                     )
        push!(expr.args, newex)
    end
    expr
end


function exfunc(ex)
    check_doc_query(ex) && return nothing  # Asking for doc? Currently this is:  ?, SomeHead
    res = extomx(ex)  # Translate to Mxpr
    reset_meval_count()
    reset_try_downvalue_count()
    reset_try_upvalue_count()
    if is_timing()
        @time mx = tryexfunc(res)
        println("tryrule count: downvalue ", get_try_downvalue_count(),", upvalue ", get_try_upvalue_count())
    else
        mx = tryexfunc(res)
    end
    if is_SJSym(mx) mx = getssym(mx) end # must do this otherwise Julia symbol is returned
    set_system_symval(:ans,mx)  # Like Julia and matlab, not Mma
    increment_line_number()
    if isinteractive()
        set_sjulia_prompt(get_line_number() + 1)
    end
    push_output(mx)
    @bind_Os
    symval(mx) == Null  && return nothing
    if isinteractive()    #  we don't need this at the moment ->   && do_we_print_outstring    
        print("Out(" * string(get_line_number()) * ") = ")
    end
    mx
end

function tryexfunc(mxin)
    try
        doeval(mxin)
    catch e
        if isa(e,ArgCheckErr)
            warn(e.msg)
            return mxin
        elseif isa(e,RecursionLimitError)
            warn(e.msg)
            e.mx
        else
            rethrow(e)
        end
    end
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

# We should make this user settable somehow
recursion_limit() =  1024

# Diagnostic. Count number of exits from points in infseval
global const exitcounts = Int[0,0,0,0]

function infseval(mxin::Mxpr)
    @mdebug(2, "infseval ", mxin)
    neval = 0
    if checkdirtysyms(mxin) # is timestamp on any free symbol in mxin more recent than timestamp on mxin ?
        unsetfixed(mxin)    # Flag mxin as not being at its fixed point in this environment.
    end                     # This might be good for iterating over list of args in Mxpr.
    if is_fixed(mxin)       # If mxin was already fixed and none of its free vars changed, just return.
        return lcheckhash(mxin)
    end
    mx = meval(mxin)        # Do the first evaluation
    if is_Mxpr(mx)
        if is_fixed(mx)     # The first meval may have set the fixed point flag. Eg, an Mxpr with only numbers, does not need another eval.
            return lcheckhash(mx)  # Only a few exits counted here
        elseif mx == mxin   # meval did not set fixed flag, but we see that it is at fixed point.
            setfixed(mx)    # They may be equal but we need to set fixed bit in mx !
            setfixed(mxin)  # Do we need to do this to both ?
            return lcheckhash(mx)
        end
    end
    local mx1
    while true              # After 1 eval fixed bit is not set and input not equal to result of first eval
        mx1 = meval(mx)     # So, we do another eval.
        if (is_Mxpr(mx1) && is_fixed(mx1))  || mx1 == mx  # The most recent eval was enough, we are done
            mx = mx1
            break
        end
        neval += 1
        if neval > 2
            setfixed(mx)
            throw(RecursionLimitError("infseval: Too many, $neval, evaluations. Expression still changing", mxprcf(:Hold,mx)))
        end
        mx = mx1
    end
    if is_Mxpr(mx) && mx == mxin
        setfixed(mxin)
        setfixed(mx)
    end
    return lcheckhash(mx)  # checking hash code is disbled.
end

function infseval(qs::Qsym)
    mx = meval(qs)
    return mx == qs ? qs : infseval(mx)
end

function infseval(s::SJSym)
    mx = meval(s)
    return mx == s ? s : infseval(mx)
end
# Any type that other than SJSym (ie Symbol) or Mxpr is not meval'd.
@inline infseval(x) = x
@inline infseval{T<:Real}(x::Complex{T}) = x.im == zero(x.im) ? x.re : x

infseval{T<:Integer}(x::Complex{Rational{T}}) = meval(x)
infseval{T<:Integer}(x::Rational{T}) = meval(x)

meval(x::Float64) = x == Inf ? Infinity : x == -Inf ? MinusInfinity : x

#################################################################################
#                                                                               #
#  meval  Evaluation of Mxpr                                                    #
#  main evaluation routine. Call doeval on head                                 #
#  and some of the arguments. Then apply rules and other things on the result.  #
#                                                                               #
#################################################################################

# These are normally not called, but rather are caught by infseval.
@inline meval{T<:Real}(x::Complex{T}) = x.im == 0 ? x.re : x

meval{T<:Integer}(x::Rational{T}) = x.den == 1 ? x.num : x.den == 0 ? ComplexInfinity : x

meval{T<:Void}(x::T) = Null

meval(x) = x

meval(s::SJSym) = symval(s) # this is where var subst happens

function meval(qs::Qsym)
#    println("meval: Qsym ", qs)
    res = symval(qs)
#    println("meval: Qsym got res ", res)
    res
end

function meval(mx::Mxpr)
    increment_meval_count()
    if get_meval_count() > recursion_limit()
        setfixed(mx)
        res = mxprcf(:Hold,mx)
        throw(RecursionLimitError("Recursion depth of " * string(recursion_limit()) *  " exceeded.", res))
    end
    local ind::ByteString = ""  # some places get complaint that its not defined. other places no !?
    if is_meval_trace()
        ind = " " ^ (get_meval_count() - 1)
        println(ind,">>", get_meval_count(), " " , mx)
    end
    nmx::Mxpr = meval_arguments(mx)
    @mdebug(2, "meval: done meval_args ", nmx)
    setfreesyms(nmx,revisesyms(mx)) # set free symbol list in nmx
    @mdebug(2, "meval: done setfreesyms ")
    res = meval_apply_all_rules(nmx)
    @mdebug(2, "meval: done meval_apply_all_rules ", res)
    is_meval_trace() && println(ind,get_meval_count(), "<< ", res)
    decrement_meval_count()
    @mdebug(2, "meval: returning ", res)    
    return res
end

function meval_apply_all_rules(nmx::Mxpr)
    if  ! is_canon(nmx)
        if get_attribute(nmx,:Flat) nmx = flatten!(nmx) end
        if get_attribute(nmx,:Listable)  nmx = threadlistable(nmx) end
        res = canonexpr!(nmx)
    end
    @mdebug(2, "meval_apply_all_rules: entering apprules: ", res)
    res = apprules(res)           # apply "builtin" rules
    @mdebug(2, "meval_apply_all_rules: exited apprules ", res)
    is_Mxpr(res) || return res
    @mdebug(2, "meval_apply_all_rules: entering ev_upvalues ", res)        
    res = ev_upvalues(res)
    @mdebug(2, "meval_apply_all_rules: exited ev_upvalues ", res)   
    res = ev_downvalues(res)
    @mdebug(2, "meval_apply_all_rules: entering merge_args_if_empty_syms")        
    merge_args_if_emtpy_syms(res) # merge free symbol lists from arguments
    @mdebug(2, "meval_apply_all_rules: exiting")    
    return res
end

# Evaluate arguments of mx, construct and return new Mxpr
function meval_arguments(mx::Mxpr)
    nhead = doeval(mhead(mx))
    local nargs::MxprArgs
    mxargs::MxprArgs = margs(mx)
    len::Int = length(mxargs)
    if len == 0
        nargs = newargs(0)
    elseif get_attribute(nhead,:HoldFirst)
        nargs = newargs(len)
        nargs[1] = mxargs[1]
        @inbounds for i in 2:length(mxargs)
            nargs[i] = doeval(mxargs[i])
        end
    elseif get_attribute(nhead,:HoldAll)
        nargs = copy(mxargs)
    elseif get_attribute(nhead,:HoldRest)
        nargs = copy(mxargs)
        nargs[1] = doeval(nargs[1])
    else      # Evaluate all arguments
        nargs = newargs(len)
        @inbounds for i in 1:len
            nargs[i] = doeval(mxargs[i])
        end
    end
    ! (get_attribute(nhead, :SequenceHold) || get_attribute(nhead, :HoldAllComplete)) ?
       splice_sequences!(nargs) : nothing
    nmx::Mxpr = mxpr(nhead,nargs)   # new expression with evaled args
    return nmx
end

# Similar to checkdirtysyms. The original input Mxpr had a list of free symbols.
# That input has been mevaled at least once and the result is mx, the argument
# to revisesyms. Here, we make a free-symbol list for mx. We look at its current
# free symbol list, which is inherited, and identify those that are no longer
# free. Eg. the environment changed. E.g The user set a = 1. Or 'a' may
# evaluate to an expression with other symbols.
#
# move this to mxpr_type ??
function revisesyms(mx::Mxpr)
    s::FreeSyms = getfreesyms(mx)
    mxage::UInt64 = getage(mx)
    nochange::Bool = true      # Don't create a new symbol list if nothing changed
    for sym in keys(s)         # Check if changes. Does this save or waste time ?
        if symage(sym) > mxage
            nochange = false
            break
        end
    end
    # Need to return a copy here, or Table(x^i + x*i + 1,[i,10]) shows a bug.
    nochange == true && return copy(s)
    nsyms::FreeSyms = newsymsdict()
    for sym in keys(s)
        if symage(sym) > mxage
            mergesyms(nsyms,symval(sym))
        else
            mergesyms(nsyms,sym)  # just copying from the old symbol list
        end
    end
    return nsyms
end

## Try applying downvalues

@inline function ev_downvalues(mx::Mxpr)
    if has_downvalues(mx)
        return applydownvalues(mx)
    else
        return mx
    end
end
@inline ev_downvalues(x) = x

## Applying upvalues. This has to be efficient, we must not iterate over args.
#  Instead, we check free-symbol list.

@inline function ev_upvalues(mx::Mxpr)
    merge_args_if_emtpy_syms(mx) # do upvalues are for free symbols in mx.
    for s in listsyms(mx)
        if has_upvalues(s)
            mx = applyupvalues(mx,s)
            break      # I think we are supposed to only apply one rule
        end
    end
    return mx
end
@inline ev_upvalues(x) = x

## Build list of free syms in Mxpr if list is empty.
@inline function merge_args_if_emtpy_syms(mx::Mxpr)
    if isempty(getfreesyms(mx))    # get free symbol lists from arguments
        mergeargs(mx)              # This is costly if it is not already done.
        add_nothing_if_no_syms(mx) # If there are no symbols, add :nothing, so this is not called again.
    end
end
@inline merge_args_if_emtpy_syms(x) = nothing


#### Thread Listable over Lists

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
    pos = Array(Int,0)      # should avoid this
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
    lenp == 0 && return mx      # Nothing to do. return input array
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

#### Splice expressions with head Sequence into argument list

## f(a,b,Sequence(c,d),e,f) -> f(a,b,c,d,e,f)
## args are args of an Mxpr
function splice_sequences!(args)
    length(args) == 0 && return
    i::Int = 1
    while true
        if is_Mxpr(args[i], :Sequence)
            sargs = margs(args[i])
            splice!(args,i,sargs)
            i += length(sargs)   # skip over new arguments.
        end                      # splicing in lower levels should be done already.
        i += 1
        i > length(args) && break
    end
end


apprules(mx::Mxpr{GenHead}) = do_GenHead(mx, mhead(mx))
do_GenHead(mx,h) = mx

# Head is a Julia function. Apply it to the arguments
do_GenHead{T<:Function}(mx,f::T) = f(margs(mx)...)

#### Currying

# This feature was added to Mma sometime after 3.0: (actually, in 2014)
# Assume operator version of an SJulia "function". Eg, Map
# Map(q)([1,2,3])
# But, not all functions use the first operator. Eg for MatchQ it is
# the second.
function do_GenHead(mx,head::Mxpr)
    mxpr(mhead(head),margs(head)...,copy(margs(mx))...)
end

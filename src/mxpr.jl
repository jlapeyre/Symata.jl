## This file contains mostly meval, loopmeval, and code to translate Expr from cli to Mxpr.

# Choose infinite or single evaluation.
# The test suite assumes loopmeval is used.
@inline doeval(x) = loopmeval(x)  # infinite evaluation
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

## Translate Expr to Mxpr

extomx(x) = x
function extomx(s::Symbol)
    ss = string(s)
    if contains(ss,"_")  # Blanks used in patterns
        return parseblank(ss)
    else
        return getsym(jtomsym(s)) # Maybe translate the symbol
    end
end

# Underscore is not allowed in symbols. Instead,
# they signify part of a pattern. This follows Mma,
# and we don't consume and Julia syntax to signify patterns.
# We don't yet parse three blanks in a row.
function parseblank(s::String)
    a = split(s,['_'], keep=true)
    length(a) > 3 && error("parseblank: Illegal Pattern expression '$s'")
    if length(a) == 2
        blanktype = :Blank
        (blankhead,blankname) = (a[1],a[2])
    else
        a[2] != "" && error("parseblank: Illegal Pattern expression '$s'")
        blanktype = :BlankSequence
        (blankhead,blankname) = (a[1],a[3])
    end
    if length(blankname) == 0
        blank = mxpr(blanktype)
    else
        blank = mxpr(blanktype,symbol(blankname))
    end
    length(blankhead) == 0 && return blank
    mxpr(:Pattern,symbol(blankhead),blank)
end

function extomxarr(ain,aout)
    for x in ain
        push!(aout,extomx(x))
    end
end

## Main translation routine
# We use Julia for lexing/parsing. But we change the semantics:
# sometimes a little, sometimes a lot.
function extomx(ex::Expr)
    newa = newargs()
    local head::Symbol
    ex = rewrite_expr(ex)
    a = ex.args
    # We usually set the head and args in the conditional and construct Mxpr at the end
    if ex.head == :call
        head = jtomsym(a[1])
        for i in 2:length(a) push!(newa,extomx(a[i])) end
    elseif ex.head == :block
        mx = extomx(a[2]) # Can't remember, I think this is Expr with head :call
        return mx
    elseif haskey(JTOMSYM,ex.head)
        head = JTOMSYM[ex.head]
        extomxarr(a,newa)
    elseif ex.head == :kw  # Interpret keword as Set, but Expr is different than when ex.head == :(=)
        head = :Set
        extomxarr(a,newa)        
    elseif ex.head == :(:) # Eg the colon here: g(x_Integer:?(EvenQ)) := x
        if length(a) == 2
            if is_type(a[1], Symbol) && is_type(a[2], Expr) &&
                (a[2].args)[1] == :(?)
                ptargs = a[2].args
                length(ptargs) != 2 && error("extomx: too many args to PatternTest")
                is_type(ptargs[2],Symbol) || error("extomx: argument to PatternTest must be a Symbol")
                head = :PatternTest
                push!(newa,extomx(a[1]),getsym(ptargs[2]))
            else
                # something here later
                error("extomx: No translation for $ex")
            end
        else
            error("extomx: No translation for $ex")
        end
    elseif ex.head == :quote  # Quotes are wrapped in Jxpr which is evaluated by Julia eval()
        head = :Jxpr          # This allows running Julia code from within SJulia.
        push!(newa,eval(ex))
    else        
        dump(ex)
        error("extomx: No translation for Expr head '$(ex.head)' in $ex")
    end
    mx = mxpr(head,newa)  # Create the Mxpr
    return mx
end

is_call(ex::Expr) = ex.head == :call
# is ex a call with operator op ?
is_call(ex::Expr, op::Symbol) = ex.head == :call && ex.args[1] == op
# is ex a call with operator op and len args (including the op) ?
is_call(ex::Expr, op::Symbol, len::Int) = ex.head == :call && ex.args[1] == op && length(ex.args) == len
# is ex a call with len args (including the op) ?
is_call(ex::Expr, len::Int) = is_call(ex) && length(ex.args) == len

# We check for :call repeatedly. We can optimize this later.
is_binary_minus(ex::Expr) = is_call(ex, :-, 3)
# number of args != 3 will pass through. But probably can't be used
is_division(ex::Expr) = is_call(ex, :/,3)  
is_power(ex::Expr) = is_call(ex, :^)

# In extomx, we first rewrite some Math to canonical forms
# a - b  -->  a + (-b)
rewrite_binary_minus(ex::Expr) = Expr(:call, :+, ex.args[2], Expr(:call,:(-),ex.args[3]))
# a / b -->  a * b^(-1)
rewrite_division(ex::Expr) = Expr(:call, :*, ex.args[2], Expr(:call,:^,ex.args[3],-1))

# Not used
#rewrite_binary_minus(mx::Mxpr) = mxpr(:+, mx[1], mxpr(:(-),mx[2]))
#rewrite_division(mx::Mxpr) = mxpr(:+, mx[1], mxpr(:^,mx[2],-1))

# There is no binary minus, no division, and no sqrt in Mxpr's.
# Concrete example: a - b --> a + -b.
# We definitely need to dispatch on a hash query, or types somehow
# Other rewrites needed, but not done.
function rewrite_expr(ex::Expr)
    if is_binary_minus(ex)  #  a - b --> a + -b.
        ex = rewrite_binary_minus(ex)
    elseif is_division(ex) # a / b --> a + b^(-1)
        ex = rewrite_division(ex)
    elseif is_call(ex, :Exp, 2)  # Exp(x) --> E^x
        ex = Expr(:call, :^, :E, ex.args[2])
#    elseif is_call(ex,:Sqrt,2)  TODO
#        ex = Expr(:call, :^, ex.args[2], SJRational(1//2))
#    end
    end
    return ex
end

## Macro for translation and evaluation, at repl or from file

type Meval
    entrycount::Int  # For trace
    traceon::Bool    # TraceOn()
    timingon::Bool   # TimeOn() does @time on every user input
end
const MEVAL = Meval(0,false,false)

reset_meval_count() = MEVAL.entrycount = 0
get_meval_count() = MEVAL.entrycount
increment_meval_count() = MEVAL.entrycount += 1
decrement_meval_count() = MEVAL.entrycount -= 1
set_meval_trace() = MEVAL.traceon = true
unset_meval_trace() = MEVAL.traceon = false
is_meval_trace() = MEVAL.traceon

# Read a line of user input, translate Expr to Mxpr, but don't evaluate result
macro exnoeval(ex)
    mx = extomx(ex)
    :(($(esc(mx))))
end

# ex is called by the repl on each user input line.
macro ex(ex)
    check_help_query(ex) && return nothing  # Asking for doc? Currently this is:  ?, SomeHead
    res = extomx(ex)  # Translate to Mxpr
    reset_meval_count()
    if MEVAL.timingon
        mx = @time doeval(res) # doeval just calls loopeval. But we can change it to get single eval.
    else
        mx = doeval(res)
    end
    if is_SJSym(mx) mx = getssym(mx) end # must do this otherwise Julia symbol is returned
    setsymval(:ans,mx)  # Like Julia and matlab, not Mma
    :(($(esc(mx))))  # Let the repl display the result
end

# Diagnostic. Count number of exits from points in loopeval
global const exitcounts = Int[0,0,0,0]

# We use infinite or fixed point evaluation: the Mxpr is evaled repeatedly until it does
# not change. Actually Mma, and SJulia try to detect and avoid more evaluations.
# Also try to detect if the expression is simplified, (fixed or canonical).
# This is also complicated by infinite evaluation because whether an expression is
# simplified depends on the current environment. We try to solve this with lists of 'free' symbols.
# Note: lcheckhash is the identity (ie disabled)
# doeval is loopmeval: ie, we use 'infinite' evaluation. Evaluate till expression does not change.
function loopmeval(mxin::Mxpr)
    @mdebug(2, "loopmeval ", mxin)
#    println("loopmeval ", mxin)
    neval = 0  # We cut off infinite eval at 100. This would probably only be a bug in SJulia.
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
        if neval > 100
            println(mx)
            error("loopmeval: Too many, $neval, evaluations. Expression still changing")
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

# This stuff is maybe a bit more efficient ? But it breaks abstraction.
# never called
# loopmeval{T<:Number}(s::SSJSym{T}) = (println("ssjsym") ; symval(s))

# function loopmeval(s::SJSym, ss::SSJSym)
#     return s == symval(ss) ? s : loopmeval(symval(ss))
# end

# function loopmeval{T<:Number}(s::SJSym, ss::SSJSym{T})
#     return symval(ss)
# end

function loopmeval(s::SJSym)
#    loopmeval(s,getssym(s))
    mx = meval(s)
    return mx == s ? s : loopmeval(mx)
end

# Any type that other than SJSym (ie Symbol) or Mxpr is not meval'd.
loopmeval(x) = x

## Evaluation of Mxpr

meval(x) = x
meval(s::SJSym) = symval(s)

# Similar to checkdirtysyms. The original input Mxpr had a list of free symbols.
# That input has been mevaled at least once and the result is mx, the argument
# to revisesyms. Here, we make a free-symbol list for mx. We look at its current
# free symbol list, which is inherited, and identify those that are no longer
# free. Eg. the environment changed. E.g The user set a = 1. Or 'a' may
# evaluate to an expression with other symbols.
#
# move this to mxpr_type
function revisesyms(mx::Mxpr)
    s = mx.syms
#    println("revising $mx:  $s")
    mxage = getage(mx)
    nochange = true     # Don't create a new symbol list if nothing changed
    for sym in keys(s)  # Don't know if this saves or wastes time ?
        if symage(sym) > mxage
            nochange = false
            break
        end
    end
    nochange == true && return s
    nsyms = newsymsdict()
    for sym in keys(s)  
 #        mergesyms(nsyms,symval(sym))
        if symage(sym) > mxage
#            println("Merging Changed $sym")
            mergesyms(nsyms,symval(sym))
        else
#            println("Merged unchanged $sym")
            mergesyms(nsyms,sym)  # just copying from the old symbol list
        end
    end
    return nsyms
end

function meval(mx::Mxpr)
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
#    println("1. meval $mx: ", listsyms(mx))
    mxargs = mx.args
    len = length(mxargs)
    if get_attribute(mx.head,:HoldFirst)
        nargs = newargs(len)
        nargs[1] = mxargs[1]
        for i in 2:length(mxargs)
            nargs[i] = doeval(mxargs[i])
#            println("HoldFirst loop $i ",nargs[i], " : ", listsyms(nargs[i])) 
        end
#        println("HoldFirst Leaving")
    elseif get_attribute(mhead(mx),:HoldAll)
#        println("************************************** HoldAll")
        nargs = mxargs
#        println("HoldAll Leaving")
    elseif get_attribute(mhead(mx),:HoldRest)
        nargs = mxargs
        nargs[1] = doeval(nargs[1])
#        println("HoldRest Leaving")        
    else
#        changeflag = false  
#         for i in 1:length(mxargs)  # need to see if this code is worth anything. It breaks somes things.
# #            println("Checking change in ", mxargs[i], " in expr ",mx)
#             if mxargs[i] != doeval(mxargs[i])
#                 changeflag = true
#                 break
#             end
#         end
        changeflag = true
        if changeflag
            nargs = newargs(len)
            for i in 1:len
                res1 = doeval(mxargs[i])
#                println(" *** Meval inner $res1")
                nargs[i] = res1
            end

        else
            nargs = mxargs
        end
#        println("NoHold Leaving")        
    end
    nmx = mxpr(nhead,nargs)
    # Need following, but there is probably a better way to do this.
    nmx.syms = revisesyms(mx)
#    mergeargs(nmx)
#    nmx.syms = mx.syms # This is wrong, too. Dependent symbols can change.
    if get_attribute(nmx,:Listable)  nmx = threadlistable(nmx) end
    # We apply the rules before doing the ordering. This differs from Mma.
    res = apprules(nmx)
    if res == nothing
        is_meval_trace()  && println(ind,">> " , res)
        return nothing
    end
    if  ! is_canon(res)
#        println("Entering fl ca $res: ", listsyms(res))
        res = flatten!(res)
        res = canonexpr!(res)
#        println("Exiting fl ca $res: ", listsyms(res))
    end
    # The conditional probably saves little time
    if is_Mxpr(res) && length(downvalues(res.head)) != 0  res = applydownvalues(res)  end
    is_meval_trace() && println(ind,get_meval_count(), ">> ", res)
    decrement_meval_count()
    if is_Mxpr(res)  && isempty(res.syms)
#        clearsyms(res)  Clearing is bad.
        for i in 1:length(res)  # This is costly if it is not already done.
            mergesyms(res,res[i])
        end
        add_nothing_if_no_syms(res)
    end
#    println("Exiting meval: $res: ", listsyms(res))
    return res
end

function threadlistable(mx::Mxpr)
    pos = Array(Int,0) # should avoid this
    lenmx = length(mx)
    lenlist = -1
    h = mhead(mx)
    for i in 1:lenmx
        if is_Mxpr(mx[i],:List)
            nlen = length(mx[i])
            if lenlist >= 0 && nlen != lenlist
                error("Trying to thread over lists of different lengths.")
            end
            lenlist = nlen
            push!(pos,i)
        end
    end
    lenp = length(pos)
    lenp == 0 && return mx
    largs = newargs(lenlist)
#    println("lenp=$lenp, lenlist=$lenlist")
    for i in 1:lenlist
        nargs = newargs(lenmx)
        p = 1
        for j in 1:lenmx
            if p <= lenp && pos[p] == j
#                println("got pos $p, at j=$j")
                nargs[j] = mx[j][i]
#                println("and set")
                p += 1
            else
 #               println("setting scalar")                
                nargs[j] = mx[j]
#                println("set scalar")
            end
        end
#        println("Pushing list $i")
        largs[i] = mxpr(h,nargs)
    end
    nmx = mxpr(:List,largs)
#    println("$largs")    
#    println("Done")
    return nmx
end

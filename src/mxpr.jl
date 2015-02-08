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

## Not the right place for these
Base.base(p::Mxpr{:Power}) = margs(p,1)
expt(p::Mxpr{:Power}) = margs(p,2)

function get_attributes(sj::SJSym)
    ks = sort!(collect(keys(symattr(sj))))
    mxpr(:List,ks...) # need to splat because array ks is not of type Any
end

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

# Wht is this doing here!!
#getindex(mx::Mxpr, k::Int) = return k == 0 ? mx.head : mx.args[k]
Base.length(mx::Mxpr) = length(mx.args)

## Mxpr Display

# Julia-like syntax
const FUNCL = '('
const FUNCR = ')'
const LISTL = '['
const LISTR = ']'

# Mma fullform returns the value and prints differently.
# We only print the value.
function fullform(io::IO, mx::Mxpr)
    print(io,mhead(mx))
    print("(")
    if length(mx) > 0 fullform(io,mx[1]) end
    for i in 2:length(mx)
        print(io,",")        
        fullform(io,mx[i])
    end
    print(")")
end
fullform(io::IO,x) = show(io,x)
fullform(x) = fullform(STDOUT,x)

## Translate Expr to Mxpr

extomx(x) = x
function extomx(s::Symbol)
    ss = string(s)
    if contains(ss,"_")
        return parseblank(ss)
    else
        return getsym(jtomsym(s))
    end
end

# Underscore is not allowed in symbols. Instead,
# they signify part of a pattern. This follows Mma,
# and we don't consume and Julia syntax to signify patterns.
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
    if ex.head == :call
        head = jtomsym(a[1])
        for i in 2:length(a) push!(newa,extomx(a[i])) end
#        println("got coll")
    elseif ex.head == :block
        mx = extomx(a[2])
        return mx
    elseif haskey(JTOMSYM,ex.head)
        head = JTOMSYM[ex.head]
        extomxarr(a,newa)
    elseif ex.head == :(:)
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
    elseif ex.head == :quote
        head = :Jxpr
        push!(newa,eval(ex))
    else        
        dump(ex)
        error("extomx: No translation for Expr head '$(ex.head)' in $ex")
    end
#    println("Making mxpr")
    mx = mxpr(head,newa)
#    println("done Making mxpr")
    return mx
end

is_call(ex::Expr) = ex.head == :call
is_call(ex::Expr, op::Symbol) = ex.head == :call && ex.args[1] == op
is_call(ex::Expr, op::Symbol, len::Int) = ex.head == :call && ex.args[1] == op && length(ex.args) == len
is_call(ex::Expr, len::Int) = is_call(ex) && length(ex.args) == len

# We check for :call repeatedly. We can optimize this later.
is_binary_minus(ex::Expr) = is_call(ex, :-, 3)
# number of args != 3 will pass through. But probably can't be used
is_division(ex::Expr) = is_call(ex, :/,3)  
is_power(ex::Expr) = is_call(ex, :^)


rewrite_binary_minus(ex::Expr) = Expr(:call, :+, ex.args[2], Expr(:call,:(-),ex.args[3]))
rewrite_division(ex::Expr) = Expr(:call, :*, ex.args[2], Expr(:call,:^,ex.args[3],-1))
rewrite_binary_minus(mx::Mxpr) = mxpr(:+, mx[1], mxpr(:(-),mx[2]))
rewrite_division(mx::Mxpr) = mxpr(:+, mx[1], mxpr(:^,mx[2],-1))

# There is no binary minus, no division, and no sqrt in Mxpr's.
# Concrete example: a - b --> a + -b.
# We definitely need to dispatch on a hash query, or types somehow
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
    entrycount::Int
    traceon::Bool
    timingon::Bool
end
const MEVAL = Meval(0,false,false)

# TODO: get rid of the global
global MEVAL_ENTRY_COUNT = 0

reset_meval_count() = MEVAL.entrycount = 0
get_meval_count() = MEVAL.entrycount
increment_meval_count() = MEVAL.entrycount += 1
decrement_meval_count() = MEVAL.entrycount -= 1
set_meval_trace() = MEVAL.traceon = true
unset_meval_trace() = MEVAL.traceon = false
is_meval_trace() = MEVAL.traceon

## Don't evaluate the expression
macro exnoeval(ex)
    mx = extomx(ex)
    :(($(esc(mx))))
end

macro ex(ex)
    check_help_query(ex) && return nothing
    res = extomx(ex)
    reset_meval_count()
    if MEVAL.timingon
        mx = @time doeval(res)
    else
        mx = doeval(res)
    end
    if is_SJSym(mx) mx = getssym(mx) end # otherwise Julia symbol is returned
    sjset(getsym(:ans),mx)
    :(($(esc(mx))))
end

global const exitcounts = Int[0,0,0,0,0]

# Note: lcheckhash is the identity (ie disabled)
# doeval is loopmeval: ie, we use 'infinite' evaluation. Evaluate till expression does not change.
function loopmeval(mxin::Mxpr)
    @mdebug(2, "loopmeval ", mxin)
    neval = 0
    if checkdirtysyms(mxin)
#        println("got dirty syms $mxin")
        unsetfixed(mxin)
    end
    if is_fixed(mxin)
        # if is_Mxpr(mx) setage(mx) ; println("2 setting age of $mx") end
        exitcounts[1] += 1
        #pprintln("1 Returning ckh $mxin")
        return lcheckhash(mxin)
    end
    mx = meval(mxin)
    if is_Mxpr(mx) && is_fixed(mx)  # Few exits here
        exitcounts[2] += 1
        #pprintln("2 Returning ckh $mx")
        return lcheckhash(mx)
    end
#    println("Check $mx == $mxin : ", mx == mxin)
    if is_Mxpr(mx) && mx == mxin
        setfixed(mxin)
        setage(mxin)
        exitcounts[3] += 1
        #pprintln("3 Returning ckh $mx")
        return lcheckhash(mxin)
    end
    local mx1    
    while true
        mx1 = meval(mx)
        if (is_Mxpr(mx1) && is_fixed(mx1)) || mx1 == mx
            mx = mx1
#            setfixed(mx) # don't think this is correct
            break
        end
        neval += 1
        if neval > 100
            println(mx)
            error("loopmeval: Too many, $neval, evaluations. Expression still changing")
        end
        mx = mx1
    end
    if is_Mxpr(mx) && mx == mxin  setfixed(mx) end  # why not set age here ?
    # No test exits via this point
#     if is_Mxpr(mx) && !(is_fixed(mx)) && mx == mxin
#         setfixed(mxin)
# #        println("3 setting age of $mxin")
#         setage(mxin)
#         exitcounts[4] += 1
#         #pprintln("4 Returning ckh $mx")        
#         return mxin
#     else
# #        println("3 not setting age of $mxin != $mx")
#     end
    exitcounts[5] += 1
    #pprintln("5 Returning ckh $mx")
    return lcheckhash(mx)
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

loopmeval(x) = x

## Evaluation of Mxpr

meval(x) = x
meval(s::SJSym) = symval(s)

function meval(mx::Mxpr)
    increment_meval_count()
    if get_meval_count() > 200
        error("Too many meval entries ", get_meval_count())
    end
    if is_meval_trace()
        ind = " " ^ get_meval_count()
        println(ind,"<< " , mx)
    end
    nhead = doeval(mhead(mx))
#    nhead = mx.head
    local nargs
    mxargs = mx.args
    len = length(mxargs)
    start = 1
    if get_attribute(mx.head,:HoldFirst)
        nargs = newargs(len)
        nargs[1] = mxargs[1]
        for i in 2:length(mxargs)
            nargs[i] = doeval(mxargs[i])
        end        
    elseif get_attribute(mhead(mx),:HoldAll)
        nargs = mxargs
    elseif get_attribute(mhead(mx),:HoldRest)
        nargs = mxargs
        nargs[1] = doeval(nargs[1])
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
                nargs[i] = res1
            end

        else
            nargs = mxargs
        end
    end
    nmx = mxpr(nhead,nargs)
    # Need following, but there is probably a better way to do this.
    nmx.syms = mx.syms
    if get_attribute(nmx,:Listable)  nmx = threadlistable(nmx) end
    # We apply the rules before doing the ordering. This differs from Mma.
    res = apprules(nmx) 
    res == nothing && return nothing    
    if  ! is_canon(res)
        res = flatten!(res)        
        res = canonexpr!(res)
    end
    # The conditional probably saves little time
    if is_Mxpr(res) && length(downvalues(res.head)) != 0  res = applydownvalues(res)  end
    is_meval_trace() && println(ind,">> " , res)
    decrement_meval_count()
    if is_Mxpr(res) && isempty(res.syms)
        for i in 1:length(res)  # This is costly if it is not already done.
            mergesyms(res,res[i])
        end
        checkemptysyms(res)
    end
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

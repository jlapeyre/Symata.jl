const MXDEBUGLEVEL = -1 # debug level, larger means more verbose. -1 is off

Base.base(p::Mxpr{:Power}) = p.args[1]
expt(p::Mxpr{:Power}) = p.args[2]

getindex(x::Mxpr,k::Int) = return k == 0 ? x.head : x.args[k]

function get_attributes(sj::SJSym)
    ks = sort!(collect(keys(symattr(sj))))
    mxpr(:List,ks...)    
end

function ==(ax::Mxpr, bx::Mxpr)
    ax.head != bx.head  && return false
    a = ax.args
    b = bx.args
    (na,nb) = (length(a),length(b))
    na != nb && return false
    for i in 1:na
        a[i] != b[i] && return false
    end
    true
end

getindex(mx::Mxpr, k::Int) = return k == 0 ? mx.head : mx.args[k]
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
    print(io,mx.head)
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

# Underscore is not allow in symbols. Instead,
# they signify part of a pattern
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
end
const MEVAL = Meval(0,false)

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
    res = extomx(ex)
    reset_meval_count()
    mx = loopmeval(res)
    if is_SJSym(mx) mx = getssym(mx) end # otherwise Julia symbol is returned
    sjset(getsym(:ans),mx)
    :(($(esc(mx))))
end

global const exitcounts = Int[0,0,0,0,0]


#lcheckhash(x) = checkhash(x)
@inline lcheckhash(x) = x

# We use 'infinite' evaluation. Evaluate till expression does not change.
function loopmeval(mxin::Mxpr)
    @mdebug(2, "loopmeval ", mxin)
    neval = 0
    # if checkdirtysyms(mxin)
    #     println("mxin is dirty, unsetting")
    #     unsetfixed(mxin)
    # end
    if checkdirtysyms(mxin)
#        println("got dirty syms $mxin")
        unsetfixed(mxin)
#        println("is fixed $mxin ? ", is_fixed(mxin))
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
            break
        end
        neval += 1
        if neval > 100
            println(mx)
            error("loopeval: Too many, $neval, evaluations. Expression still changing")
        end
        mx = mx1
    end
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

function loopmeval(s::SJSym)
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
    nhead = loopmeval(mx.head)
#    nhead = mx.head
    local nargs
    mxargs = mx.args
    start = 1
    if get_attribute(mx.head,:HoldFirst)
        nargs = newargs()
        push!(nargs,mxargs[1])
        for i in 2:length(mxargs)
            push!(nargs,loopmeval(mxargs[i]))
        end        
    elseif get_attribute(mx.head,:HoldAll)
        nargs = mxargs
    elseif get_attribute(mx.head,:HoldRest)
        nargs = mxargs
        nargs[1] = loopmeval(nargs[1])
    else
        changeflag = false  # don't think this helps any
        for i in 1:length(mxargs)
            if mxargs == loopmeval(mxargs)
                changeflag = true
                break
            end
        end
        if changeflag
            nargs = newargs()        
            for i in 1:length(mx.args)
                res1 = loopmeval(mxargs[i])
                push!(nargs,res1)
            end
        else
            nargs = mxargs
        end
    end
    nmx = mxpr(nhead,nargs)
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
    return res
end

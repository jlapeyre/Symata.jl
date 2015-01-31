const MXDEBUGLEVEL = -1 # debug level, larger means more verbose. -1 is off

Base.base(p::Mxpr{:Power}) = p.args[1]
expt(p::Mxpr{:Power}) = p.args[2]

## SJSym functions


sjeval(s::SJSym) = symval(s)
sjset(s::SJSym,val) = setsymval(s,val)
==(a::SJSym,b::SJSym) = symname(a) == symname(b)

getindex(x::Mxpr,k::Int) = return k == 0 ? x.head : x.args[k]

function get_attributes(sj::SJSym)
    ks = sort!(collect(keys(sj.attr)))
    sjs = [getsym(x) for x in ks]
    mxpr(:List,sjs...)
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

# Mathematica syntax
# const FUNCL = '['
# const FUNCR = ']'
# const LISTL = '{'
# const LISTR = '}'

# Julia-like syntax
const FUNCL = '('
const FUNCR = ')'
const LISTL = '['
const LISTR = ']'

needsparen(x::Mxpr) = length(x) > 1
needsparen(x::Rational) = true
needsparen(x::Complex) = true
needsparen(x) = false

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
        blank = mxpr(blanktype,getsym(blankname))
    end
    length(blankhead) == 0 && return blank
    mxpr(:Pattern,getsym(blankhead),blank)
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
    mxpr(head,newa...)
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
#    println("Here in ex")
    reset_meval_count()
    mx = loopmeval(res)
    sjset(getsym(:ans),mx)
    :(($(esc(mx))))
end

# We use 'infinite' evaluation. Evaluate till expression does not change.
function loopmeval(mxin::Union(Mxpr,SJSym))
    @mdebug(2, "loopmeval ", mxin)
#    println("trying $mxin")
    neval = 0
    # if checkdirtysyms(mxin)
    #     println("mxin is dirty, unsetting")
    #     unsetfixed(mxin)
    # end
    mx = meval(mxin)
 #   println("mevaled once")
    if checkdirtysyms(mx)
#        println("mxin is dirty, unsetting")
        unsetfixed(mx)
    end    
    is_fixed(mx) && return mx # (unsetfixed(mx); return mx)
#    println("was fixed in liooper")
    local mx1    
    while true
        mx1 = meval(mx)
        if is_fixed(mx1) || mx1 == mx
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
#    unsetfixed(mx)
    mx
end

loopmeval(x) = x

## Evaluation of Mxpr

meval(x) = x
meval(s::SJSym) = symval(s) == symname(s) ? s : symval(s)

function meval(mx::Mxpr)
#   println("herweeee minsf ")
    increment_meval_count()
    if get_meval_count() > 200
        error("Too many meval entries ", get_meval_count())
    end
    if is_meval_trace()
        ind = " " ^ get_meval_count()
        println(ind,"<< " , mx)
    end
    nhead = mx.head
    local nargs
    start = 1
    if get_attribute(mx.head,:HoldFirst)
        nargs = newargs()
        push!(nargs,mx.args[1])
        for i in 2:length(mx.args)
            push!(nargs,loopmeval(mx.args[i]))
        end        
    elseif get_attribute(mx.head,:HoldAll)
        nargs = mx.args
    elseif get_attribute(mx.head,:HoldRest)
        nargs = mx.args
        nargs[1] = loopmeval(nargs[1])
    else
#        println("Evaling each arg")
        nargs = newargs()        
        for i in 1:length(mx.args)
            res1 = loopmeval(mx.args[i])
            push!(nargs,res1)
        end
    end
    nmx = mxpr(nhead,nargs...)
    res = apprules(nmx)
    res == nothing && return nothing
    if  ! is_canon(res)
        res = deepflatten!(res)        
        res = deepcanonexpr!(res)
#        res = (print("dc "); @time(deepcanonexpr!(res))) # actually, this should be orderless only. others split off        
    end
    res = applydownvalues(res)
    is_meval_trace() && println(ind,">> " , res)
    decrement_meval_count()
#    println("exiting eval dump")
#    is_Mxpr(res) ? dump(res.syms) : nothing
    res
end

## Application of Rules for many heads of Mxpr

apprules(x) = x

function checkprotect(s::SJSym)
    get_attribute(symname(s),:Protected) &&
    error("Symbol '",symname(s), "' is protected.")
end

checkprotect(mx::Mxpr) = checkprotect(mx.head)

# Set SJSym value.
# Set has HoldFirst, SetDelayed has HoldAll.
function apprules(mx::Union(Mxpr{:Set},Mxpr{:SetDelayed}))
    set_and_setdelayed(mx,mx.args[1],mx.args[2])
end

# getsym(symname(lhs)) is because a copy of symbol is being made somewhere
# so we look up the original in the table
function set_and_setdelayed(mx,lhs::SJSym, rhs)
    checkprotect(lhs)
    sjset(getsym(symname(lhs)),rhs)
    rhs
end

# Create DownValue. "function" definition
# eg f(x_) := x  defines a DownValue for the SJSym f
function set_and_setdelayed(mx,lhs::Mxpr, rhs)
    checkprotect(lhs)
    rule = mxpr(:RuleDelayed,mxpr(:HoldPattern,lhs),rhs)
    push_downvalue(lhs.head,rule) # push DownValue
    rule
    nothing
end

# Optimize a bit. Localize variables once, not every time pattern is evaluated
set_and_setdelayed(mx,lhs::Mxpr, rhs::Mxpr{:Module}) = set_and_setdelayed(mx,lhs,localize_module(rhs))

# Bind a Julia symbol to the rhs
function apprules(mx::Mxpr{:SetJ})
    lhs = mx.args[1]
    rhs = mx.args[2]
    eval(Expr(:(=),symname(lhs),rhs))
end 

set_and_setdelayed(mx,y,z) = mx

function apprules(mx::Mxpr{:Symbol})
    dosymbol(mx,mx[1])
end
dosymbol(mx,s::String) = getsym(symbol(s))
dosymbol(mx,x) = error("Symbol: expected a string")

# 'Clear' a value. ie. set symbol's value to its name
function apprules(mx::Mxpr{:Clear})
    for a in mx.args
        checkprotect(a)
        setsymval(a,symname(a))
    end
end

# Remove all values associate with SJSym. values and DownValues
function apprules(mx::Mxpr{:ClearAll})
    for a in mx.args
        checkprotect(a)
        setsymval(a,symname(a))
        clear_downvalues(a)
    end
end

# DumpHold does not evaluate args before dumping
apprules(mx::Union(Mxpr{:Dump},Mxpr{:DumpHold})) = for a in mx.args dump(a) end

apprules(mx::Mxpr{:Length}) = symjlength(mx.args[1])
symjlength(mx::Mxpr) = length(mx.args)
symjlength(x) = length(x)

# Get part of expression. Julia :ref is mapped to :Part
# This won't work for setting a part
function apprules(mx::Mxpr{:Part})
    a = mx.args
    (a[1])[a[2]]
end

apprules(mx::Mxpr{:Head}) = gethead(mx.args[1])
gethead(mx::Mxpr) = mx.head
gethead(s::SJSym) = getsym(:Symbol)
gethead(s::Symbol) = getsym(:JuliaSymbol)
gethead(ex) = typeof(ex)
apprules(mx::Mxpr{:JVar}) = eval(symname(mx.args[1]))
apprules(mx::Mxpr{:AtomQ}) = atomq(mx[1])
apprules(mx::Mxpr{:Attributes}) = get_attributes(mx.args[1])
apprules(mx::Mxpr{:DownValues}) = listdownvalues(mx.args[1])

apprules(mx::Mxpr{:Replace}) = doreplace(mx,mx[1],mx[2])
doreplace(mx,expr,r::Mxpr{:Rule}) = replace(expr,Rule_to_PRule(r))
doreplace(mx,a,b) = mx

apprules(mx::Mxpr{:ReplaceAll}) = doreplaceall(mx,mx[1],mx[2])
doreplaceall(mx,expr,r::Mxpr{:Rule}) = replaceall(expr,Rule_to_PRule(r))
doreplaceall(mx,a,b) = mx

function apprules(mx::Mxpr{:MatchQ})
    (gotmatch,cap) = cmppat(mx[1],just_pattern(mx[2]))
    gotmatch
end

apprules(mx::Mxpr{:FullForm}) = fullform(STDOUT,mx[1])

## Comparison

# Mma does not do it this way. Julia does, and mma4max does.
function apprules(mx::Mxpr{:Comparison})
    nargs1 = newargs()
    i = 1
    while i <= length(mx)
        if is_SJSym(mx[i]) && symname(mx[i]) == :(==)
            if eval(Expr(:comparison,mx[i-1],:(==),mx[i+1]))
                i += 1
            else
                return false
            end
        else
            push!(nargs1,mx[i])
        end
        i += 1
    end
    length(nargs1) == 1  && return true
    nargs = newargs()    
    for x in nargs1
        if is_Number(x)
            push!(nargs,x)
        elseif is_comparison_symbol(x)
            push!(nargs,symname(x))
        else
            return mx
        end
    end
    eval(Expr(:comparison,nargs...))
end

## A few Number rules

# We should probably remove these because they are done by
# the canonicalizer

apprules(mx::Mxpr{://}) = makerat(mx,mx.args[1],mx.args[2])
makerat{T<:Number}(mx::Mxpr{://},n::T,d::T) = n//d
makerat(mx,n,d) = mx
apprules(mx::Mxpr{:complex}) = makecomplex(mx,mx.args[1],mx.args[2])
makecomplex(mx::Mxpr{:complex},n::Real,d::Real) = complex(n,d)
makecomplex(mx,n,d) = mx
#apprules(mx::Mxpr{:Power}) = (println("dopower $mx $(mx[1]), $(mx[2])"); dopower(mx,mx[1],mx[2]))
apprules(mx::Mxpr{:Power}) = dopower(mx,mx[1],mx[2])
dopower(mx::Mxpr{:Power},b::Number,e::Number) = mpow(b,e)
dopower(mx::Mxpr{:Power},b::Symbolic,n::Integer) = n == 0 ? one(n) : n == 1 ? b : mx
dopower(mx::Mxpr{:Power},b::Mxpr{:Power},exp::Integer) = mpow(base(b), (exp*expt(b)))
dopower(mx::Mxpr{:Power},b::Mxpr{:Power},exp) = mpow(base(b), (exp*expt(b)))
dopower(mx,b,e) = mx 

apprules(mx::Mxpr{:BI}) = dobigint(mx,mx[1])
dobigint(mx,x) = mx
dobigint{T<:Number}(mx,x::T) = BigInt(x)
apprules(mx::Mxpr{:BF}) = dobigfloat(mx,mx[1])
dobigfloat(mx,x) = mx
dobigfloat{T<:Number}(mx,x::T) = BigFloat(x)

function apprules(mx::Mxpr{:Plus})
    if length(mx) == 2
        doplus(mx,mx[1],mx[2])
    else
        mx
    end
end
doplus(mx,a::Number,b::Number) = mplus(a,b)
doplus(mx,b,e) = mx

apprules(mx::Mxpr{:Minus}) = is_Number(mx[1]) ? -mx[1] : -1 * mx[1]

## Tracing evaluation

apprules(mx::Mxpr{:TraceOn}) = (set_meval_trace() ; nothing)
apprules(mx::Mxpr{:TraceOff}) = (unset_meval_trace() ; nothing)

function apprules(mxt::Mxpr{:Timing})
    t = @elapsed begin
        reset_meval_count()
        mx = loopmeval(mxt[1])
        sjset(getsym(:ans),mx)
    end
    mxpr(:List,t,mx)
end

function apprules(mxt::Mxpr{:Allocated})
    local mx
    a = @allocated begin
        reset_meval_count()
        mx = loopmeval(mxt[1])
        sjset(getsym(:ans),mx)
    end
    mxpr(:List,a,mx)
end

function apprules(mx::Mxpr{:CompoundExpression})
    local res
        for i in 1:length(mx)
            res = loopmeval(mx[i])
        end
    res
end

apprules(mx::Mxpr{:BuiltIns}) = protectedsymbols()
apprules(mx::Mxpr{:EvenQ}) = is_type_less(mx[1],Integer) && iseven(mx[1])
apprules(mx::Mxpr{:OddQ}) = is_type_less(mx[1],Integer) &&  ! iseven(mx[1])
apprules(mx::Mxpr{:StringLength}) = length(mx[1])
apprules(mx::Mxpr{:Module}) = localize_module(mx)
apprules(mx::Mxpr{:Println}) = println(margs(mx)...)

## Expand, only a bit implemented

function apprules(mx::Mxpr{:Expand})
    mx1 = mx[1]
    ! is_Mxpr(mx1) && return mx # TODO
    doexpand(mx,mx1)
end

doexpand(mx::Mxpr{:Expand}, p::Mxpr{:Power}) = do_expand_power(mx,base(p),expt(p))
doexpand(mx,n) = mx

do_expand_power(mx::Mxpr{:Expand}, b::Mxpr{:Plus}, n::Integer) =
    length(b) != 2 ? mx : do_expand_binomial(mx,b[1],b[2],n)
do_expand_power(mx,b,ex) = mx

#do_expand_binomial(mx::Mxpr{:Expand}, a, b, n::Integer) = @time(expand_binomial(a,b,n))
do_expand_binomial(mx::Mxpr{:Expand}, a, b, n::Integer) = expand_binomial(a,b,n)
do_expand_binomial(mx,a,b,n) = mx

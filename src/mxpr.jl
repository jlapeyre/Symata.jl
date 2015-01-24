const MXDEBUGLEVEL = -1 # debug level, larger means more verbose. -1 is off

for v in ( "Set", "Pattern", "SetJ" )
    @eval begin
        set_attribute(symbol($v),:HoldFirst)
        set_attribute(symbol($v),:Protected)        
    end
end

for v in ("Clear", "ClearAll", "SetDelayed", "HoldPattern", "Hold", "DumpHold",
          "DownValues")
    @eval begin
        set_attribute(symbol($v),:HoldAll)
        set_attribute(symbol($v),:Protected)
    end
end

for v in ("Attributes",)
    @eval begin
        set_attribute(symbol($v),:HoldAll)
        set_attribute(symbol($v),:Protected)
        set_attribute(symbol($v),:Listable)        
    end
end

for v in ("RuleDelayed",)
    @eval begin
        set_attribute(symbol($v),:HoldRest)
        set_attribute(symbol($v),:Protected)
        set_attribute(symbol($v),:SequenceHold)        
    end
end

for v in ("Timing","Allocated")
    @eval begin
        set_attribute(symbol($v),:HoldAll)
        set_attribute(symbol($v),:Protected)
        set_attribute(symbol($v),:SequenceHold)        
    end
end

for v in ("Apply","Dump", "Cos", "Length","Plus","Times", "Blank",
          "JVar", "Replace", "ReplaceAll","TraceOn","TraceOff","FullForm",
         "BI", "BF")
    @eval begin
        set_attribute(symbol($v),:Protected)        
    end
end

for v in ("Pi","E")
    @eval begin
        set_attribute(symbol($v),:Protected)
        set_attribute(symbol($v),:ReadProtected)
        set_attribute(symbol($v),:Constant)        
    end
end

Base.base(p::Mxpr{:Power}) = p.args[1]
expt(p::Mxpr{:Power}) = p.args[2]

## predicate

atomq(x::Mxpr) = false
atomq(x) = true

## SJSym functions

Base.show(io::IO, s::SJSym) = Base.show_unquoted(io,symname(s))
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

# Mma fullform returns the value and prints differently.
# We only print the value
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

function Base.show(io::IO, s::Mxpr)
    if getoptype(s.head) == :binary  
        return show_binary(io,s)
    elseif getoptype(s.head) == :infix
        return show_infix(io,s)
    end
    show_prefix_function(io,s)
end


function Base.show(io::IO, mx::Mxpr{:Comparison})
    args = mx.args    
    for i in 1:length(args)-1
        show(io,args[i])
        print(io," ")
    end
    isempty(args) || show(io,args[end])
end

function show_prefix_function(io::IO, mx::Mxpr)
    mx.head == getsym(:List) ? nothing : print(io,mtojsym(mx.head))
    args = mx.args
    print(io,mx.head == getsym(:List) ? LISTL : FUNCL)
    for i in 1:length(args)-1
        show(io,args[i])
        print(io,",")
    end
    isempty(args) || show(io,args[end])
    print(io,mx.head == getsym(:List) ? LISTR : FUNCR)    
end

function show_binary(io::IO, mx::Mxpr)
    if length(mx) != 2
        show_prefix_function(io,mx)
    else
        lop = mx.args[1]
        if is_Mxpr(lop) && length(lop) > 1
            print(io,"(")
            show(io,lop)
            print(io,")")
        else
            show(io,lop)
        end        
#        show(io,mx.args[1])
        print(io, "", mtojsym(mx.head), "")
        rop = mx.args[2]
        if is_Mxpr(rop) && length(rop) > 1
            print(io,"(")
            show(io,rop)
            print(io,")")
        else
            show(io,rop)
        end
    end
end

# unary minus
function Base.show(io::IO, mx::Mxpr{:Minus})
    arg = mx.args[1]
    if is_Number(arg) || is_SJSym(arg)
        print(io,"-")
        show(io,arg)
    else
        print(io,"-(")
        show(io,arg)
        print(io,")")
    end
end

function Base.show(io::IO, mx::Mxpr{:Plus})
    args = mx.args
    show(io,args[1])    
    for i in 2:length(args)
        if is_type(args[i],Mxpr{:Minus})
            print(io, " - ")            
            show(io,(args[i]).args[1])
        else
            print(io, " + ")
            show(io,args[i])
        end
    end
#    isempty(args) || show(io,args[end])    
end

function show_infix(io::IO, mx::Mxpr)
    args = mx.args
    for i in 1:length(args)-1
        show(io,args[i])
        print(io, mtojsym(mx.head))
    end
    isempty(args) || show(io,args[end])
end

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

# There is no binary minus and no division, and no sqrt in Mxpr's.
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
    sjset(getsym(:ans),mx)
    :(($(esc(mx))))
end

function loopmeval(mxin::Union(Mxpr,SJSym))
    @mdebug(2, "loopmeval ", mxin)
    neval = 0
    mx = meval(mxin)
    local mx1    
    while true
        mx1 = meval(mx)
#        @mdebug(2, " loopmeval in loop: mx=$mx, mx1=$mx1")
        if mx1 == mx
            break
        end
        neval += 1
        if neval > 5
            println(mx)
            error("loopeval: Too many, $neval, evaluations. Expression still changing")
        end
        mx = mx1
    end
    mx
end

loopmeval(x) = x

## Evaluation of Mxpr

meval(x) = x
meval(s::SJSym) = symval(s) == symname(s) ? s : symval(s)

function meval(mx::Mxpr)
    increment_meval_count()
    if get_meval_count() > 200
        error("Too many meval entries ", get_meval_count())
    end
    is_meval_trace() ? ind = " " ^ get_meval_count() : nothing
    is_meval_trace() && println(ind,"me<- " , mx)
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
        nargs = newargs()        
        for i in 1:length(mx.args)
            res1 = loopmeval(mx.args[i])
            push!(nargs,res1)
        end
    end
    nmx = mxpr(nhead,nargs...)
    res = apprules(nmx)
    res == nothing && return nothing
    res = deepflatten!(res)
    res = deepcanonexpr!(res) # actually, this should be orderless only. others split off
    res = applydownvalues(res)
    is_meval_trace() && println(ind,"me-> " , res)
    decrement_meval_count()    
    res
end

## Application of Rules for many SJSym's ...

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

function set_and_setdelayed(mx,lhs::SJSym, rhs)
    checkprotect(lhs)
    sjset(lhs,rhs)
    rhs
end

function set_and_setdelayed(mx,lhs::Mxpr, rhs)
    checkprotect(lhs)
    rule = mxpr(:RuleDelayed,mxpr(:HoldPattern,lhs),rhs)
    pushdownvalue(lhs.head,rule)
    rule
    nothing
end

function apprules(mx::Mxpr{:SetJ})
    lhs = mx.args[1]
    rhs = mx.args[2]
    eval(Expr(:(=),symname(lhs),rhs))
end 

set_and_setdelayed(mx,y,z) = mx

# 'Clear' a value. ie. set symbol's value to its name
function apprules(mx::Mxpr{:Clear})
    for a in mx.args
        checkprotect(a)
        setsymval(a,symname(a))
    end
end

function apprules(mx::Mxpr{:ClearAll})
    for a in mx.args
        checkprotect(a)
        setsymval(a,symname(a))
        cleardownvalues(a)
    end
end

apprules(mx::Union(Mxpr{:Dump},Mxpr{:DumpHold})) = for a in mx.args dump(a) end

apprules(mx::Mxpr{:Length}) = symjlength(mx.args[1])
symjlength(mx::Mxpr) = length(mx.args)
symjlength(x) = length(x)

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

apprules(mx::Mxpr{:FullForm}) = fullform(STDOUT,mx[1])

## Comparison

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
apprules(mx::Mxpr{:Power}) = dopower(mx,mx[1],mx[2])
dopower(mx,b::Number,e::Number) = mpow(b,e)
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
apprules(mx::Mxpr{:Minus}) = is_Number(mx[1]) ? -mx[1] : mx

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

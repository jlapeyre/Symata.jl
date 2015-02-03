##  Some 'apprules' definitions.
# These evaluate expressions with builtin (protected) heads.

# Would be nice to get something like this working to get
# rid of boiler plate or allow changing how dispatch is done
#macro apphead(sym,body...)
#    name = :apprules
#    :(function $(esc(name))(mx::Mxpr{$sym}) $body end)
#end

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
# This won't work for setting a part.
# You must nest this to go down more than one level.
# a[i] parses to Part(a,i), and a[i][j] to Part(Part(a,i),j)
function apprules(mx::Mxpr{:Part})
    a = mx.args
    arr = a[1]
    i = a[2]
    i = i < 0 ? length(arr)+i+1 : i
    arr[i]
end

## crude implementation.
# We don't have syntax to set a part yet.
# This only works at one level.
function apprules(mx::Mxpr{:SetPart})
    a = mx.args
    x = a[1]
    ind = a[2]
    val = a[3]
    x[ind] = val
end

apprules(mx::Mxpr{:Head}) = gethead(mx.args[1])
gethead(mx::Mxpr) = mx.head
gethead(s::SJSym) = getsym(:Symbol)
#gethead(s::Symbol) = getsym(:JuliaSymbol)  # out dated
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

# We do this the Julia- and mma4max way, not the Mma way.
function apprules(mx::Mxpr{:Comparison})
    nargs1 = newargs()
    i = 1
    while i <= length(mx)  # do all the != and ==
        if is_SJSym(mx[i])
            if symname(mx[i]) == :(==)
                if mx[i-1] == mx[i+1]
                    i += 1
                else
                    return false
                end
            elseif symname(mx[i]) == :(!=)
                if mx[i-1] != mx[i+1]
                    i += 1
                else
                    return false
                end
            else
                push!(nargs1,mx[i])
            end
        else
            push!(nargs1,mx[i])
        end
        i += 1
    end
    length(nargs1) == 1  && return true
    nargs = newargs()    
    for x in nargs1   # Do numeric inequalities
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

# These may not all be necessary.

apprules(mx::Mxpr{://}) = makerat(mx,mx.args[1],mx.args[2])
makerat{T<:Number}(mx::Mxpr{://},n::T,d::T) = n//d
makerat(mx,n,d) = mx
apprules(mx::Mxpr{:complex}) = makecomplex(mx,mx.args[1],mx.args[2])
makecomplex(mx::Mxpr{:complex},n::Real,d::Real) = complex(n,d)
makecomplex(mx,n,d) = mx
apprules(mx::Mxpr{:Power}) = dopower(mx,mx[1],mx[2])
dopower(mx::Mxpr{:Power},b::Number,e::Number) = mpow(b,e)
dopower(mx::Mxpr{:Power},b::Symbolic,n::Integer) = n == 1 ? b : n == 0 ? one(n) : mx
dopower(mx::Mxpr{:Power},b::Mxpr{:Power},exp::Integer) = mpow(base(b), (exp*expt(b)))
dopower(mx::Mxpr{:Power},b::Mxpr{:Power},exp) = mpow(base(b), (exp*expt(b)))
dopower(mx,b,e) = mx 

## convert to BigInt or BigFloat. We cannot yet do this automatically
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

# This does not work. Does not report correct time and allocation
# We have to do Allocted and Timing separately
function apprules(mxt::Mxpr{:Timing2})
    begin
        reset_meval_count()
        mx = @time(loopmeval(mxt[1]))
        sjset(getsym(:ans),mx)
    end
    mx
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

# Get the last-altered timestamp of an expression or symbol
apprules(mx::Mxpr{:Age}) = do_getage(mx,mx[1])
do_getage(mx,s::Symbol) = int(getage(s))
do_getage(mx,s::Mxpr) = int(getage(symval(s)))
do_getage(mx,x) = mx

# Get fixed-point bit. Idea is to set it if expr evaluates to itself.
# But, it seems this requires elaborate heuristic to manage elaborate
# data structure to implement leaky abstraction.
apprules(mx::Mxpr{:Fixed}) = is_fixed(symval(mx[1]))

apprules(mx::Mxpr{:BuiltIns}) = protectedsymbols()
apprules(mx::Mxpr{:EvenQ}) = is_type_less(mx[1],Integer) && iseven(mx[1])
apprules(mx::Mxpr{:OddQ}) = is_type_less(mx[1],Integer) &&  ! iseven(mx[1])
apprules(mx::Mxpr{:StringLength}) = length(mx[1])
apprules(mx::Mxpr{:Module}) = localize_module(mx)
apprules(mx::Mxpr{:Println}) = println(margs(mx)...)

## Expand, only a bit is implemented

function apprules(mx::Mxpr{:Expand})
    mx1 = mx[1]
    ! is_Mxpr(mx1) && return mx # TODO
    doexpand(mx,mx1)
end

doexpand(mx::Mxpr{:Expand}, p::Mxpr{:Power}) = do_expand_power(mx,base(p),expt(p))
function doexpand(mx::Mxpr{:Expand}, prod::Mxpr{:Times})
    mulsums(margs(prod)...)
end

doexpand(mx,n) = mx

do_expand_power(mx::Mxpr{:Expand}, b::Mxpr{:Plus}, n::Integer) =
    length(b) != 2 ? mx : do_expand_binomial(mx,b[1],b[2],n)
do_expand_power(mx,b,ex) = mx

#do_expand_binomial(mx::Mxpr{:Expand}, a, b, n::Integer) = @time(expand_binomial(a,b,n))
do_expand_binomial(mx::Mxpr{:Expand}, a, b, n::Integer) = expand_binomial(a,b,n)
do_expand_binomial(mx,a,b,n) = mx

# Only some of Range implemented for testing other things.
# From the command line, all the time evaluating Range(n) for n>10000 or so
# is spent in the inner loop here. Mma v3 is about 4-6 times or so faster.
# So this seems to be purely a Julia vs. low-level Mma difference.
# This is used to test summing numbers.
# Maxima is faster than Mma v3 at Apply(Plus,l) , where l is a big list of numbers
# Maxima is about 10x faster than this code (SJulia).
function apprules(mx::Mxpr{:Range})
    if length(mx) == 1
        n = mx[1]
        args = newargs(n);
        for i in 1:n
            args[i] = i
        end
    elseif length(mx) == 2
        n0 = mx[1] - 1
        n = mx[2]
        args = newargs(n-n0);
        for i in 1:n-n0
            args[i] = i+n0
        end
    elseif length(mx) == 3
        n0 = mx[1]
        n = mx[2]
        di = mx[3]
        off = n > n0 ? 1 : -1
        args = newargs(div(n-n0+off,di));
        len = length(args)
        s = n0
        for i in 1:len
            args[i] = s
            s += di
        end        
    else
        return mx
    end
    r = mxpr(:List,args)
    setfixed(r)
    setcanon(r)
    return r    
end

## quickly hacked Table, just for testing other parts of evaluation

# Replace symbol os with ns in ex
function replsym(ex,os,ns)
    if is_Mxpr(ex)
        args = margs(ex)
        for i in 1:length(args)
            args[i] = replsym(args[i],os,ns)
        end
    end
    if ex == os
        return ns
    else
        return ex
    end
end

# We only do Table(expr,[i,imax])
# Our Table is rather slow. Slower than Maxima makelist.
# Even if we cheat and only do a single evaluation, it is slower.
function apprules(mx::Mxpr{:Table})
    expr = mx[1]
    iter = mx[2]
    isym = gensym(string(iter[1]))
    imax = iter[2]
    ex = replsym(deepcopy(expr),iter[1],isym)
    args = newargs(imax)
    for i in 1:imax
        sjset(getsym(isym),i)
        v = loopmeval(ex)
#        v = meval(ex)        
        setfixed(v)
        args[i] = v
    end
    mx = mxpr(:List,args)
    setfixed(mx)
    mx
end

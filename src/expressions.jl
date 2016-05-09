## TODO: Reorganize this. Migrate most of the Heads handled here elsewhere.
## Which heads should be here ?

## ExpandA, Apply, Reverse

import Combinatorics: permutations

#### Head

@sjdoc Head "
Head(expr) returns the head of expr, which may be an SJulia expression or object of any
Julia type. The head of a Julia expression is Expr, eg.
Head( :( :( a = 1) )) returns Expr. Note we have to quote twice, because one level of
a quoted Julia expression is evaluated so that we can embed Julia code.
"

@mkapprule Head  :nargs =>  1

@doap Head(mx1::Mxpr) = mhead(mx1)
@doap Head(s::SJSym) = getsym(:Symbol)  # or just :Symbol ? This is the ancient inteface
@doap Head(ex) = typeof(ex)

#### ReleaseHold

typealias Holds Union{Mxpr{:Hold}, Mxpr{:HoldForm}, Mxpr{:HoldPattern}, Mxpr{:HoldComplete}}

@mkapprule ReleaseHold :nargs => 1

@sjdoc ReleaseHold "
ReleaseHold(expr) removes the outer layer of Hold, HoldForm, HoldPattern, and HoldComplete from expr.
"

@doap function ReleaseHold(mxa::Holds)
    length(margs(mxa)) == 0 && return mxpr(:Sequence)
    length(margs(mxa)) > 1 && return  mxpr(:Sequence,margs(mxa)...)
    return mxa[1]
end

@doap ReleaseHold(ex) = ex

#### ExpandA

function _doexpand(x)
    ! is_Mxpr(x) && return x
    n = length(x)
    args = margs(x)
    nargs = newargs(n)
    @inbounds for i in 1:n
        nargs[i] = doexpand(args[i])
    end
    return doexpand(mxpr(mhead(x),nargs))
end

doexpand(p::Mxpr{:Power}) = do_expand_power(p,base(p),expt(p))

function doexpand(prod::Mxpr{:Times})
    a = margs(prod)
    len = length(a)
    @inbounds for i in 1:len
        a[i] = doexpand(a[i])
    end
    have_sum = false
    j = 0
    @inbounds for i in 1:len # check if we have anything to do
        j += 1
        if is_Mxpr(a[i],:Plus)
            have_sum = true
            break
        end
    end
    ! have_sum && return prod
    nonsums = newargs()
    @inbounds for i in 1:j-1  # none of these are sums
        push!(nonsums,a[i])
    end
    sums = newargs()
    push!(sums,a[j]) # already know its a sum
    @inbounds for i in j+1:len   # push more sums, if there are any
        if is_Mxpr(a[i],:Plus)
            push!(sums,a[i])
        else
            push!(nonsums,a[i])
        end
    end
    sumres = length(sums) == 1 ? sums[1] : mulfacs(sums...)
    nlen = length(nonsums)
    if nlen == 0
        mxout = sumres
    elseif nlen == 1
        mxout = mulfacs(nonsums[1],sumres)
    else
        mxout = mulfacs(mxpr(:Times,nonsums),sumres)
    end
    setfixed(mxout)
    return mxout
end

doexpand(mx) = mx

do_expand_power(p,b::Mxpr{:Plus}, n::Integer) =
    length(b) != 2 ? p : do_expand_binomial(p,b[1],b[2],n)
do_expand_power(p,b,ex) = p

do_expand_binomial(p,a, b, n::Integer) = expand_binomial(a,b,n)
do_expand_binomial(a,b,n) = p

function doexpand(s::Mxpr{:Plus})
    args = margs(s)
    @inbounds for i in 1:length(args)
        args[i] = doexpand(args[i])
    end
    return s
end

## expand product of two sums
# a an b are the factors
# In test cases, this is fast. later, canonicalizing each term is slowest thing.
function mulfacs(a::Mxpr{:Plus},b::Mxpr{:Plus})
    terms = newargs(length(a)*length(b))
    i = 0
    for ax in a.args
        for bx in b.args
            i += 1
            t = flatcanon!(mxpr(:Times, ax, bx)) # TODO specialize for types of ax, bx
            mergesyms(t,ax)
            mergesyms(t,bx)
            setfixed(t)
           @inbounds  terms[i] = t
        end
    end
    mx = flatcanon!(mxpr(:Plus,terms))
    for t in terms
        mergesyms(mx,t)
    end
    setfixed(mx)
    mx
end

# Not the right way to do this. We need to expand each term, as well.
mulfacs(a,b,c) = mulfacs(mulfacs(a,b),c)
mulfacs(a,b,c,xs...) = mulfacs(mulfacs(mulfacs(a,b),c),xs...)

# Should probably write this
#function mulfacs(a::Mxpr{:Plus}, b::SJSym)
#end

function _pushfacs!(facs,mx::Mxpr{:Times})
    append!(facs,margs(mx))
end

function _pushfacs!(facs,b)
    push!(facs,b)
end

function mulfacs(a::Mxpr{:Plus},b)
    terms = newargs(length(a))
    i = 0
    for ax in a.args
        i += 1
        if is_Mxpr(ax) && mxprtype(ax) == :Times
            facs = copy(margs(ax))
            _pushfacs!(facs,b)  # I hope type inference optimizes this.
            terms[i] = mxpr(:Times,facs)
        else
            terms[i] = b * ax
        end
        canonexpr!(terms[i])
        mergeargs(terms[i])
    end
    mx = mxpr(:Plus,terms)
    mergeargs(mx)
    mx
end

function mulfacs(a, b::Mxpr{:Plus})
    mulfacs(b,a)
end

## construct Power. Decide whether to canonicalize according to types of args
function canonpower{T<:Real}(base,expt::T)
    canonexpr!(base^expt)
end
function canonpower(base::SJSym,expt)
    base^expt
end
function canonpower(base,expt)
    base^expt
end

##

# optimize a bit for types
function _expand_binomial_aux1(a::SJSym,b::SJSym,n)
    mxpr(:Times,n,canonpower(a,(n-1)),b)
end
function _expand_binomial_aux1(a,b,n)
    flatcanon!(mxpr(:Times,n,canonpower(a,(n-1)),b))
end
function _expand_binomial_aux2(a::SJSym,b::SJSym,n)
    mxpr(:Times,n,a,canonpower(b,(n-1)))
end
function _expand_binomial_aux2(a,b,n)
    flatcanon!(mxpr(:Times,n,a,canonpower(b,(n-1))))
end


# Be careful to construct expression in canonical form.
# Lots of ways to go wrong.
# Assume a < b in canonical order
# This is the only place we are testing meta data in Mxpr giving which
# symbols it depends on.
function expand_binomial{T<:Integer}(a,b,n::T)
    args = newargs(n+1)
    args[1] = canonpower(a,n)
    mergesyms(args[1],a)
    setfixed(args[1])
    args[n+1] =  canonpower(b,n)
    mergesyms(args[n+1],b)
    setfixed(args[n+1])
    if n == 2
        args[2] = flatcanon!(mxpr(:Times,2,a,b))  # we have to flatcanon
    else
        # TODO optimize for symbols a,b, as in general case below. No flatcanon.
        args[2] = _expand_binomial_aux1(a,b,n)
        args[n] = _expand_binomial_aux2(a,b,n)
        mergesyms(args[2],a)
        mergesyms(args[2],b)
        mergesyms(args[n],a)
        mergesyms(args[n],b)
        setfixed(args[2])
        setfixed(args[n])
        fac = n
        k = n
        l = one(n)
        expand_binomial_aux(k,l,n,fac,a,b,args)
    end
    mx = mxprcf(:Plus,args)
    mergesyms(mx,a)
    mergesyms(mx,b)
    apply_upvalues_to_args!(mx)  # takes some time
    setage(mx)
    setfixed(mx)
    mx
end

# Big increase in efficiency (> 10x) for both these types
typealias ExpNoCanon Union{SJSym,Number}

# Expand((a+b*c)^n) is 10x slower than Expand((a+b)^n)
function _expand_mulpowers(fac,b1,e1,b2,e2)
    m1 = canonpower(b1,e1)  # adds 10-15% time
    m2 = canonpower(b2,e2)
    setfixed(m1)
    setfixed(m2)
    mergesyms(m1,b1)
    mergesyms(m2,b2)
    return flatcanon!(mxpr(:Times, fac, m1, m2)) # flatcanon adds 10x time !, even if nothing is done
end

function _expand_mulpowers{T<:ExpNoCanon, V<:ExpNoCanon}(fac,b1::T,e1,b2::V,e2)
    m1 = b1^e1
    m2 = b2^e2
    setfixed(m1)
    setfixed(m2)
    mergesyms(m1,b1)
    mergesyms(m2,b2)
    return mxpr(:Times, fac, m1, m2)
end

function _expand_mulpowers{T<:ExpNoCanon}(fac,b1::T,e1,b2,e2)
    m1 = b1^e1
    m2 = canonpower(b2,e2)
    setfixed(m1)
    setfixed(m2)
    mergesyms(m1,b1)
    mergesyms(m2,b2)
    return flatcanon!(mxpr(:Times, fac, m1, m2))
end

function _expand_mulpowers{T<:ExpNoCanon}(fac,b1,e1,b2::T,e2)
    m1 = canonpower(b1,e1)
    m2 = b2^e2
    setfixed(m1)
    setfixed(m2)
    mergesyms(m1,b1)
    mergesyms(m2,b2)
    return flatcanon!(mxpr(:Times, fac, m1, m2))
end

function expand_binomial_aux(k,l,n,fac,a,b,args)
        @inbounds for j in 2:n-2
            k = k - 1
            l = l + 1
            fac *= k
            fac = div(fac,l)
            args[j+1] = _expand_mulpowers(fac,a,n-j,b,j)
            mergesyms(args[j+1],a)
            mergesyms(args[j+1],b)
            setfixed(args[j+1])
        end
end

#### Apply

@sjdoc Apply "
Apply(f,expr) replaces the Head of expr with f. This also works for some
Julia objects. Eg. Apply(Plus, :( [1:10] )) returns 55. Apply can be used
in operator form. For example m = Apply(Plus),  m(f(a,b,c)).
"

# Why mkapprule does not work ?
apprules(mx::Mxpr{:Apply}) = do_Apply(mx,margs(mx)...)

# This allows things like:  Apply(f)([a,b,c])
do_Apply(mx,f) = mx
do_Apply(mx,x,y) = mx

function do_Apply(mx::Mxpr,head::SJSym,mxa::Mxpr)
    if (head == :Plus || head == :Times ) # 4 or 5 times faster for plus on numbers, don't evaluate
#        mx = mxpr(head,copy(margs(mxa))) # we may find that we need to copy
        mx = mxpr(head,margs(mxa))
        mx = canonexpr!(mx)            # this is ok
#        mx = canonexpr_orderless!(mx) # this is ok too.
        setcanon(mx)
    else
        mx = mxpr(head,margs(mxa))
    end
    is_Mxpr(mx) && length(mx) == 0 && return 0   # do this instead. fixes bug Apply(Times, [DirectedInfinity(),0]) --> 0
    mx
end

do_Apply(mx::Mxpr,h,mxa::Mxpr) = mxpr(h,margs(mxa))

# Apply operation to a typed numeric array.
# We can build these functions with a macro and
# mapping from  :Times -> mmul
# :Cos -> cos, etc.
function do_Apply{T<:Number}(mx::Mxpr,h::SJSym,arr::Array{T})
    if h == :Plus
        s = zero(T)
        for i in 1:length(arr)
            s += arr[i]
        end
        return s
    end
    return mx
end

#### Reverse

function Base.reverse(mx::Mxpr)
    mx1 = copy(mx)
    Base.reverse!(margs(mx1))
    return mx1
end

@sjdoc Reverse "
Reverse(expr) reverses the order of the arguments in expr.
"
function apprules(mx::Mxpr{:Reverse})
    do_reverse(mx[1])
end

# Builtin Orderless
# they would only be resorted
do_reverse(mx::Orderless) = mx

function do_reverse(mx::Mxpr)
    if get_attribute(mx,:Orderless)
        return mx
    end
    setfixed(mxpr(mhead(mx),reverse(margs(mx))))
end

#### Permutations

@sjdoc Permutations "
Permutations(expr) give a list of all permutations of elements in expr.
"

function apprules(mx::Mxpr{:Permutations})
    perms = collect(permutations(margs(mx[1])))
    h = mhead(mx[1])
    len = length(perms)
    nargs = newargs(len)
    @inbounds for i in 1:len
        nargs[i] = setfixed(mxpr(:List,perms[i]))
    end
    setfixed(mxpr(:List,nargs))
end

@sjdoc FactorInteger "
FactorInteger(n) gives a list of prime factors of n and their multiplicities.
"
apprules(mx::Mxpr{:FactorInteger}) = setfixed(mxpr(:List,do_unpack(factor(mx[1]))))

#### Level




#### Map

@sjdoc Map "
Map(f,expr) returns f applied to each element in a copy of expr.
f can be an SJulia object or a Julia function. Map can be used in
an operator form. For example Map(f)(expr).
"

@mkapprule Map

function do_Map(mx::Mxpr{:Map},f::Function,expr::Mxpr)
    args = margs(expr)
    nargs = newargs(args)
    @inbounds for i in 1:length(args)
        nargs[i] = f(args[i]) # Probably need more evaluation
    end
    mxpr(mhead(expr),nargs)
end

# We create one Mxpr outside the loop. Old
# code (commented out) created Mxpr every time.
# This saves 30 percent of time and allocation in some tests.
function do_Map(mx::Mxpr{:Map},f,expr::Mxpr)
    args = margs(expr)
    nargs = newargs(args)
    mx = mxpr(f,0) # reserve one argument
    @inbounds for i in 1:length(args)
        mx.args[1] = args[i]  # map f of one argument
        nargs[i] = doeval(mx)
    end
    mxpr(mhead(expr),nargs)
end

#### ToExpression

@sjdoc ToExpression "
ToExpression(str) converts string str to an expression.
"
set_pattributes("ToExpression")
apprules(mx::Mxpr{:ToExpression}) = do_ToExpression(mx,margs(mx)...)
do_ToExpression{T<:AbstractString}(mx,s::T) = eval(parse("@ex " * mx[1]))
do_ToExpression(mx,s) = s
do_ToExpression(mx,args...) = mx

#### Count

@sjdoc Count "
Count(expr,pattern) returns the number of arguments in expr than match pattern.
Only matching on one level is supported. This is for testing the performance
of pattern matching. Count(pattern) can be used as the head of an expression,
as an operator. For instance cop = Count(_^2) defines a function that counts
the number of arguments that have the form of a square. Count also works when
expr is a Julia Dict.
"

@sjexamp( Count,
         ("Count(Range(10), _Integer)", "10"),
         ("Count(_Integer)(Range(10))", "10"),
         ("Count(Range(10), 2)", "1"))

set_pattributes("Count")
function apprules(mx::Mxpr{:Count})
    do_Count(mx,margs(mx)...)
end


# Allocating outside loop and sending Dict as arg is 3x faster in one test
function do_Count(mx,expr,pat)
    args = margs(expr)
    c = 0
    jp = just_pattern(pat)
    capt = capturealloc()
    @inbounds for i in 1:length(args)
        (gotmatch,capt) = cmppat(args[i],jp,capt)
        gotmatch ? c += 1 : nothing
    end
    return c
end

# for operator form.
function do_Count(mx,pat)
    mx
end

# operator form of Count
function do_GenHead(mx,head::Mxpr{:Count})
    mxpr(mhead(head),copy(margs(mx))...,margs(head)...)
end


#### Cases

@sjdoc Cases "
Cases(expr,pattern) returns the elements in expr that match the pattern.
Matching on only one level is supported. Cases(pattern) can be used as the head of an expression, as an operator.
eg: getints = Cases(_Integer). The head of the returned object is the same as that of expr.
"

@sjexamp( Cases,
         ("Cases([1,2.0,3,\"dog\"], _Integer)", "[1,3]"))

set_pattributes("Cases")
function apprules(mx::Mxpr{:Cases})
    do_Cases(mx,margs(mx)...)
end

function sjcopy(s::AbstractString)
    identity(s)
end

function sjcopy(x)
    copy(x)
end

# Allocating outside loop and sending Dict as arg is 3x faster in one test
function do_Cases(mx,expr,pat)
    args = margs(expr)
    nargs = newargs()
    jp = just_pattern(pat)
    capt = capturealloc()
    @inbounds for i in 1:length(args)
        (gotmatch,capt) = cmppat(args[i],jp,capt)
        gotmatch ? push!(nargs,sjcopy(args[i])) : nothing
    end
    rmx = mxpr(mhead(expr),nargs)
    return rmx
end

# for operator form.
function do_Cases(mx,pat)
    mx
end

# operator form of Cases
function do_GenHead(mx,head::Mxpr{:Cases})
    mxpr(mhead(head),sjcopy(margs(mx))...,margs(head)...)
end


#### DeleteCases

@sjdoc DeleteCases "
DeleteCases(expr,pattern) deletes the elements in expr that match the pattern.
Matching on only one level is supported. DelteCases(pattern) can be used as the head of an expression, as an operator.
eg: noints = DeleteCases(_Integer). The head of the returned object is the same as that of expr.
"

@sjexamp( DeleteCases,
         ("DeleteCases([1,2.0,3,\"dog\"], _Integer)", "[2.0,\"dog\"]"))


@mkapprule DeleteCases

# Allocating outside loop and sending Dict as arg is 3x faster in one test
@doap function DeleteCases(expr,pat)
    args = margs(expr)
    nargs = newargs()
    jp = just_pattern(pat)
    capt = capturealloc()
    @inbounds for i in 1:length(args)
        (gotmatch,capt) = cmppat(args[i],jp,capt)
        gotmatch ? nothing : push!(nargs,sjcopy(args[i])) # The difference from Cases
    end
    rmx = mxpr(mhead(expr),nargs)
    return rmx
end

# for operator form.
# Using @doap is not transparent!
@doap function DeleteCases(pat)
    mx
end

# operator form of DeleteCases
function do_GenHead(mx,head::Mxpr{:DeleteCases})
    mxpr(mhead(head),sjcopy(margs(mx))...,margs(head)...)
end


#### Push!

@sjdoc Push! "
Push!(a,val) pushes val onto the expression that symbol a evaluates to.
Push! is outside the Mma programming model, which requires immutable
expressions. Re-evaluation, and updating metadata is not implemented.
Re-evaluation can be forced with Unfix(a).
"

@sjexamp( Push!,
         ("ClearAll(a,b)",""),
         ("a = []",""),
         ("For(i=1, i < 1000, Increment(i), Push!(a,Symbol(\"b\$i\")))",""))

set_pattributes(["Push!"],[:HoldFirst])
apprules(mx::Mxpr{:Push!}) = do_Push(mx,margs(mx)...)
do_Push(mx,args...) = mx
do_Push(mx,x::SJSym,val) = do_Push1(mx,symval(x),val)
do_Push1(mx,x,val) = mx
do_Push1(mx,x::Mxpr,val) = (push!(x.args,val); val)

#### Pop!

@sjdoc Pop! "
Pop!(expr) pops a value from the arguments of expr. This mutates expr.
Re-evaluation, and updating metadata is not implemented.
Re-evaluation can be forced with Unfix(a).
"

set_pattributes(["Pop!"],[:HoldFirst])
apprules(mx::Mxpr{:Pop!}) = do_Pop(mx,margs(mx)...)
do_Pop(mx,args...) = mx
do_Pop(mx,x::SJSym,val) = do_Pop1(mx,symval(x),val)
do_Pop1(mx,x,val) = mx
do_Pop1(mx,x::Mxpr,val) = pop!(x.args)

###########################################################
## Canonical ordering of elements in orderless Mxpr       #
###########################################################

### !! This file has other canonicalizing code.: for Power

# There are four (more or less) steps:
# 1. Reduce sequences of numbers, leaving perhaps singletons. (We could choose to get singletons as well)
#    Doing this first is much,much faster for sums and prods with lots of numbers. May slow down
#    canoning other expressions.
# 2. Sort the terms and factors into a canonical order.
# 3. Evaluate all + and * between singleton numbers from 1., which have been sorted to beginning.
# 4. Combine
#    A. terms with same numeric coefficient
#    B. terms with same numeric power

## Comparison functions for sorting expressions to canonical order.

# Mathematica and Maxima sort terms and factors before evaluating-- it
# is difficult or impossible to hold an expression in non-canonical
# form (well, in SJulia and Maxima, you have the source code.) Maple
# apparently does not sort expressions to canoncial order. But Maple
# (so they say) essentially never stores multiple copies of
# expressions that are the same: a hash key is computed for each
# expression, and the key is looked-up in a data structure. If the
# expression exists, a pointer to the original expression is used in
# place of the new one. In Maple, the hash key for for AC expressions
# is invariant under permutation of terms(factors).

# We try to follow the Mma order for Orderless Heads, only Plus and Times here.

# Anything with a Blank is greater than anything without a Blank.
# This is the natural order for pattern matching.
# Blank, BlankSequence, BlankNullSequence are not less than one another

const _jstypeorder = Dict{DataType,Int}()
#const _jsoporder = Dict{Symbol,Int}()  # maybe more efficient to use this rather than mult dispatch.

# orderless (commutative) functions will have terms ordered from first
# to last according to this order of types. Then lex within types.
function _mklexorder()
    i = 1   ## CAREFULL! We use literal index of Any below
    for typ in (Float64,BigFloat,Int,BigInt,Any,Rational,Symbol,SJSym,Expr,AbstractMxpr)
        _jstypeorder[typ] = i
        i += 1
    end
    i = 1
    # for op in (:Plus,:Times,:Power)
    #     _jsoporder[op] = i
    #     i += 1
    # end    
end

_mklexorder()

# returns ordering precedence of Type, typ
function mxtypeorder(typ::DataType)
    ! haskey(_jstypeorder,typ)  && return 5  # Any
    return _jstypeorder[typ]
end

# function mxoporder(op::SJSym)
#     ! haskey(_jsoporder,op)  && return 4 # higher
#     return _jsoporder[op]
# end

## FIXME jslexless.  simplify and economize this code.

# Return the name of the blank or ""
# For sorting, no name becomes a zero-length string.
# The "blankname" is Blank[Name], this name is the type qualifier for the match
blankname(b) = length(b) == 0 ? "" : b[1]

# Order of Blankxxx's when they have no name (i.e. do not capture a match)
lessblanknoname(x::Mxpr{:Blank},y::Mxpr{:BlankSequence}) = true
lessblanknoname(x::Mxpr{:BlankSequence},y::Mxpr{:Blank}) = false
lessblanknoname(x::Mxpr{:Blank},y::Mxpr{:BlankNullSequence}) = true
lessblanknoname(x::Mxpr{:BlankNullSequence},y::Mxpr{:Blank}) = false
lessblanknoname(x::Mxpr{:BlankNullSequence},y::Mxpr{:BlankSequence}) = true
lessblanknoname(x::Mxpr{:BlankSequence},y::Mxpr{:BlankNullSequence}) = false

jslexless(x::Mxpr{:Pattern}, y::Mxpr{:Pattern}) = jslexless(x[1],y[1])

for b in ("Blank","BlankSequence","BlankNullSequence")
    for b1 in ("Blank","BlankSequence","BlankNullSequence")    
        @eval begin
            function jslexless(x::Mxpr{symbol($b)},y::Mxpr{symbol($b1)})
                blankname(x) == blankname(y) == "" && return lessblanknoname(x,y)
                jslexless(blankname(x),blankname(y))
            end
        end
    end
    @eval begin
        jslexless(x::Mxpr{symbol($b)},y::Mxpr{:Pattern}) = true
        jslexless(x::Mxpr{:Pattern},y::Mxpr{symbol($b)}) = false
    end
end

# This works, but we did not need it
# is_nameless_blankxxx(mx::BlankXXX) = blankname(mx) == ""
# # Return true if mx contains a BlankXXX at any depth
# function expr_has_blank(mx::Mxpr)
#     is_blankxxx(mx) && return true
#     for i in 1:length(mx)
#         expr_has_blank(mx[i]) && return true
#     end
#     return false
# end
# expr_has_blank(x) = false

for b in ("Blank","BlankSequence","BlankNullSequence","Pattern")
    for o in ("Times","Plus","Power")
        @eval begin
            jslexless(x::Mxpr{symbol($o)},y::Mxpr{symbol($b)}) = false            
            jslexless(x::Mxpr{symbol($b)},y::Mxpr{symbol($o)}) = true
        end
    end
    @eval begin
        jslexless(x::Mxpr{symbol($b)},y::SJSym) = false
        jslexless(x::SJSym, y::Mxpr{symbol($b)}) = true
    end        
end

# Try using Union type BlankXXX here. No, this causes ambiguity errors.
for b in ("Blank","BlankSequence","BlankNullSequence")
    @eval begin
        function jslexless(x::Mxpr, y::Mxpr{symbol($b)})
            return false
        end
        function  jslexless(x::Mxpr{symbol($b)},y::Mxpr)
            return true
        end
    end
end
#jslexless(x::Mxpr, y::BlankXXX) = false
#jslexless(x::BlankXXX, y::Mxpr) = true

jslexless(x::Mxpr, y::Mxpr{:Pattern}) = true
jslexless(x::Mxpr{:Pattern}, y::Mxpr) = false

function jslexless(x::Mxpr{:Power}, y::Mxpr{:Power})
    jslexless(base(x),base(y)) || (!jslexless(base(y),base(x))) &&  jslexless(expt(x),expt(y))
end
jslexless(x::Mxpr{:Times}, y::Mxpr{:Times}) = jslexless(x[end],y[end])
jslexless(x::Mxpr{:Times}, y::Mxpr{:Power}) = jslexless(x[end],y)
jslexless(x::Mxpr{:Power}, y::Mxpr{:Times}) = jslexless(x,y[end])

function jslexless(x::Mxpr{:Power}, y::Mxpr)
    if y == base(x)
        return is_type_less(expt(x),Real) && expt(x) < 0
    end
    jslexless(base(x),y)
end
function jslexless(x::Mxpr, y::Mxpr{:Power})
    if x == base(y)
        return is_type_less(expt(y),Real) && expt(y) > 1
    end
    jslexless(x,base(y))
end

function jslexless(x::Mxpr{:Power}, y::SJSym)
    if y == base(x)
        return is_type_less(expt(x),Real) && expt(x) < 0
    end
    jslexless(base(x),y)
end

function jslexless(x::SJSym, y::Mxpr{:Power})
    if x == base(y)
        return is_type_less(expt(y),Real) && expt(y) > 1
    end
    jslexless(x,base(y))
end

# Last element in a sorted expression is most significant
jslexless(x::Mxpr{:Times}, y::SJSym) = jslexless(x[end],y)
jslexless(x::SJSym, y::Mxpr{:Times}) = jslexless(x,y[end])

jslexless(x::Mxpr{:Plus}, y::SJSym) = jslexless(x[end],y)
jslexless(x::SJSym, y::Mxpr{:Plus}) = jslexless(x,y[end])

jslexless(x::Mxpr{:Times}, y::Mxpr) = jslexless(x[end],y)
jslexless(x::Mxpr, y::Mxpr{:Times}) = jslexless(x,y[end])

# TODO: do we need to define casecmp differently for MSWin ?
# cmp(a::Symbol, b::Symbol) is in base/string.jl
# Mma does case insensitive ordering of symbols, we do too
casecmp(a::Symbol, b::Symbol) = int(sign(ccall(:strcasecmp, Int32, (Ptr{UInt8}, Ptr{UInt8}), a, b)))
# Same as isless(a,b) for symbols, but we do case insensitive comparison first
# *and* we invert order of upper to lower case.  we want  a < A.
jsisless(a::Symbol, b::Symbol) = (r = casecmp(a,b); r == 0 ? cmp(a,b) > 0 : r < 0)
jslexless(x::SJSym, y::SJSym) = jsisless(x,y)
jslexless(x::Number, y::SJSym) = true
jslexless(x::Number, y::Mxpr) = true
jslexless(x::Mxpr, y::Number) = false
jslexless(x::SJSym, y::Mxpr) = true
jslexless(x::Mxpr, y::SJSym) = false

# Assume args of x and y are sorted. Compare
# last (most significant) element in expressions,
# x[i], y[j] (i and j are in general different becuase
# x and y are of different lengths)
# If x[i] != y[j], return true or false.
# Otherwise repeat, comparing next-to-last elements, etc.
# If all terms compared are equal, the shorter of
# x and y is the least.
function jslexless{T}(x::Mxpr{T},y::Mxpr{T})
    x === y && return false
    ax = margs(x)
    ay = margs(y)
    lx = length(ax)
    ly = length(ay)
    ix = lx
    iy = ly
    n = min(lx,ly)
    @inbounds for i in 1:n
        jslexless(ax[ix],ay[iy]) && return true
        if jslexless(ay[iy],ax[ix])
            return false
        end
        ix -= 1
        iy -= 1
    end
    return lx < ly
end

jslexless{T,V}(x::Mxpr{T},y::Mxpr{V}) = T < V
jslexless(x::SJSym, y::Mxpr) = true
# Following methods will only be called on all non-Symbolic types. (not Symbol or Mxpr)
_jslexless(x::DataType,y::DataType) = x <: y
_jslexless{T}(x::T,y::T) = lexless(x,y)  # use Julia definitions
#jslexless{T}(x::T,y::T) = !(x === y) && _jslexless(x,y)  # x === y may or may not be efficient.
jslexless{T}(x::T,y::T) =  _jslexless(x,y)
# We have defined an order for different types
jslexless{T,V}(x::T,y::V) = mxtypeorder(T) < mxtypeorder(V)

## Step 2. Sort expression according to jslexless.

# Order the args in orderless Mxpr.
function orderexpr!(mx::Mxpr)
    ar = mx.args
    sort!(ar,lt=jslexless)
    mx
end

# Step 1. Reduce sequences of explicit numbers.
# Sum (multiply) sequence of numbers in expression before sorting.
# This only removes one consecutive run of numbers, but we repeat.
for (op,name,id) in  ((:Plus,:plusfirst!,0),(:Times,:mulfirst!,1))
    @eval begin    
        function ($name)(mx::Mxpr, n0::Int)
            args = margs(mx)
            len = length(args)
            n = 0
            @inbounds for i in n0:len
                if is_Number(args[i])
                    n = i
                    break
                end
            end
            n == 0 && return (mx,len)
            s = $(op == :Plus ? :(zero(args[n]))  :  :(one(args[n])))
            m = 0
            @inbounds for i in n:len
                x = args[i]
                if is_Number(x)
                    s = $(op == :Plus ? :(mplus(s,x)) : :(mmul(s,x)) )
                else
                    m = i
                    break
                end
            end
            m == 0 && n == 1 &&  return (s,len)
            if m == 0 m = len + 1 end
            splice!(args,n:m-1,[s])
            return (mx,m)
        end
    end
end

# Reduce all sequences of numbers in sums and products before sorting.
# Summing numbers in sums before sorting makes a large (eg 1000x) difference
# for large sums with many (or all) numbers.
# Singleton sequences may remain and they are sorted and removed with
# compactsumlike!. Probably could analyze sum and choose a strategy.
function loopnumsfirst!(mx::Orderless)
    len = length(mx)
    m = 1
    while true
        (mx,m) =  numsfirst!(mx,m)
        (is_Number(mx) || m >= len) && return mx
        nlen = length(mx)
        p = len - m        
        m = nlen - p
        len = nlen
    end
end 

numsfirst!(mx::Mxpr{:Times},n) = mulfirst!(mx,n)
numsfirst!(mx::Mxpr{:Plus},n) = plusfirst!(mx,n)

# Try: Assume we can mutate and reuse the same Mxpr.
# (x^n)^m --> x^(n*m) , only for real n,m
function mulpowers(mx::Mxpr{:Power})
    (e,b) = numeric_expt_and_base(mx)
    (e1,b1) = numeric_expt_and_base(base(mx))
    e != 1 && e1 != 1 && return mxpr(:Power,b1,mmul(e,e1))
    return mx
end
mulpowers(x) = x

## Distribute -1 * sum

function distribute_minus_one_b(n,terms)
    newterms = newargs(length(terms))
    @inbounds for i in 1:length(terms)
        newterms[i] = mxpr(:Times,n,terms[i])
    end
    return newterms
end
function distribute_minus_one_a(mx, n::Integer, sum0::Mxpr{:Plus})
    n == -1 || return mx
    return mxpr(:Plus,distribute_minus_one_b(n,margs(sum0)))
end
distribute_minus_one_a(mx, n, m) = mx

function distribute_minus_one(mx::Mxpr{:Times})
    length(mx) != 2 && return mx
    distribute_minus_one_a(mx,mx[1],mx[2])
end
distribute_minus_one(x) = x

#### canonexpr!

canonexpr!(mx::Orderless) = canonexpr_orderless!(mx)

function canonexpr!(mx::Mxpr)
    if get_attribute(mx,:Orderless)
        orderexpr!(mx)
    end
    mx
end

## Apply all steps listed at top of this file.
# We say 'sum', but this applies to Times as well
function canonexpr_orderless!(mx)
#    println("canon $mx")
    mx = loopnumsfirst!(mx)  # remove sequences of numbers
    is_Number(mx) && return mx
    mx = distribute_minus_one(mx)
    orderexpr!(mx)  # sort terms
    if is_type_less(mx,Mxpr)        
        mx = compactsumlike!(mx) # sum numbers not gotten by loopnumsfirst.
        if is_type_less(mx,Mxpr)
            mx = collectordered!(mx)  # collect terms differing by numeric coefficients
            # following is rarely if ever used.
            if is_Mxpr(mx,:Power)
                mx = mulpowers(mx)
            end  # add numeric exponents when base is same
        end
    end
    setcanon(mx)
    mx
end 

# We set fixed here, but not for orderless above. Which one is correct ?
function canonexpr!(mx::Mxpr{:Power})
    mx = do_canon_power!(mx,base(mx),expt(mx)) # Don't want "!" here
    if is_Mxpr(mx,:Times)
        mx = canonexpr!(mx)
    end
    setcanon(mx)
    #  setfixed(mx) # m = (a * b * c * d * e *f * g * h)^2  takes 2e-4 s if we don't do this
    if ! is_Mxpr(mx,:Power)  # but some Power mxprs are not evaled completely.
        setfixed(mx)
    end
    return mx
    #    mergeargs(mx)
end

# (expr1*expr2*....)^n --> expr1^n * expr2^n * .... for numeric n
# Assume the product prod has already been sorted;
# the number, if present is first.
function do_canon_power!{T<:Real}(mx::Mxpr{:Power},prod::Mxpr{:Times}, expt::T)
    len = length(prod)
    args = margs(prod)
    nargs = newargs(len) # we must use new args.
    if is_Number(args[1])  # No more than the first item will be a number
        nargs[1] = mpow(args[1],expt)
    else
        nargs[1] = mxpr(:Power,args[1],expt)
    end
    @inbounds for i in 2:len
        nargs[i] = mxpr(:Power,args[i],expt)
    end
    @inbounds for i in 1:len
        setcanon(nargs[i])
    end
    mx = mxpr(:Times,nargs)
    return mx
end

do_canon_power!(mx,base,expt) = mx

## For expressions/objects that are not canonicalized
canonexpr!(x) = x

flatcanon!(x) = canonexpr!(flatten!(x))
#flatcanon!(x) = canonexpr!(x)  # These are for testing speed
#flatcanon!(x) = flatten!(x)
#flatcanon!(x) = x

## Step 3.
# TODO  'compact' not a good name for this function
#################################################################
## Sum collected numerical args in :mplus, (or same for :Times)  #
#################################################################
# + and * are nary. Replace all numbers in the list of args, by one sum or product

compactsumlike!(mx::Mxpr{:Plus}) = compactplus!(mx)
compactsumlike!(mx::Mxpr{:Times}) = compactmul!(mx)

for (op,name,id) in  ((:Plus,:compactplus!,0),(:Times,:compactmul!,1))
    if op == :Times
        fop = :mmul
    else
        fop = :mplus
    end
    @eval begin
        function ($name)(mx::Mxpr)
            @mdebug(2,"In ", $name)
            length(mx) < 2 && return mx
            @mdebug(3, $name, ": length(mx)>1")
            a = margs(mx)
            @mdebug(3, $name, ": got margs")
            typeof(a[1]) <: Number || return mx
            sum0 = a[1]
            while length(a) > 1
                shift!(a)
                typeof(a[1]) <: Number || break
                sum0 = ($fop)(sum0,a[1])
            end
            @mdebug(3, $name, ": done while loop, a=$a, sum0=$sum0")
            (length(a) == 0 || is_type_less(a[1],Number)) && return sum0
            $(fop == :mmul ? :(sum0 == 0 && return 0) : :())
            sum0 != $id && unshift!(a,sum0)
            length(a) == 1 && return a[1]
            @mdebug(3, $name, ": returning at end")
            return mx
        end
    end
end

# Get numeric coefficient of expr. It is 1 if there is none.
function numeric_coefficient(x::Mxpr{:Times})
    local c::Number
    c = is_type_less(x[1],Number) ? x[1] : 1
end
numeric_coefficient(x::Number) = x
numeric_coefficient(x) = 1

# Get numeric exponent; may be 1 (zero never encountered, I hope)
function numeric_expt(x::Mxpr{:Power})
    local c::Number
    c = is_type_less(expt(x),Number) ? expt(x) : 1
end
numeric_expt(x::Number) = 1
numeric_expt(x) = 1

# TODO: Tests fail unless we copy. But, there may be a way around this
function _rest!(mx::Mxpr)
    res=copy(mx)  # could be slow
#    res = mx
    shift!(margs(res))
    return length(res) == 1 ? @ma(res,1) : res
end

# split product n*expr into (n,expr) with numeric n. n may be 1
function numeric_coefficient_and_factor(a)
    n = numeric_coefficient(a)
    res = n == 1 ? (n,a) : (n,_rest!(a))    
    return res
end

function numeric_expt_and_base(a)
    n = numeric_expt(a)
    return n == 1 ? (n,a) : (n,base(a))
end

# Test if two terms differ only by numeric coeffcient,
# and return coefficient and common sub-expression.
# name is too generic.
function _matchterms(a,b)
    (na,a1) = numeric_coefficient_and_factor(a)
    (nb,b1) = numeric_coefficient_and_factor(b)
    return a1 == b1 ? (true,na+nb,a1) : (false,0,a1)
end

# Same as above, but for products
function _matchfacs(a,b)
    (na,a1) = numeric_expt_and_base(a)
    (nb,b1) = numeric_expt_and_base(b)
    return a1 == b1 ? (true,na+nb,a1) : (false,0,a1)
end

## Step 4.
# Replace n repeated terms expr by expr*x, and factors expr by expr^n
# This assumes that terms have been ordered so that collectable
# terms are neighboring.
# FIXME. Here or elsewhere  expr^n * expr^m with m and or n symbolic should
# also go to expr^(n+m)
collectordered!(x) = x
collectordered!(mx::Mxpr{:Plus}) = collectmplus!(mx)
collectordered!(mx::Mxpr{:Times}) = collectmmul!(mx)
for (op,name,matchf) in  ((:mplus,:collectmplus!, :_matchterms),
                          (:mmul,:collectmmul!,:_matchfacs))
    @eval begin
        function ($name)(mx::Mxpr)
            length(mx) < 2 && return mx
            a = margs(mx)
            n = 1
            count = 0
            coeffcount = 0
            while n < length(a)
                is_type_less(a[n],Number) && (n += 1; continue)
                (success,coeffsum,fac) = ($matchf)(a[n],a[n+1])
                if success
                    count = 1
                    coeffcount = coeffsum
                    @inbounds for i in (n+1):(length(a)-1)
                        (success1,coeffsum1,fac1) = ($matchf)(fac,a[i+1])
                        #                        if a[i] == a[i+1]
                        if success1
                            count += 1
                            coeffcount += coeffsum1 - 1
                        else
                            break
                        end
                    end
                    newex = $(op== :mplus ?
                    :(coeffcount == 1 ? fac : mxpr(:Times,coeffcount,fac)) :
                    :(coeffcount == 0 ? 0 : coeffcount == 1 ? fac : mxpr(:Power,fac,coeffcount)))
                    if coeffcount == 0
                        splice!(a,n:n+count)
                    else
                        splice!(a,n:n+count,[newex])
                    end
                end
                n += 1
            end
#            a = mulpowers(a)
            if  length(a) == 1
#                a = mulpowers(a[1])  # BAD
                return a[1]
            end
            if length(a) == 0
                $(op== :mplus ? :(return 0) : :(return 1))
            end
            return mx
        end
    end
end

###########################################################
## Canonical ordering of elements in orderless Mxpr       #
###########################################################

# There are four (more or less) stages:
# 1. Reduced sequences of numbers, leaving perhaps singletons. (We could get those as well)
# 2. Put the terms and factors in a canonical order.
# 3. Evaluate all + and * between singletons which have been sorted to beginning.
# 4. Combine terms with:
#    A. same numeric coefficient
#    B. same numeric power
# 4a.  Sort again

# We try to follow the Mma order for Orderless.

# Anything with a Blank is greater than anything without a Blank.
# This is the natural order for pattern matching.
# Blank, BlankSequence, BlankNullSequence are not less than one another

const _jstypeorder = Dict{DataType,Int}()
#const _jsoporder = Dict{Symbol,Int}()  # maybe more efficient to use this rather than mult dispatch.

# orderless (commutative) functions will have terms ordered from first
# to last according to this order of types. Then lex within types.
function _mklexorder()
    i = 1
    for typ in (Float64,Int,Any,Rational,Symbol,SJSym,Expr,AbstractMxpr)
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
    ! haskey(_jstypeorder,typ)  && return 3  # Any
    return _jstypeorder[typ]
end

# function mxoporder(op::SJSym)
#     ! haskey(_jsoporder,op)  && return 4 # higher
#     return _jsoporder[op]
# end

## FIXME jslexless.  simplify and economize this code.

# For sorting, no name becomes a zero-length string.
blankname(b) = length(b) == 0 ? "" : b[1]

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

for b in ("Blank","BlankSequence","BlankNullSequence","Pattern")
    for o in ("Times","Plus","Power")
        @eval begin
            jslexless(x::Mxpr{symbol($o)},y::Mxpr{symbol($b)}) = false            
            jslexless(x::Mxpr{symbol($b)},y::Mxpr{symbol($o)}) = true
        end
    end
    @eval begin
        jslexless(x::Mxpr, y::Mxpr{symbol($b)}) = false        
        jslexless(x::Mxpr{symbol($b)},y::Mxpr) = true
        jslexless(x::Mxpr{symbol($b)},y::SJSym) = false
        jslexless(x::SJSym, y::Mxpr{symbol($b)}) = true
    end        
end

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

jslexless(x::SJSym, y::SJSym) = isless(x,y)
jslexless(x::Number, y::SJSym) = true
jslexless(x::SJSym, y::Mxpr) = true
jslexless(x::Mxpr, y::SJSym) = false

# Assume args of x and y are sorted. Compare
# last (most significant) element in expression,
# then next-to-last, etc. If x is shorter than y
# and equal to truncated y, then x is jslexless than y.
function jslexless{T}(x::Mxpr{T},y::Mxpr{T})
    x === y && return false
    ax = margs(x)
    ay = margs(y)
    lx = length(ax)
    ly = length(ay)
    ix = lx
    iy = ly
    n = min(lx,ly)
    eqterms_flag = true
    for i in 1:n
        jslexless(ax[ix],ay[iy]) && return true
        if jslexless(ay[iy],ax[ix]) eqterms_flag = false end
        ix -= 1
        iy -= 1
    end
    return eqterms_flag ? lx < ly : false
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

# Order the args in orderless Mxpr.
function orderexpr!(mx::Orderless)
    ar = mx.args
    sort!(ar,lt=jslexless)
    mx
end

# Sum (multiply) sequence of numbers in expression before sorting.
# This only removes one consecutive run of numbers.
for (op,name,id) in  ((:Plus,:plusfirst!,0),(:Times,:mulfirst!,1))
    @eval begin    
        function ($name)(mx::Mxpr, n0::Int)
            args = margs(mx)
            len = length(args)
            n = 0
            for i in n0:len
                if is_Number(args[i])
                    n = i
                    break
                end
            end
            n == 0 && return (mx,len)
            s = $(op == :Plus ? :(zero(args[n]))  :  :(one(args[n])))
            m = 0
            for i in n:len
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

# We say 'sum', but this applies to Times as well
function canonexpr!(mx::Orderless)
    if true
        mx = loopnumsfirst!(mx)  # remove sequences of numbers
        is_Number(mx) && return mx
        orderexpr!(mx)  # sort terms
        if is_type_less(mx,Mxpr)        
            mx = compactsumlike!(mx) # sum numbers not gotten by loopnumsfirst.
            if is_type_less(mx,Mxpr)
                mx = collectordered!(mx)  # collect terms differing by numeric coefficients
                if is_type(mx,Mxpr{:Power}) mx = mulpowers(mx) end  # add numeric exponents when base is same
                if is_type(mx,Orderless)
                    # no test, fails if we remove this. But maybe we forgot a test.
                    #  mx = orderexpr!(mx)  # order again (is this needed ?)
                end
            end
        end
    end
    setcanon(mx)
    mx
end    
canonexpr!(x) = x

# Recursive descent is done by meval; not needed here.
# function deepcanonexpr!(mx::Mxpr)
#     for i = 1:length(mx)
#         if ! is_canon(mx) mx.args[i] = deepcanonexpr!(mx.args[i]) end
#     end
#     mx = canonexpr!(mx)
#     return mx    
# end
# deepcanonexpr!(x) = x


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

# name is too generic.
# Test if two terms differ only by numeric coeffcient,
# and return coefficient and common sub-expression
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

# Is used in canonexpr!
# (x^n)^m --> x^(n*m)
function mulpowers(mx::Mxpr{:Power})
    (e,b) = numeric_expt_and_base(mx)
    (e1,b1) = numeric_expt_and_base(base(mx))
    e != 1 && e1 != 1 && return mxpr(:Power,b1,mmul(e,e1))
    return mx
end
mulpowers(x) = x

# Replace n repeated terms x by n*x, and factors x by x^n
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
                    for i in (n+1):(length(a)-1)
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

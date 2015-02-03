###########################################################
## Canonical ordering of elements in orderless Mxpr       #
###########################################################

# There are three stages:
# 1. Put the terms and factors in a canonical order.
# 2. Evaluate all + and * between numbers and replace with result
# 3. Combine terms with:
#    A. same numeric coefficient
#    B. same numeric power

# Anything with a Blank is greater than anything without a Blank.
# This is the natural order for pattern matching.
# Blank, BlankSequence, BlankNullSequence are not less than one another

# We should also compare to the same algorithms for other
# available CAS's. But, Maxima uses a different order.

typealias Orderless Union(Mxpr{:Plus},Mxpr{:Times})
typealias Blanks Union(Mxpr{:Blank},Mxpr{:BlankSequence},Mxpr{:BlankNullSequence})

## compatibility with old code. fix this later
#is_order_clean(x) = true
set_order_clean!(x) = true

const _jstypeorder = Dict{DataType,Int}()
const _jsoporder = Dict{Symbol,Int}()

#pprintln(x...) = println(x...)
# pprintln(x...) = nothing  # splatting is slow

# orderless (commutative) functions will have terms ordered from first
# to last according to this order of types. Then lex within types.
function _mklexorder()
    i = 1
    for typ in (Float64,Int,Any,Rational,Symbol,SJSym,Expr,AbstractMxpr)
        _jstypeorder[typ] = i
        i += 1
    end
    i = 1
    for op in (:Plus,:Times,:Power)
        _jsoporder[op] = i
        i += 1
    end    
end

_mklexorder()

# interface: returns ordering precedence of Type, typ
function mxtypeorder(typ::DataType)
    ! haskey(_jstypeorder,typ)  && return 3  # Any
    return _jstypeorder[typ]
end

function mxoporder(op::SJSym)
    ! haskey(_jsoporder,op)  && return 4 # higher
    return _jsoporder[op]
end

## FIXME jslexless.  simplify and economize this code.

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
#jslexless(x::Mxpr{:Times}, y::Mxpr{:Power}) = jslexless(x[end],base(y))

## These get most of it
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

# assume args of x and y are sorted
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
#        println("cmp ", ax[ix], "  ", ay[iy])
        jslexless(ax[ix],ay[iy]) && return true
        if jslexless(ay[iy],ax[ix]) eqterms_flag = false end
        ix -= 1
        iy -= 1
    end
#    println("Eqterms ", eqterms_flag)
    return eqterms_flag ? lx < ly : false
#    return lx < ly
    # for i in 1:min(lx,ly)
    #     jslexless(ax[i],ay[i]) && return true
    # end
    # lx < ly && return true
    # return false    
end
jslexless{T,V}(x::Mxpr{T},y::Mxpr{V}) = T < V
jslexless(x::SJSym, y::Mxpr) = true
# Following methods will only be called on non-Symbolic types.
_jslexless(x::DataType,y::DataType) = x <: y
_jslexless{T}(x::T,y::T) = lexless(x,y)  # use Julia definitions
jslexless{T}(x::T,y::T) = !(x === y) &&_jslexless(x,y) 
jslexless{T,V}(x::T,y::V) = mxtypeorder(T) < mxtypeorder(V)

# Order the args in orderless Mxpr.
function orderexpr!(mx::Orderless)
    ar = mx.args
    sort!(ar,lt=jslexless)
    mx
end

# Add numbers in list before sorting
# This only removes one consecutive run of numbers
function sumfirst!(mx::Mxpr{:Plus})
#    println("sumfirst on $mx")
    args = margs(mx)
    len = length(args)
    n = 0
    for i in 1:len
        if is_Number(args[i])
            n = i
            break
        end
    end
    n == 0 && return mx
    s = zero(args[n])
    m = 0
    for i in n:len
        x = args[i]
#        println("$i: trying $x")
        if is_Number(x)
            s = mplus(s,x)
#            println("$x $s")
        else
#            println("Not number")
            m = i
            break
        end
    end
#    println("m=$m, n=$n")
    m == 0 && n == 1 &&  return s
    if m == 0 m = len + 1 end
    splice!(args,n:m-1,[s])
#    println("mx is $mx")
    mx
end

sumfirst!(mx) = mx

# Canonicalize expression
# Sorting is often the slowest part. It is the only significant bottleneck in
# Applying Plus to a big list of  numbers.
# Pulling numbers out of factors and sums first would make a large (eg 1000x) difference
# in cases where there are many numbers.
# Also, we are ordering the numbers, which is a waste, because they will only be
# multiplied or added before the evaluation is done.
# We will not remove compactsumlike! when this is done
function canonexpr!(mx::Orderless)
    if true
        mx = sumfirst!(mx)
        is_Number(mx) && return mx
        orderexpr!(mx)
        if is_type_less(mx,Mxpr)        
            mx = compactsumlike!(mx)
            if is_type_less(mx,Mxpr)
                mx = collectordered!(mx)
                if is_type(mx,Mxpr{:Power}) mx = mulpowers(mx) end
                if is_type(mx,Orderless)
                    mx = orderexpr!(mx)
                end
            end
        end
    end
    setcanon(mx)
#    setfixed(mx)
    mx
end    
canonexpr!(x) = x

# function deepcanonexpr!(mx::Mxpr)
#     @mdebug(5,"deepcanonexpr!: mx = $mx")
#     for i = 1:length(mx)
#         @mdebug(8,"deepcanonexpr!: loop: i=$i, mx[$i] = $(mx[i])")
#         if ! is_canon(mx) mx.args[i] = deepcanonexpr!(mx.args[i]) end
#     end
#     mx = canonexpr!(mx)
#     ## FIXME following should be an assertion
# #    is_rat_and_int(mx) && error("canonexpr!: returning integer rational $mx")
#     return mx    
# end
# deepcanonexpr!(x) = x

#needs_ordering(mx::Mxpr) = get_attribute(mx,:orderless) && ! is_order_clean(mx)

# function order_if_orderless!(mx::Orderless)
#     if needs_ordering(mx)
#         orderexpr!(mx)
#         mx = compactsumlike!(mx)
#     end
#     mx
# end
# order_if_orderless!(x) = x

# # Check the dirty bits of all all orderless subexpressions
# #deep_order_if_orderless!(x) = deepcanonexpr!(x)
# function olddeep_order_if_orderless!(mx::Mxpr)
#     for i = 1:length(mx)
#         @mdebug(10,"deep_order_if_orderless!: loop: i=$i, mx[$i] = $i")
#         @ma(mx,i) = deep_order_if_orderless!(@ma(mx,i))
#     end
#     mx = order_if_orderless!(mx)
#     return mx
# end
#deep_order_if_orderless!(x) = x

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

function numeric_coefficient(x::Mxpr{:Times})
    local c::Number
    c = is_type_less(x[1],Number) ? x[1] : 1
end
numeric_coefficient(x::Number) = x
numeric_coefficient(x) = 1

function numeric_expt(x::Mxpr{:Power})
    local c::Number
    c = is_type_less(expt(x),Number) ? expt(x) : 1
end
numeric_expt(x::Number) = 1
numeric_expt(x) = 1

# TODO: decide whether to copy and propogate the !
# Tests fail unless we copy. But, there may be a way around this
function _rest!(mx::Mxpr)
#    println("rest copy $mx")
    res=copy(mx)  # slow
#    res = mx
    shift!(margs(res))
    return length(res) == 1 ? @ma(res,1) : res
end

#getfac(mx::Mxpr{:Times}) = _rest(mx)
#getfac(mx::Mxpr{:Power}) = base(mx)

function numeric_coefficient_and_factor(a)
    n = numeric_coefficient(a)
    res = n == 1 ? (n,a) : (n,_rest!(a))    
    return res
end

function numeric_expt_and_base(a)
    n = numeric_expt(a)
    return n == 1 ? (n,a) : (n,base(a))
end

function _matchterms(a,b)
    (na,a1) = numeric_coefficient_and_factor(a)
    (nb,b1) = numeric_coefficient_and_factor(b)
    return a1 == b1 ? (true,na+nb,a1) : (false,0,a1)
end

function _matchfacs(a,b)
    (na,a1) = numeric_expt_and_base(a)
    (nb,b1) = numeric_expt_and_base(b)
    return a1 == b1 ? (true,na+nb,a1) : (false,0,a1)
end

# Not used. Done in apprules for Power
# (x^n)^m --> x^(n*m)
function mulpowers(mx::Mxpr{:Power})
    (e,b) = numeric_expt_and_base(mx)
    (e1,b1) = numeric_expt_and_base(base(mx))
    e != 1 && e1 != 1 && return mxpr(:Power,b1,mmul(e,e1))
    return mx
end

mulpowers(x) = x

#Replace n repeated terms x by n*x, and factors x by x^n
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

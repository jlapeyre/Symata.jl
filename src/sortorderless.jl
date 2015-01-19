###########################################################
## Canonical ordering of elements in orderless Mxpr       #
###########################################################

const _jstypeorder = Dict{DataType,Int}()
const _jsoporder = Dict{Symbol,Int}()

# orderless (commutative) functions will have terms ordered from first
# to last according to this order of types. Then lex within types.
function _mklexorder()
    i = 1
    for typ in (Float64,Int,Any,Rational,Symbol,Expr,AbstractMxpr)
        _jstypeorder[typ] = i
        i += 1
    end
    i = 1
    for op in (:mplus,:mmul,:mpow)
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

function mxoporder(op::Symbol)
    ! haskey(_jsoporder,op)  && return 4 # higher
    return _jsoporder[op]
end

## FIXME jslexless. Probably working. But, write a battery of tests first, then
# simplify and economize this code.

function jslexless(x::Mxpr{:mpow}, y::Mxpr{:mpow})
    jslexless(base(x),base(y))  ||  jslexless(expt(x),expt(y))
end
jslexless(x::Mxpr{:mmul}, y::Mxpr{:mmul}) = jslexless(x[end],y[end])
jslexless(x::Mxpr{:mmul}, y::Mxpr{:mpow}) = jslexless(x[end],base(y))
jslexless(x::Mxpr{:mpow}, y::Mxpr{:mmul}) = jslexless(base(x),y[end])
function jslexless(x::Mxpr{:mpow}, y::Mxpr)
    if y == base(x)
        return is_type_less(expt(x),Real) && expt(x) < 0
    end
    jslexless(base(x),y)
end
function jslexless(x::Mxpr, y::Mxpr{:mpow})
    if x == base(y)
        return is_type_less(expt(y),Real) && expt(y) > 1
    end
    jslexless(x,base(y))
end
function jslexless(x::Mxpr{:mpow}, y::Symbol)
    if y == base(x)
        return is_type_less(expt(x),Real) && expt(x) < 0
    end
    jslexless(base(x),y)
end
function jslexless(x::Symbol, y::Mxpr{:mpow})
    if x == base(y)
        return is_type_less(expt(y),Real) && expt(y) > 1
    end
    jslexless(x,base(y))
end
jslexless(x::Mxpr{:mmul}, y::Symbol) = jslexless(x[end],y)
jslexless(x::Symbol, y::Mxpr{:mmul}) = jslexless(x,y[end])
jslexless(x::Mxpr{:mmul}, y::Mxpr) = jslexless(x[end],y)
jslexless(x::Mxpr, y::Mxpr{:mmul}) = jslexless(x,y[end])
jslexless(x::Symbol, y::Mxpr) = true
jslexless(x::Mxpr, y::Symbol) = false
function jslexless{T}(x::Mxpr{T},y::Mxpr{T})
    x === y && return false
    ax = margs(x)
    ay = margs(y)
    lx = length(ax)
    ly = length(ay)
    for i in 1:min(lx,ly)
        jslexless(ax[i],ay[i]) && return true
    end
    lx < ly && return true
    return false    
end
jslexless{T,V}(x::Mxpr{T},y::Mxpr{V}) = T < V
jslexless(x::Symbol, y::Mxpr) = true
# Following methods will only be called on non-Symbolic types.
_jslexless(x::DataType,y::DataType) = x <: y
_jslexless{T}(x::T,y::T) = lexless(x,y)  # use Julia definitions
jslexless{T}(x::T,y::T) = !(x === y) &&_jslexless(x,y) 
jslexless{T,V}(x::T,y::V) = mxtypeorder(T) < mxtypeorder(V)

# Order the args in orderless Mxpr.
function orderexpr!(mx::Orderless)
    ar = jargs(mx)
    sort!(ar,lt=jslexless) # TODO: optimize this after design is more fixed
    set_order_clean(mx)
    mx
end

# Canonicalize expression
function canonexpr!(mx::Orderless)
    orderexpr!(mx)
end    

#needs_ordering(mx::Mxpr) = get_attribute(mx,:orderless) && ! is_order_clean(mx)
needs_ordering(mx::Mxpr) = false
needs_ordering(mx::Orderless) = ! is_order_clean(mx)

function order_if_orderless!(mx::Orderless)
    if needs_ordering(mx)
        orderexpr!(mx)
        mx = compactsumlike!(mx)
    end
    mx
end
order_if_orderless!(x) = x

# Check the dirty bits of all all orderless subexpressions
function deep_order_if_orderless!(mx::Mxpr)
    for i = 1:length(mx)
        @mdebug(10,"deep_order_if_orderless!: loop: i=$i, mx[$i] = $i")
        @ma(mx,i) = deep_order_if_orderless!(@ma(mx,i))
    end
    mx = order_if_orderless!(mx)
    ## FIXME following should be an assertion
    is_rat_and_int(mx) && error("deep_order_if_orderless!: returning integer rational $mx")
    return mx
end
deep_order_if_orderless!(x) = x

# TODO  'compact' not a good name for this function
#################################################################
## Sum collected numerical args in :mplus, (or same for :mmul)  #
#################################################################
# + and * are nary. Replace all numbers in the list of args, by one sum or product

compactsumlike!(mx::Mxpr{:mplus}) = compactplus!(mx)
compactsumlike!(mx::Mxpr{:mmul}) = compactmul!(mx)

for (fop,name,id) in  ((:mplus,:compactplus!,0),(:mmul,:compactmul!,1))
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
            length(a) == 0 && return sum0
            $(fop == :mmul ? :(sum0 == 0 && return 0) : :())
            sum0 != $id && unshift!(a,sum0)
            length(a) == 1 && return a[1]
            @mdebug(3, $name, ": returning at end")
            return mx
        end
    end
end

function numeric_coefficient(x::Mxpr{:mmul})
    local c::Number
    c = is_type_less(x[1],Number) ? x[1] : 1
end
numeric_coefficient(x::Number) = x
numeric_coefficient(x) = 1

# Note this is really getting the numeric exponent
function numeric_coefficient(x::Mxpr{:mpow})
    local c::Number
    c = is_type_less(expt(x),Number) ? expt(x) : 1
end

function _rest(mx::Mxpr)
    res=deepcopy(mx)
    shift!(margs(res))
    return length(res) == 1 ? @ma(res,1) : res
end

getfac(mx::Mxpr{:mmul}) = _rest(mx)
getfac(mx::Mxpr{:mpow}) = base(mx)

function numeric_coefficient_and_factor(a)
    n = numeric_coefficient(a)
    return n == 1 ? (n,a) : (n,getfac(a))
end

function _matchterms(a,b)
    (na,a1) = numeric_coefficient_and_factor(a)
    (nb,b1) = numeric_coefficient_and_factor(b)
    return a1 == b1 ? (true,na+nb,a1) : (false,0,a1)
end

#Replace n repeated terms x by n*x, and factors x by x^n
for (op,name,id) in  ((:mplus,:collectplus!,0),(:mmul,:collectmul!,1))
    opstr = string(op)
    @eval begin
        function ($name)(mx::Mxpr)
            length(mx) < 2 && return mx
            a = margs(mx)
            n = 1
            count = 0
            coeffcount = 0
            while n < length(a)
                (success,coeffsum,fac) = _matchterms(a[n],a[n+1])
                println("start (csum=$coeffsum fac=$fac)")
                if success
#                if a[n] == a[n+1]
                    count = 1
                    coeffcount = coeffsum
                    for i in (n+1):(length(a)-1)
                        (success1,coeffsum1,fac1) = _matchterms(fac,a[i+1])
                        #                        if a[i] == a[i+1]
                        if success1
                            println("next (csum=$coeffsum1 fac1=$fac1)")
                            count += 1
                            coeffcount += coeffsum1 - 1
                        else
                            break
                        end
                    end
#                    println("n=$n, count=$count")
                    newex = $(op== :mplus ?
                              :(coeffcount == 1 ? fac : mxpr(:mmul,coeffcount,fac)) :
                              :(coeffcount == 1 ? fac : mxpr(:mpow,fac,coeffcount)))
                    println("newex $newex")
                    if coeffcount == 0
                        splice!(a,n:n+count)
                    else 
                        splice!(a,n:n+count,[newex])
                    end
                end
                n += 1
            end
            return length(a) == 1 ? a[1] : mx
        end
    end
end

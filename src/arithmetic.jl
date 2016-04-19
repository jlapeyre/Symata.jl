# Divide and multiply integers and rationals
# like a CAS does. Always return exact result (no floats).
# Return Int if possible. We want to avoid infection Rationals
# and putting explicit conversions everywhere. Examples:
#  12/6 --> 2, not 2.0
#  13/6 --> 13//6
#  (13//6) * 6 --> 13
rat_to_int{T<:Integer}(r::Rational{T}) = r.den == 1 ? r.num : r
mmul{T<:Integer}(x::Int, y::Rational{T}) = (res = x * y; return res.den == 1 ? res.num : res )
mmul{T<:Integer}(x::Rational{T}, y::Int) = (res = x * y; return res.den == 1 ? res.num : res )
mmul(x,y) = x * y
mplus{T<:Integer,V<:Integer}(x::Rational{T}, y::Rational{V}) = rat_to_int(x+y)
mplus(x,y) = x + y

# mdiv is apparently used in do_Rational, but this should never be used now.
mdiv{T<:Integer,V<:Integer}(x::T, y::V) =  y == 0 ? ComplexInfinity : rem(x,y) == 0 ? div(x,y) : x // y
mdiv{T<:Integer}(x::Int, y::Rational{T}) = (res = x / y; return res.den == 1 ? res.num : res )
mdiv(x,y) = x/y

mpow{T<:Integer,V<:Integer}(x::T,y::V) = y >= 0 ? x^y : x == 0 ? ComplexInfinity : 1//(x^(-y))
mpow{T<:Real}(x::SJulia.Mxpr{:DirectedInfinity}, y::T) = y > 0 ? x : 0

mpow{T<:Real}(x::SJulia.Mxpr{:DirectedInfinity}, y::Complex{T}) = y.re > 0 ? x : 0

mpow{T<:AbstractFloat,V<:Number}(b::T,exp::V) = b < 0 ? complex(cospi(exp),sinpi(exp)) * abs(b)^exp : b^exp

#mpow(x::Complex,y::Integer) = x^y handled below

# FIXME: This code and
# function do_Power{T<:Integer}(mx::Mxpr{:Power},b::T,e::Rational)
# in apprules are in conflict.
function mpow{T<:Integer, V<:Integer}(x::T,y::Rational{V})
    x == 1 && return x
    local gotneg::Bool
    if x < 0
        x = -x
        gotneg = true
    else
        gotneg = false
    end
    facs = factor(x)
    newfacs = newargs()
    for (fac,mul) in facs
        (q,r) = divrem(mul,y.den)
        if r == 0
            nf = fac^(q*abs(y.num))
            nf != 1 ? y.num > 0 ? push!(newfacs,nf) : push!(newfacs,1//nf) : nothing
            continue
        else
            nf = fac^q
            nf != 1 ? y.num > 0 ? push!(newfacs,nf) : push!(newfacs,1//nf) : nothing
        end
#        r == 0 ? continue : nothing
        (nf1,r1) = divrem(r*y.num,y.den)
        if nf1 != 0
            newfac = fac^nf1
            push!(newfacs,newfac)
        end
        if r1 != 0
            newexp = r1//(y.den)
            push!(newfacs,mxprcf(:Power,fac,newexp))
        end
    end
    if gotneg == true  # This at least not wrong, but it's not like Mma for y < 0
        (n,r) = divrem(y.num,y.den)
        fac = y.den == 2 ? :I : mxprcf(:Power, -1, r//y.den)
        if iseven(n)
            push!(newfacs, fac)
        else
            push!(newfacs,mxprcf(:Times, -1, fac))
        end
    end
    length(newfacs) == 1 && return newfacs[1]
    mxprcf(:Times,newfacs...)  # make this more efficient
end

# Find if this is not called and remove it. Otherwise, get rid of it.
function mpow{T<:Integer, V<:Integer}(x::Rational{T}, y::Rational{V})
    println(STDERR, "arithetic.jl: Calling stupid mow")
    mxpr(:Times,mpow(x.num,y), mxpr(:Power,mpow(x.den,y), -1))
#    Rational(mpow(x.num,y),mpow(y.den,y))
end

mpow(x,y) = x^y

mabs(x) = abs(x)

# TODO.  T Rational
function mabs{T<:Integer}(x::Complex{T})
    r,i = reim(x)
    sq = r*r + i*i
    x = isqrt(sq)
    if x * x == sq
        return x
    else
        res = mxpr(:Power,sq,1//2)
        setfixed(res)
        mergesyms(res,:nothing)
        return res
    end
end

# mpow for Rational is wrong often wrong.
# so this will be broken, too.
function mabs{T<:Integer}(x::Complex{Rational{T}})
    r,i = reim(x)
    sq = mplus(mmul(r,r),mmul(i,i))
    return mpow(sq,1//2)
end

## copied from base/operators, a great trick
for op = (:mplus, :mmul)
    @eval begin
        # note: these definitions must not cause a dispatch loop when +(a,b) is
        # not defined, and must only try to call 2-argument definitions, so
        # that defining +(a,b) is sufficient for full functionality.
        ($op){T<:Number}(a::T, b::T, c::T)        = ($op)(($op)(a,b),c)
        ($op){T<:Number}(a::T, b::T, c::T, xs::T...) = ($op)(($op)(($op)(a,b),c), xs...)
        # a further concern is that it's easy for a type like (Int,Int...)
        # to match many definitions, so we need to keep the number of
        # definitions down to avoid losing type information.
    end
end

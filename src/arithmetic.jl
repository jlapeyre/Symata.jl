# Divide and multiply integers and rationals
# like a CAS does. Always return exact result (no floats).
# Return Int if possible. We want to avoid infection Rationals
# and putting explicit conversions everywhere. Examples:
#  12/6 --> 2, not 2.0
#  13/6 --> 13//6
#  (13//6) * 6 --> 13
rat_to_int(r::Rational) = r.den == 1 ? r.num : r
mmul(x::Int, y::Rational) = (res = x * y; return res.den == 1 ? res.num : res )
mmul(x::Rational, y::Int) = (res = x * y; return res.den == 1 ? res.num : res )
mmul(x,y) = x * y
mplus(x::Rational, y::Rational) = rat_to_int(x+y)
mplus(x,y) = x + y
mdiv(x::Int, y::Int) =  rem(x,y) == 0 ? div(x,y) : x // y
mdiv(x::Int, y::Rational) = (res = x / y; return res.den == 1 ? res.num : res )
mdiv(x,y) = x/y
mpow(x::Integer,y::Integer) = y >= 0 ? x^y : 1//(x^(-y))

# FIXME: currently (3*3)^(2/3) --> 3^(4//3). should be 3 * 3^(1//3)
# (7*7*7)^(1//2) --> 7 * 7^(1//2)
function mpow{T<:Integer}(x::T,y::Rational)
    facs = factor(x)    
    newfacs = newargs()
    for (fac,mul) in facs
        (q,r) = divrem(mul,y.den)
        nf = fac^q
        nf != 1 ? push!(newfacs,nf) : nothing
        newexp = mmul(r,y)
        newexp != 0 ? push!(newfacs,mxpr(:Power,fac,newexp)) : nothing
    end
    length(newfacs) == 1 && return newfacs[1]
    mxpr(:Times,newfacs...)
end

function mpow(x::Rational, y::Rational)
    mxpr(:Times,mpow(x.num,y), mxpr(:Power,mpow(x.den,y), -1))
#    Rational(mpow(x.num,y),mpow(y.den,y))
end

mpow(x,y) = x^y


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

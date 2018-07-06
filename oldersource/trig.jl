## Test implementing some math logic

# The other trig functions are handled by sympy.
# We find than Cos(Pi) is about 50 time faster than Sin(Pi), etc. in some tests
# So it would be good to implement more of this in Julia

# sympy does more at the moment.
@mkapprule Cos :nargs => 1

do_Cos(mx::Mxpr{:Cos},x) = cos_one_arg(mx,x)


# This is overwritten now anyway
# This does not work. But, If I eval it after starting SJulia, it does work
# Should dispatch on number of args, anyway.
# apprules(mx::Mxpr{:Sin}) = length(mx) == 1 ? sin_one_arg(mx,margs(mx)...) : mx

Cos_pi_coeff(mx::Mxpr{:Cos},c::T) where {T<:Integer} = iseven(c) ? 1 : -1


## Do multiples of pi/2, pi/3, and pi/4
function Cos_pi_coeff(mx::Mxpr{:Cos},c::Rational{T}) where T<:Integer
    n = c.num
    d = c.den
    d == 2 && return zero(d)
    if d == 3
        md = mod(n,6)
        if (md == 2 || md == 4)
            nmx = -1//2
        else
            nmx = 1//2
        end
        return nmx
    end
    if d == 4
        md = mod(n,8)
        if (md == 3 || md == 5)
            nmx = _moosq2
        else
            nmx = _oosq2
        end
        return nmx
    end
    return mx
end

Cos_pi_coeff(mx::Mxpr{:Cos},c::T) where {T<:AbstractFloat} = cospi(c)
Cos_pi_coeff(mx::Mxpr{:Cos},c) = mx

function cos_one_arg(mx::Mxpr{:Cos}, arg::Mxpr{:Times})
    if is_Complex(arg[1])  # find a good way to dispatch on type here.
        (r,i) = reim(arg[1])
        if r == 0  #  Cos(I x) -> Cosh(x)
            nargs = copy(margs(arg))
            nargs[1] = i
            return mxpr(:Cosh,mxpr(:Times,nargs))
        end
    end
    return length(arg) == 2 ? Cos_factor_arg(mx,arg.args...) :
    mx
end

# function cos_one_arg2{T<:Real}(mx::Mxpr{:Cos}, arg::Mxpr{:Times}, arg1::Complex{T})
# end
# function cos_one_arg2(mx::Mxpr{:Cos}, arg::Mxpr{:Times}, arg1)
# end

function Cos_factor_arg(mx::Mxpr{:Cos},f1::T,f2::SJSym) where T<:Number
    if f2 == :Pi
        return Cos_pi_coeff(mx,f1)
    else
        return mx
    end
end

Cos_factor_arg(mx::Mxpr{:Cos},f1,f2) = mx
@inline cos_one_arg(mx::Mxpr{:Cos},arg::Symbol) = arg == :Pi ? -1 : mx
@inline cos_one_arg(mx::Mxpr{:Cos},arg::T) where {T<:Integer} = arg == 0 ? 1 : mx
@inline cos_one_arg(mx::Mxpr{:Cos},x::T) where {T<:AbstractFloat} = cos(x)
@inline cos_one_arg(mx::Mxpr{:Cos},x::Complex{T}) where {T<:AbstractFloat} = cos(x)
@inline cos_one_arg(mx::Mxpr{:Cos},x::Mxpr{:ArcCos}) = length(x) == 1 ? x[1] : mx
function cos_one_arg(mx::Mxpr{:Cos},x::Mxpr{:ArcSin})
    res = mpow((1-mpow(x[1],2)),(1//2))
    return res
end
cos_one_arg(mx::Mxpr{:Cos},x) = mx


#### Sin

# This is overwritten somewhere. Does not work


sin_one_arg(mx::Mxpr{:Sin}, arg::T) where {T<:Integer} = arg == 0 ? 0 : mx

function sin_one_arg(mx::Mxpr{:Sin},arg::Mxpr{:Times})
    if is_Complex(arg[1])  # use dispatch!
        (r,i) = reim(arg[1])
        if r == 0  #  Sin(I x) -> I Sinh(x)
            nargs = copy(margs(arg))
            nargs[1] = i
            return mxpr(:Times, complex(0,1), mxpr(:Sinh,mxpr(:Times,nargs)))
        end
    end
#    todo
    mx
end

sin_one_arg(mx::Mxpr{:Sin},x) = mx

####

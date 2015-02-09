## Test implementing some math logic

apprules(mx::Mxpr{:Cos}) = length(mx) == 1 ? cos_one_arg(mx,mx.args[1]) : mx

Cos_pi_coeff(mx::Mxpr{:Cos},c::Integer) = iseven(c) ? 1 : -1

## Making these constant does save a bit of time
# We could do this in an more organized way.
const _moosq2 = mmul(-1,mpow(2,-1//2))
const _oosq2 = mpow(2,-1//2)
mergesyms(_moosq2,:nothing)
mergesyms(_oosq2,:nothing)
setfixed(_moosq2)
setcanon(_moosq2)
setfixed(_oosq2)
setcanon(_oosq2)

function Cos_pi_coeff(mx::Mxpr{:Cos},c::Rational)
    n = c.num
    d = c.den
    d == 2 && return zero(d)
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

Cos_pi_coeff(mx::Mxpr{:Cos},c::FloatingPoint) = cospi(c)
Cos_pi_coeff(mx::Mxpr{:Cos},c) = mx

function cos_one_arg(mx::Mxpr{:Cos},arg::Mxpr{:Times})
    return length(arg) == 2 ? Cos_factor_arg(mx,arg.args...) :
    mx
end
function Cos_factor_arg(mx::Mxpr{:Cos},f1::Number,f2::SJSym)
    if f2 == getsym(:Pi)
        return Cos_pi_coeff(mx,f1)
    else
        return mx
    end
end
Cos_factor_arg(mx::Mxpr{:Cos},f1,f2) = mx
cos_one_arg(mx::Mxpr{:Cos},arg::Symbol) = arg == :Pi ? -1 : mx
cos_one_arg(mx::Mxpr{:Cos},arg::Integer) = arg == 0 ? 0 : mx


cos_one_arg(mx::Mxpr{:Cos},x::FloatingPoint) = cos(x)
cos_one_arg(mx::Mxpr{:Cos},x::Mxpr{:ACos}) = length(x) == 1 ? x[1] : mx
function cos_one_arg(mx::Mxpr{:Cos},x::Mxpr{:ASin})
    res = mpow((1-mpow(x[1],2)),(1//2))
    return res
end
cos_one_arg(mx::Mxpr{:Cos},x) = mx    

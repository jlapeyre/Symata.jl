apprules(mx::Mxpr{:Cos}) = length(mx) == 1 ? meval_one_arg(mx,mx.args[1]) : mx

Cos_pi_coeff(mx::Mxpr{:Cos},c::Integer) = iseven(c) ? 1 : -1
Cos_pi_coeff(mx::Mxpr{:Cos},c::FloatingPoint) = cospi(c)
Cos_pi_coeff(mx::Mxpr{:Cos},c) = mx
function Cos_factor_arg(mx::Mxpr{:Cos},f1::Number,f2::SJSym)
    if f2 == getsym(:Pi)
        return Cos_pi_coeff(mx,f1)
    else
        return mx
    end
end
Cos_factor_arg(mx::Mxpr{:Cos},f1,f2) = mx
meval_one_arg(mx::Mxpr{:Cos},arg::Symbol) = arg == :Pi ? -1 : mx
meval_one_arg(mx::Mxpr{:Cos},arg::Integer) = arg == 0 ? 0 : mx
function meval_one_arg(mx::Mxpr{:Cos},arg::Mxpr{:Times})
    return length(arg) == 2 ? Cos_factor_arg(mx,arg.args...) :
    mx
end

meval_one_arg(mx::Mxpr{:Cos},x::FloatingPoint) = cos(x)
meval_one_arg(mx::Mxpr{:Cos},x::Mxpr{:ACos}) = length(x) == 1 ? x[1] : mx
#function meval_one_arg(mx::Mxpr{:Cos},x::Mxpr{:ASin})
#   mxpr(:Power, ((1-x[1]^2),1//2)
#end
meval_one_arg(mx::Mxpr{:Cos},x) = mx    

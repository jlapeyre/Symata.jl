## Test implementing some math logic

# Symols like Cos should probably be treated not with apprules, but
# rules, for instance.

apprules(mx::Mxpr{:Cos}) = length(mx) == 1 ? cos_one_arg(mx,mx.args[1]) : mx

Cos_pi_coeff(mx::Mxpr{:Cos},c::Integer) = iseven(c) ? 1 : -1

## 1/Sqrt(2) and -1/Sqrt(2)
# Making these constant does save a bit of time
# We could do this in an more organized way.
const _moosq2 = mmul(-1,mpow(2,-1//2))
const _oosq2 = mpow(2,-1//2)
mergesyms(_moosq2,:nothing)
mergesyms(_oosq2,:nothing)
setfixed(_moosq2)
setcanon(_moosq2)
setfixed(_oosq2)
setcanon(_oosq2)

## Do multiples of pi/2 and pi/4
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

####

# Example of using down value

# With the present implmentation, this is slower than
# writing an apprule directly in Julia. Patterns are not optimized
unset_attribute(:Sin,:Protected)
unset_attribute(:Tan,:Protected)
unset_attribute(:Cos,:Protected)

# Some are not evaled far enough.
# Unfix is a workaround for bugs that prevent evaluation
@ex  Sin(-1*x_) := -1 * Sin(x)   # These won't work right until AC matching.
@ex  Tan(-1*x_) := -1 * Tan(x)
@ex  Tan(ACos(x_)) := Unfix(Sqrt(1-x^2)/x)
@ex  Tan(ASin(x_)) := Unfix(x/Sqrt(1-x^2))
@ex  Sin(ASin(x_)) := x
@ex  Sin(ACos(x_)) := Unfix((1-x^2)^(1/2))  
@ex  Cos(-1*x_) := Cos(x)
# Following works together with rules for Cos <---> Cosh, which are not yet written


set_attribute(:Sin,:Protected)
set_attribute(:Cos,:Protected)
set_attribute(:Tan,:Protected)

# Not trig. We can move this.
unset_attribute(:Power,:Protected)

@ex  Exp(Log(x_)) := x
@ex  Power(Cos(x_),-1) := Sec(x)   # These should be upvalues of Cos
@ex  Power(Sec(x_),-1) := Cos(x)
@ex  Power(Sin(x_),-1) := Csc(x)   
@ex  Power(Csc(x_),-1) := Sin(x)
@ex  Power(Tan(x_),-1) := Cot(x)   
@ex  Power(Cot(x_),-1) := Tan(x)


set_attribute(:Power,:Protected)

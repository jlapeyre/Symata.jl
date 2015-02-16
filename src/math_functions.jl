## First pass at math functions. There are more domain restrictions to be implemented.

# Bind SJulia symbols to Julia types so that, eg.
# Head(1.0) == Float64  is true
# But, this breaks the pattern matching code,
# which could be fixed to work this way as well.
# function bind_types()
#     for x in ("Int","Float64","Int64")  # etc.
#         t = Main.eval(parse(x))
#         s = symbol(x)
#         setsymval(s,t)
#     end
# end
# bind_types()

# We want to put things into modules eventually
function evalmath(x)
    Main.eval(x)
end

function make_math()
    single_arg_float_complex =   # check, some of these can't take complex args
#      
        [ (:sin,), (:tan,), (:sind,:SinD), (:cosd,:CosD),(:tand,:TanD),
         (:sinpi,:SinPi), (:cospi,:CosPi), (:sinh,),(:cosh,),
         (:tanh,), (:acos,:ArcCos), (:asin,:ArcSin),
         (:atan,:ArcTan),(:atan2,:ArcTan2),(:acosd,:ArcCosD), (:asind,:ArcSinD),
         (:atand,:ArcTanD),(:sec,),(:csc,),(:cot,),(:secd,:SecD),(:csc,:CscD),(:cot,:CotD),
         (:asec,:ArcSec),(:acsc,:ArcCsc),(:acot,:ArcCot),  # (:acotd,:ArcCotD)
         (:csch,),(:coth,),(:asinh,:ASinh),(:acosh,:ACosh),(:atanh,:ATanh),
         (:asech,:ArcSech),(:acsch,:ArcCsch),(:acoth,:ArcCoth),
         (:sinc,),(:cosc,),(:log,),
         (:log1p,),(:exp2,),(:exp10,),(:expm1,),(:abs2,),
         (:erf,),(:erfc,),(:erfi,),(:erfcx,),(:dawson,),(:real,:Re),(:imag,:Im),
         (:conj,:Conjugate),(:angle,:Arg),(:cis,),(:gamma,),(:lgamma,:LogGamma),
         (:lfact,:LogFactorial),(:digamma,),(:trigamma,),(:airyai,:AiryAi),
         (:airybi,:AiryBi),(:airyaiprime,:AiryAiPrime),(:airybiprime,:AiryBiPrime),
         (:besselj0,:BesselJ0),(:besselj1,:BesselJ1),(:bessely0,:BesselY0),(:bessely1,:BesselY1),
         (:eta,),(:zeta,)
         
         ]

    single_arg_float = [(:sign,),(:signbit,),(:cbrt,),(:erfinv,:ErfInv),(:erfcinv,:ErfcInv),(:invdigamma,:InvDigamma)
                        ]

    single_arg_float_int = [(:factorial,)]

    single_arg_int = [(:isqrt,:ISqrt),(:ispow2,:IsPow2),(:nextpow2,:NextPow2),(:prevpow2,:PrevPow2),
                      (:isprime,:PrimeQ)
                        ]    

    two_arg_int = [(:binomial,),(:ndigits,:NDigits)
                   ]

    two_arg_float_and_float_or_complex =
     [
      (:besselj,:BesselJ),(:besseljx,:BesselJx),(:bessely,:BesselY),
      (:besselyx,:BesselYx),(:hankelh1,:HankelH1),(:hankelh1x,:HankelH1x),
      (:hankelh2,:HankelH2),(:hankelh2x,:HankelH2x),(:besseli,:BesselI),
      (:besselix,:BesselIx),(:besselk,:BesselK),(:besselkx,:BesselKx),
      ]


    two_arg_float = [ (:beta,),(:lbeta,:LogBeta),(:hypot,)]

# two arg both float or complex : zeta(s,z)  (with domain restrictions)
    
    for x in single_arg_float_complex
        jf,sjf = get_sjstr(x)
        do_common(sjf)
        aprs2 = "do_$sjf(mx::Mxpr{:$sjf},x::FloatingPoint) = $jf(x)"
        aprs3 = "do_$sjf{T<:FloatingPoint}(mx::Mxpr{:$sjf},x::Complex{T}) = $jf(x)"
        evalmath(parse(aprs2))
        evalmath(parse(aprs3))
    end

    for x in single_arg_float
        jf,sjf = get_sjstr(x)
        do_common(sjf)
        aprs2 = "do_$sjf(mx::Mxpr{:$sjf},x::FloatingPoint) = $jf(x)"
        evalmath(parse(aprs2))
    end

    for x in single_arg_float_int
        jf,sjf = get_sjstr(x)
        do_common(sjf)
        aprs2 = "do_$sjf(mx::Mxpr{:$sjf},x::Real) = $jf(x)" # may not work for rational
        evalmath(parse(aprs2))
    end

    for x in single_arg_int
        jf,sjf = get_sjstr(x)
        do_common(sjf)
        aprs2 = "do_$sjf(mx::Mxpr{:$sjf},x::Integer) = $jf(x)"
        evalmath(parse(aprs2))
    end

    for x in two_arg_int
        jf,sjf = get_sjstr(x)
        do_common(sjf)
        aprs2 = "do_$sjf(mx::Mxpr{:$sjf},x::Integer,y::Integer) = $jf(x,y)"
        evalmath(parse(aprs2))
    end

    for x in two_arg_float
        jf,sjf = get_sjstr(x)
        do_common(sjf)
        aprs2 = "do_$sjf{T<:FloatingPoint,V<:FloatingPoint}(mx::Mxpr{:$sjf},x::T,y::V) = $jf(x,y)"
        evalmath(parse(aprs2))
    end    

    for x in two_arg_float_and_float_or_complex
        jf,sjf = get_sjstr(x)
        do_common(sjf)
        aprs2 = "do_$sjf(mx::Mxpr{:$sjf},x::FloatingPoint,y::FloatingPoint) = $jf(x,y)"
        aprs3 = "do_$sjf{T<:FloatingPoint}(mx::Mxpr{:$sjf},x::FloatingPoint,y::Complex{T}) = $jf(x,y)"
        evalmath(parse(aprs2))
        evalmath(parse(aprs3))
    end                    

end

function get_sjstr(x)
    if length(x) == 1
        jf = x[1]
        st = string(jf)
        sjf = uppercase(string(st[1])) * st[2:end]
    else
        (jf,sjf) = x
    end
    return jf,sjf
end

function do_common(sjf)
    aprs = "Main.apprules(mx::Mxpr{:$sjf}) = do_$sjf(mx,margs(mx)...)"
    aprs1 = "do_$sjf(mx::Mxpr{:$sjf},x...) = mx"
    evalmath(parse(aprs))
    evalmath(parse(aprs1))
    set_attribute(symbol(sjf),:Protected)
    set_attribute(symbol(sjf),:Listable)    
end

make_math()

do_Abs2(mx::Mxpr{:Abs2},x::Integer) = x*x
do_Abs2{T<:Integer}(mx::Mxpr{:Abs2},z::Complex{T}) = ((x,y) = reim(z); x*x + y*y)

@sjdoc IntegerDigits "
IntegerDigits(n,[, base][, pad]) Returns an array of the digits of \"n\" in the given base,
optionally padded with zeros to a specified size. In contrast to Julia, more significant
digits are at lower indexes.
"

do_common("IntegerDigits")
#apprules(mx::Mxpr{:IntegerDigits}) = do_IntegerDigits(mx,margs(mx)...)
#do_IntegerDigits(mx,args...) = mx
do_IntegerDigits(mx::Mxpr{:IntegerDigits},n::Integer) = setfixed(mxpr(:List,reverse!(digits(n))...))
do_IntegerDigits(mx::Mxpr{:IntegerDigits},n::Integer,b::Integer) = setfixed(mxpr(:List,reverse!(digits(n,b))...))
do_IntegerDigits(mx::Mxpr{:IntegerDigits},n::Integer,b::Integer,p::Integer) = setfixed(mxpr(:List,reverse!(digits(n,b,p))...))

@sjdoc Primes "
Primes(n) returns a collection of the prime numbers <= \"n\"
"
apprules(mx::Mxpr{:Primes}) = do_Primes(mx,margs(mx)...)
do_Primes(mx,args...) = mx
do_Primes(mx,n::Integer) = setfixed(mxpr(:List,primes(n)...))

do_NDigits(mx::Mxpr{:NDigits},n::Integer) = ndigits(n)

unprotect(:Zeta)
@ex Zeta(1) := ComplexInfinity
protect(:Zeta)

do_Log{T<:FloatingPoint}(mx::Mxpr{:Log},b::Integer,z::Complex{T}) = log(b,z)
do_Log{T<:FloatingPoint}(mx::Mxpr{:Log},b::Integer,z::T) = log(b,z)

# This is probably quite slow, but might be correct in many cases
# The same idea could be used for other functions, such as sqrts etc.
function do_Log(mx::Mxpr{:Log},b::Integer,x::Integer)
    res = int(log(b,x))
    return b^res == x ? res : mx
end
do_Log(mx::Mxpr{:Log},pow::Mxpr{:Power}) = do_Log(mx,pow,base(pow),expt(pow))
do_Log(mx::Mxpr{:Log},pow::Mxpr{:Power},b,e) = mx
do_Log(mx::Mxpr{:Log},pow::Mxpr{:Power},b::SJSym,e::Integer) = b == :E ? e : mx

@sjdoc N "
N(expr) tries to give a the numerical value of expr.
N(expr,p) tries to give p decimal digits of precision.
"

function apprules(mx::Mxpr{:N})
    do_N(margs(mx)...)
end

do_N(x,dig) = x
do_N(x) = x
do_N(n::Integer) = float(n)
do_N(n::Rational) = float(n)

function do_N(n::Integer,p::Integer)
    float_with_precision(n,p)
end
function do_N(n::Rational,p::Integer)
    float_with_precision(n,p)
end


# p is number of decimal digits of precision
# Julia doc says set_bigfloat_precision uses
# binary digits, but it looks more like decimal.
#
# The length of the string printed is about
# p digits long if we do set_bigfloat_precision(p).
# Or not sometimes. Don't know what is happening.
# propagation of precision with operations or something.
#
# One problem is that bigfloat arithmetic does not use the
# precision of the input types, but rather the current working
# precision.
# So N(2*Pi,1000) does not do what we want.
# It may be expensive to change it on the fly.
function float_with_precision(x,p)
    if p > 16
        pr = get_bigfloat_precision()
        dig = int(p*3.322)  
        # dig = int(p)        
        set_bigfloat_precision(dig)
        res = BigFloat(x)
        set_bigfloat_precision(pr)
        return res
    else
        return float(x)
    end    
end

function do_N(m::Mxpr)
    len = length(m)
    args = margs(m)
    nargs = newargs(len)
    for i in 1:len
        nargs[i] = do_N(args[i])
    end
    return mxpr(mhead(m),nargs)
end

function do_N(m::Mxpr,p::Integer)
    len = length(m)
    args = margs(m)
    nargs = newargs(len)
    for i in 1:len
        nargs[i] = do_N(args[i],p)
    end
    return mxpr(mhead(m),nargs)
end

# We need to use dispatch as well, not conditionals
function do_N(s::Symbol)
    if s == :Pi || s == :π
        return float(pi)
    elseif s == :E
        return float(e)
    elseif s == :EulerGamma
        return float(eulergamma)
    end    
    return s
end

function do_N(s::SJSym,pr::Integer)
    if s == :Pi || s == :π
        return float_with_precision(pi,pr)
    elseif s == :E
        return float_with_precision(e,pr)
    elseif s == :EulerGamma
        return float_with_precision(eulergamma,pr)
    end    
    return s
end

@sjdoc Precision "
Precsion(x) gets the precision of a floating point number x, as defined by the
effective number of bits in the mantissa.
"
apprules(mx::Mxpr{:Precision}) = do_Precision(mx::Mxpr{:Precision},margs(mx)...)
do_Precision(mx::Mxpr{:Precision},args...) = mx
do_Precision(mx::Mxpr{:Precision},x::FloatingPoint) = precision(x)

# apprules(mx::Mxpr{:Numerator}) = do_Numerator(mx::Mxpr{:Numerator},margs(mx)...)
# do_Numerator(mx::Mxpr{:Numerator},args...) = mx
# function do_Numerator(mx::Mxpr{:Numerator},m::Mxpr{:Times})
#     nargsn = newargs()
#     nargsd = newargs()
#     args = margs(m)
#     for i in 1:length(args)
#         res = find_numerator(args[i])
#         if res
#             push!(nargsn,res)
#         else 
#             res = find_denominator(args[i])
#             if res
#                 push!(nargsd,res)
#             else
#                 push!(nargsn,args[i])
#             end
#         end
#     end
#     mxpr(mhead(m),nargsn)
# end

# find_numerator(x::Rational) = num(x)
# find_numerator(x::Power) = 
# find_numerator(x) = false


nothing

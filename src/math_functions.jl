## First pass at math functions. There are more domain restrictions to be implemented.

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
         (:sinc,),(:cosc,),
         # treat log separately
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

# two arg float: beta, lbeta
# two arg both float or complex : zeta(s,z)  (with domain restrictions)
    
    for x in single_arg_float_complex
        jf,sjf = get_sjstr(x)
        do_common(sjf)
        aprs2 = "do_$sjf(mx::Mxpr{:$sjf},x::FloatingPoint) = $jf(x)"
        aprs3 = "do_$sjf{T<:FloatingPoint}(mx::Mxpr{:$sjf},x::Complex{T}) = $jf(x)"
        eval(parse(aprs2))
        eval(parse(aprs3))
    end

    for x in single_arg_float
        jf,sjf = get_sjstr(x)
        do_common(sjf)
        aprs2 = "do_$sjf(mx::Mxpr{:$sjf},x::FloatingPoint) = $jf(x)"
        eval(parse(aprs2))
    end

    for x in single_arg_float_int
        jf,sjf = get_sjstr(x)
        do_common(sjf)
        aprs2 = "do_$sjf(mx::Mxpr{:$sjf},x::Real) = $jf(x)" # may not work for rational
        eval(parse(aprs2))
    end

    for x in single_arg_int
        jf,sjf = get_sjstr(x)
        do_common(sjf)
        aprs2 = "do_$sjf(mx::Mxpr{:$sjf},x::Integer) = $jf(x)"
        eval(parse(aprs2))
    end

    for x in two_arg_int
        jf,sjf = get_sjstr(x)
        do_common(sjf)
        aprs2 = "do_$sjf(mx::Mxpr{:$sjf},x::Integer,y::Integer) = $jf(x,y)"
        eval(parse(aprs2))
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
    aprs = "apprules(mx::Mxpr{:$sjf}) = do_$sjf(mx,margs(mx)...)"
    aprs1 = "do_$sjf(mx::Mxpr{:$sjf},x...) = mx"
    eval(parse(aprs))
    eval(parse(aprs1))
    set_attribute(symbol(sjf),:Protected)
    set_attribute(symbol(sjf),:Listable)    
end

make_math()

do_Abs2(mx::Mxpr{:Abs2},x::Integer) = x*x
do_Abs2{T<:Integer}(mx::Mxpr{:Abs2},z::Complex{T}) = ((x,y) = reim(z); x*x + y*y)

apprules(mx::Mxpr{:IntegerDigits}) = do_IntegerDigits(mx,margs(mx)...)
do_IntegerDigits(mx,args...) = mx
do_IntegerDigits(mx,n::Integer) = setfixed(mxpr(:List,reverse!(digits(n))...))
do_IntegerDigits(mx,n::Integer,b::Integer) = setfixed(mxpr(:List,reverse!(digits(n,b))...))
do_IntegerDigits(mx,n::Integer,b::Integer,p::Integer) = setfixed(mxpr(:List,reverse!(digits(n,b,p))...))


apprules(mx::Mxpr{:Primes}) = do_Primes(mx,margs(mx)...)
do_Primes(mx,args...) = mx
do_Primes(mx,n::Integer) = setfixed(mxpr(:List,primes(n)...))

nothing

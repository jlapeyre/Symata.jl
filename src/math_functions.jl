## First pass at math functions. There are more domain restrictions to be implemented.

### SetPrecision

@sjseealso_group(N, SetPrecision, GetPrecison, BigFloatInput, BigIntInput, BI, BF)

@mkapprule SetPrecision :nargs => 1
@sjdoc SetPrecision """
    SetPrecison(n)

Set the precsion of BigFloat numbers to `n` decimal digits. If `N` does not give the result you
want, you can use `SetPrecision`.
"""
@doap function SetPrecision(n::Real)
    setprecision(round(Int,3.322*n))
    return n
end

### GetPrecision

@mkapprule GetPrecision :nargs => 0
@sjdoc GetPrecision """
    GetPrecision(n)

Get the precsion of BigFloat numbers.
"""
@doap function GetPrecision()
    return precision(BigFloat)
end

"""
    evalmath(expr::Expr)

Evaluate `expr`. This is used for writing Symata interfaces
to math functions. Currently, it is equivalent to `eval`.
"""
evalmath(expr::Expr) = Symata.eval(expr::Expr)

## We also need to use these to convert Symata expressions to Julia
## This function writes apprules for math functions. Usally dispatches floating point
## args to Julia functions. Some have a fall through to SymPy
## These tuples have 1,2, or 3 members. Symbols are for Julia, Symata, and SymPy
## If only one member is present, the second is constructed by putting an inital capital on the first.
## If only one or two members are present, we do not fall back to SymPy.
## The last list has tuples with 2 args for which we do not use any Julia function.

# Typical symbols for Julia, Symata, SymPy
function mtr(sym::Symbol)
    s = string(sym)
    sjf = uppercasefirst(s)
    (sym, Symbol(sjf), sym)
end

# We choose not to implement :cis. Julia has it. sympy and mma, not

# List of not yet implemented sympy functions from keys(sympy.functions)
# :E1,:Ei,:FallingFactorial,:Heaviside, :Id,
# :Max,:Min,:Piecewise,
#  :Ynm_c,:Znm,
# :__builtins__,:__doc__,:__file__,:__name__,:__package__,:__path__,
# :acos,:acosh,:acot,:acoth,:acsc,:adjoint,
#  :assoc_laguerre,:assoc_legendre,
#  :bspline_basis, :bspline_basis_set, :ceiling,:chebyshevt_root,
# :chebyshevu_root,:combinatorial
#  :elementary,
#  :exp_polar, :factorial2,:ff
# :floor, :hyper,:im, :jacobi_normalized, :jn, :jn_zeros,
# :periodic_argument,:piecewise_fold,:polar_lift,
# :principal_branch,:re,:real_root,:rf,:root, :special,
# :transpose,:unbranched_argument,:yn

# TODO: FIX: sympy zeta may take two args
# two arg both float or complex : zeta(s,z)  (with domain restrictions)
# BellB should be split into BellB and BellY

# TODO: Implement rewrite
# sympy.airyai(x)[:rewrite](sympy.hyper)
# PyObject -3**(2/3)*x*hyper((), (4/3,), x**3/9)/(3*gamma(1/3)) + 3**(1/3)*hyper((), (2/3,), x**3/9)/(3*gamma(2/3))

# TODO: rename these arrays. Name by number of args plus identifying number.
# ie. two_args1, two_args2. Then explain in comment the particulars. A naming
# scheme to capture all possibilities is to cumbersome.

#  mtr(:exp)  <-- trying with and without this
   const single_arg_float_complex =   # check, some of these can't take complex args
    [ mtr(:sin), mtr(:cos), mtr(:tan), (:sind,:SinD), (:cosd,:CosD),(:tand,:TanD),
         (:sinpi,:SinPi), (:cospi,:CosPi), mtr(:sinh), mtr(:cosh),
         mtr(:tanh), (:acos,:ArcCos,:acos), (:asin,:ArcSin,:asin),
         (:atan,:ArcTan,:atan),(:acosd,:ArcCosD), (:asind,:ArcSinD),
         (:atand,:ArcTanD), mtr(:sec), mtr(:csc), mtr(:cot),(:secd,:SecD),(:csc,:CscD),(:cot,:CotD),
         (:asec,:ArcSec),(:acsc,:ArcCsc),(:acot,:ArcCot),  # (:acotd,:ArcCotD)
         (:csch,), mtr(:coth,),(:asinh,:ASinh,:asinh), (:acosh,:ACosh, :acosh),(:atanh,:ATanh, :atanh),
         (:asech,:ArcSech),(:acsch,:ArcCsch),(:acoth,:ArcCoth, :acoth),
         (:cosc,),
         (:log1p,),(:exp2,),(:exp10,),(:expm1,),(:abs2,),
         mtr(:erfc), mtr(:erfi),(:erfcx,),(:dawson,),(:real,:Re, :re),(:imag,:Im, :im),
         (:angle,:Arg,:arg),  (:lgamma, :LogGamma, :loggamma),
         (:lfact,:LogFactorial), mtr(:digamma), mtr(:trigamma),
         (:airyai,:AiryAi,:airyai),
         (:airybi,:AiryBi,:airybi),(:airyaiprime,:AiryAiPrime,:airyaiprime),(:airybiprime,:AiryBiPrime,:airybiprime),
         (:besselj0,:BesselJ0),(:besselj1,:BesselJ1),(:bessely0,:BesselY0),(:bessely1,:BesselY1),
         (:zeta,:Zeta,:zeta)
#         (:eta,:DirichletEta,:dirichlet_eta)
         ]

# (:log,),   removed from list above, because it must be treated specially (and others probably too!)


const single_arg_float_int_complex = [(:sinc,)]

const single_arg_float = [(:erfcinv,:InverseErfc,:erfcinv),(:invdigamma,:InverseDigamma)]

# FIXME  Ceiling should probably return integer type. So it can't be included in generic code here (or punt to sympy for everything)
    const single_arg_float_int = [(:signbit,:SignBit), (:ceil, :Ceiling, :ceiling), (:floor, :Floor, :floor)]

#  (:factorial,:Factorial, :factorial), <--- we do this by hand

    const single_arg_int = [(:isqrt,:ISqrt),(:ispow2,:IsPow2),(:nextpow2,:NextPow2),(:prevpow2,:PrevPow2),
                      (:isprime,:PrimeQ), (:(~), :BitNot)
                        ]


    const two_arg_int = [(:binomial,:Binomial,:binomial)
                         ]

# Do NDigits by hand for now!
@mkapprule NDigits :nargs => 1:2
@doap NDigits(n::T,b::V) where {T<:Integer,V<:Integer} = ndigits(n, base=b)
@doap NDigits(n::T) where {T<:Integer} = ndigits(n)
@doap NDigits(n) = mx
@doap NDigits(n,b) = mx

# TODO
#  Two or more args
#   (:gcd,:GCD), (:lcm, :LCM), Also sympy does these for polynomials

# Complicated!
    const one_or_two_args1 = [(:polygamma, :PolyGamma, :polygamma)]

    const two_arg_float_and_float_or_complex =
     [
      (:besselj,:BesselJ, :besselj), (:besseljx,:BesselJx), (:bessely,:BesselY,:bessely),
      (:besselyx,:BesselYx), (:hankelh1,:HankelH1,:hankel1), (:hankelh1x,:HankelH1x),
      (:hankelh2,:HankelH2,:hankel2), (:hankelh2x,:HankelH2x), (:besseli,:BesselI, :besseli),
      (:besselix,:BesselIx), (:besselk,:BesselK,:besselk), (:besselkx,:BesselKx),
       (:atan2,:ArcTan2,:atan2)
      ]

    const  two_arg_float = [ (:beta,),(:lbeta,:LogBeta),(:hypot,)]

    ## There are no Julia functions for these (or at least we are not using them).
    ## First symbol is for  Symata, Second is SymPy

# TODO: Migrate functions out this non-specific list
    const no_julia_function = [  (:ExpIntegralE, :expint),
                           (:GegenbauerC, :gegenbauer), (:HermiteH, :hermite),
                           (:BellB, :bell), (:BernoulliB, :bernoulli), (:CatalanNumber, :catalan),
                           (:EulerE, :euler), (:Subfactorial, :subfactorial), (:Factorial2, :factorial2),
                           (:FactorialPower, :FallingFactorial), (:Pochhammer, :RisingFactorial),
 (:LeviCivita, :LeviCivita),
(:KroneckerDelta, :KroneckerDelta),  (:HypergeometricPFQ, :hyper), (:FactorSquareFree, :sqf), (:MellinTransform, :mellin_transform),
(:InverseMellinTransform, :inverse_mellin_transform), (:SineTransform, :sine_transform), (:InverseSineTransform, :inverse_sine_transform),
(:CosineTransform, :cosine_transform), (:InverseCosineTransform, :inverse_cosine_transform),
(:HankelTransform, :hankel_transform), (:InverseHankelTransform, :inverse_hankel_transform)
]
# LeviCivita is different than in mathematica

# Mobius needs filtering
const no_julia_function_one_arg = [ (:EllipticK, :elliptic_k), (:HeavisideTheta,:Heaviside),
                                    (:CosIntegral, :Ci), (:SinIntegral, :Si),
                                    (:FresnelC, :fresnelc), (:FresnelS, :fresnels), (:DiracDelta, :DiracDelta),
                                    (:LogIntegral, :Li), (:Ei, :Ei), (:ExpandFunc, :expand_func), (:Denominator, :denom),
                                    (:MoebiusMu, :mobius), (:EulerPhi, :totient), (:Divisors, :divisors), (:DivisorCount, :divisor_count),
                                     (:SinhIntegral, :Shi),(:CoshIntegral, :Chi) ]

# elliptic_e take more than ints!
const no_julia_function_one_or_two_int = [ (:HarmonicNumber, :harmonic) , (:EllipticE, :elliptic_e) ]

# FIXME: DivisorSigma has arg order reversed in Mma
const no_julia_function_two_args = [(:LegendreP, :legendre_poly), (:EllipticF, :elliptic_f),
                                    (:ChebyshevT, :chebyshevt), (:ChebyshevU, :chebyshevu),
                                    (:Cyclotomic, :cyclotomic_poly), (:SwinnertonDyer, :swinnerton_dyer_poly),
                                    (:SphericalBesselJ, :jn), (:SphericalBesselY, :yn), (:DivisorSigma, :divisor_sigma)
#                                    (:PolyLog, :polylog), (:SphericalBesselJ, :jn), (:SphericalBesselY, :yn), (:DivisorSigma, :divisor_sigma)
                                    ]

const no_julia_function_two_or_three_args = [ (:EllipticPi, :elliptic_pi), (:LaguerreL, :laguerre_poly)]

const no_julia_function_three_args = [ (:LerchPhi, :lerchphi) ]

const no_julia_function_four_args = [ (:JacobiP, :jacobi), (:SphericalHarmonicY, :Ynm) ]

function make_math()

# Note: in Mma and Julia, catalan and Catalan are Catalan's constant. In sympy catalan is the catalan number
# can't find :stirling in sympy

    for x in no_julia_function_one_arg
        sjf = x[1]
        eval(macroexpand(Symata, :( @mkapprule $sjf)))
        write_sympy_apprule(x[1],x[2],1)
    end


    for x in no_julia_function_two_args
        sjf = x[1]
        eval(macroexpand(Symata, :( @mkapprule $sjf :nargs => 2)))
        write_sympy_apprule(x[1],x[2],2)
    end

    for x in no_julia_function_four_args
        nargs = 4
        sjf = x[1]
        eval(macroexpand(Symata, :( @mkapprule $sjf :nargs => $nargs)))
        write_sympy_apprule(x[1],x[2],nargs)
    end

    for x in no_julia_function_three_args
        nargs = 3
        sjf = x[1]
        eval(macroexpand(Symata, :( @mkapprule $sjf :nargs => $nargs)))
        write_sympy_apprule(x[1],x[2],nargs)
    end

    for x in no_julia_function_two_or_three_args
        sjf = x[1]
        eval(macroexpand(Symata, :( @mkapprule $sjf)))
        write_sympy_apprule(x[1],x[2],2)
        write_sympy_apprule(x[1],x[2],3)
    end


    for x in no_julia_function_one_or_two_int
        sjf = x[1]
        eval(macroexpand(Symata, :( @mkapprule $sjf)))
        write_sympy_apprule(x[1],x[2],1)
        write_sympy_apprule(x[1],x[2],2)
    end

    # TODO: update this code
    for x in no_julia_function
        set_up_sympy_default(x...)
        clear_attributes(x[1]) ## FIXME: Listable was set in previous line. Now we unset it. We need a better system for this.
        set_sysattributes(x[1])
    end

    # Ok, this works. We need to clean it up
    for x in single_arg_float_complex
        jf,sjf = only_get_sjstr(x...)
        eval(macroexpand(Symata, :( @mkapprule $sjf :nargs => 1 )))
        write_julia_numeric_rule(jf,sjf,"AbstractFloat")
        write_julia_numeric_rule(jf,sjf,"CAbstractFloat")
        if length(x) == 3
            write_sympy_apprule(sjf,x[3],1)
        end
        set_attribute(Symbol(sjf),:Listable)
    end

    for x in single_arg_float
        jf,sjf = only_get_sjstr(x...)
        eval(macroexpand(Symata, :( @mkapprule $sjf :nargs => 1 )))
        write_julia_numeric_rule(jf,sjf,"AbstractFloat")
        if length(x) == 3
            write_sympy_apprule(sjf,x[3],1)
        end
        set_attribute(Symbol(sjf),:Listable)
    end

    for x in single_arg_float_int
        jf,sjf = only_get_sjstr(x...)
        eval(macroexpand(Symata, :( @mkapprule $sjf :nargs => 1 )))
        write_julia_numeric_rule(jf,sjf,"Real")
        if length(x) == 3
            write_sympy_apprule(sjf,x[3],1)
        end
    end

    # This is all numbers, I suppose
    for x in single_arg_float_int_complex
        jf,sjf = only_get_sjstr(x...)
        eval(macroexpand(Symata, :( @mkapprule $sjf :nargs => 1 )))
        write_julia_numeric_rule(jf,sjf,"Real")
        write_julia_numeric_rule(jf,sjf,"CReal")
        if length(x) == 3
            write_sympy_apprule(sjf,x[3],1)
        end
    end

    for x in single_arg_int
        jf,sjf = only_get_sjstr(x...)
        eval(macroexpand(Symata, :( @mkapprule $sjf :nargs => 1 :argtypes => [Integer] )))
        write_julia_numeric_rule(jf,sjf,"Integer")
        if length(x) == 3
            write_sympy_apprule(sjf,x[3],1)
        end
    end

    for x in two_arg_int
        jf,sjf = only_get_sjstr(x...)
        eval(macroexpand(Symata, :( @mkapprule $sjf :nargs => 2 )))
        write_julia_numeric_rule(jf,sjf,"Integer", "Integer")
        if length(x) == 3
            write_sympy_apprule(sjf,x[3],2)
        end
    end

    # Mma allows one arg, as well
    for x in one_or_two_args1
        jf,sjf = only_get_sjstr(x...)
        eval(macroexpand(Symata, :( @mkapprule $sjf )))
        write_julia_numeric_rule(jf,sjf,"Integer", "AbstractFloat")
        write_julia_numeric_rule(jf,sjf,"Integer", "CAbstractFloat")
        if length(x) == 3
            write_sympy_apprule(sjf,x[3],2)
        end
    end

    for x in two_arg_float
        jf,sjf = only_get_sjstr(x...)
        eval(macroexpand(Symata, :( @mkapprule $sjf :nargs => 2 )))
        write_julia_numeric_rule(jf,sjf,"AbstractFloat", "AbstractFloat")
    end

    for x in two_arg_float_and_float_or_complex
        jf,sjf = only_get_sjstr(x...)
        eval(macroexpand(Symata, :( @mkapprule $sjf :nargs => 2 )))
        write_julia_numeric_rule(jf,sjf,"AbstractFloat", "AbstractFloat")
        write_julia_numeric_rule(jf,sjf,"AbstractFloat", "CAbstractFloat")
        write_julia_numeric_rule(jf,sjf,"Integer", "AbstractFloat")
        write_julia_numeric_rule(jf,sjf,"Integer", "CAbstractFloat")
        if length(x) == 3
            write_sympy_apprule(sjf,x[3],2)
        end
    end
end

Typei(i::Int)  =  "T" *string(i)
complex_type(t::AbstractString) = "Complex{" * t * "}"
function write_julia_numeric_rule(jf, sjf, types...)
    annot = join(AbstractString[ "T" * string(i) * "<:" *
                                 (types[i][1] == 'C' ? types[i][2:end] : types[i])  for i in 1:length(types)], ", ")
    protargs = join(AbstractString[ "x" * string(i) * "::" *
                                (types[i][1] == 'C' ? complex_type(Typei(i)) : Typei(i))
                                    for i in 1:length(types)], ", ")
    callargs = join(AbstractString[ "x" * string(i) for i in 1:length(types)], ", ")
#    appstr = "do_$sjf{$annot}(mx::Mxpr{:$sjf},$protargs) = $jf($callargs)"
    appstr = "do_$sjf(mx::Mxpr{:$sjf},$protargs) where {$annot} = $jf($callargs)"
    eval(Meta.parse(appstr))
end

function only_get_sjstr(jf,sjf,args...)
    return jf, sjf
end

function only_get_sjstr(jf)
    st = string(jf)
    sjf = Symbol(uppercasefirst(st))
    return jf, sjf
end

function get_sjstr(jf,sjf)
    do_common(sjf)
    return jf, sjf
end

function get_sjstr(jf)
    st = string(jf)
    sjf = String(uppercasefirst(st))
    do_common(sjf)
    return jf, sjf
end

## FIXME: This is outdated. Some of this is handled in @mkapprule
# Handle functions that do *not* fall back on SymPy
function do_common(sjf)
    aprs = "Symata.apprules(mx::Mxpr{:$sjf}) = do_$sjf(mx,margs(mx)...)"
    aprs1 = "do_$sjf(mx::Mxpr{:$sjf},x...) = mx"
    evalmath(Meta.parse(aprs))
    evalmath(Meta.parse(aprs1))
    set_attribute(Symbol(sjf),:Protected)
    set_attribute(Symbol(sjf),:Listable)
end

# Handle functions that fall back on SymPy
function get_sjstr(jf, sjf, sympyf)
    set_up_sympy_default(sjf, sympyf)
    return jf, sjf
end

# Faster if we don't do interpolation
function write_sympy_apprule(sjf, sympyf, nargs::Int)
    callargs = Array{AbstractString}(undef, 0)
    sympyargs = Array{AbstractString}(undef, 0)
    for i in 1:nargs
        xi = "x" * string(i)
        push!(callargs, xi)
        push!(sympyargs, "sjtopy(" * xi * ")")
        end
    cstr = join(callargs, ", ")
    sstr = join(sympyargs, ", ")
    aprpy = "function do_$sjf(mx::Mxpr{:$sjf},$cstr)
               try
                 (sympy.$sympyf($sstr) |> pytosj)
               catch e
                 showerror(stdout, e)
                 mx
               end
            end"
    evalmath(Meta.parse(aprpy))
end


function set_up_sympy_default(sjf, sympyf)
    aprs = "Symata.apprules(mx::Mxpr{:$sjf}) = do_$sjf(mx,margs(mx)...)"
    aprs1 = "function do_$sjf(mx::Mxpr{:$sjf},x...)
               try
                 (sympy.$sympyf(map(sjtopy,x)...) |> pytosj)
               catch
                   mx
               end
           end"
    evalmath(Meta.parse(aprs))
    evalmath(Meta.parse(aprs1))
    set_attribute(Symbol(sjf),:Protected)
    set_attribute(Symbol(sjf),:Listable)
end

make_math()

#### Gamma function

@mkapprule Gamma :nargs => 1:2

@doap Gamma(a) = sjgamma(a)
@doap Gamma(a,z) = sjgamma(a,z)

set_pytosj(:gamma, :Gamma)
set_pytosj(:uppergamma, :Gamma)
set_sjtopy(:Gamma, :sympy_gamma)

@sjdoc Gamma """
    Gamma(a)

the gamma function.

    Gamma(a,z)

the upper incomplete Gamma function.
"""

### GammaRegularized

## Implementation is not complete
@mkapprule GammaRegularized :nargs => 2:3

@doap function GammaRegularized(a,z::Number)
    if z == 0
       return  _gammaregularizedzero(mx,a)
    end
   return  _gammaregularizednozero(mx,a,z)
end

function _gammaregularizedzero(mx,a::Real)
    a > 0 ? 1 : a == 0 ? 0 : ComplexInfinity
end

function _gammaregularizedzero(mx,a::Complex)
    ra = real(a)
    ra > 0 ? 1 : ra == 0 ? Indeterminate : ComplexInfinity
end

function _gammaregularizedzero(mx,a)
    mx
end

function _gammaregularizednozero(mx,a::Real,z::Number)
    a < 0 && return zero(a) ## need to consider type of z as well
    mxpr(:Gamma,a,z)/mxpr(:Gamma,a)
end

function _gammaregularizednozero(mx,a::Number,z::Number)
    mxpr(:Gamma,a,z)/mxpr(:Gamma,a)
end

function _gammaregularizednozero(mx,a,z)
    mx
end

### Erf

@mkapprule Erf :nargs => 1:2

@doap Erf(x) = sjerf(x)
@doap Erf(x,y) = sjerf(x,y)

set_pytosj(:erf, :Erf)
set_pytosj(:erf2, :Erf)
set_sjtopy(:Erf, :sympy_erf)

### InverseErf

@mkapprule InverseErf :nargs => 1:2

@doap InverseErf(x::T) where {T<:AbstractFloat} = x < 1 && x > -1 ? erfinv(x) : mx
@doap InverseErf(x) = pytosj(sympy.erfinv(sjtopy(x)))
@doap InverseErf(x,y) = pytosj(sympy.erf2inv(sjtopy(x),sjtopy(y)))

register_sjfunc_pyfunc(:InverseErf,:erfinv)
register_only_pyfunc_to_sjfunc(:InverseErf,:erf2inv)

### IntegerDigits

@sjdoc IntegerDigits """
    IntegerDigits(n,[, base][, pad])

return an array of the digits of `n` in the given `base`,
optionally padded with zeros to `pad` characters.

In contrast to Julia, more significant digits are at lower indexes.
"""
@mkapprule IntegerDigits  :nargs => 1:3
set_sysattributes(:IntegerDigits, :Listable)

_intdig(args) = MList(reverse!(args)...) |> setfixed

@doap IntegerDigits(n::Integer) = digits(n) |> _intdig
@doap IntegerDigits(n::Integer,b::Integer) = digits(n, base=convert(Int, b)) |> _intdig
@doap IntegerDigits(n::Integer,b::Integer,p::Integer) = digits(n, base=convert(Int, b), convert(Int, p)) |> _intdig

@mkapprule Log :nargs => 1:2

# sjlog defined in wrappers.jl
@doap Log(x,y) = sjlog(x,y)
@doap Log(x) = sjlog(x)

### N

@sjdoc N """
    N(expr)

try to give a the numerical value of `expr`.

    N(expr,p)

try to give `p` decimal digits of precision.

Sometimes `N` does not give the number of digits requested. In this case, you can use `SetPrecision`.
"""

## TODO: call evalf(expr,n) on sympy functions to get arbitrary precision numbers.
## Eg. N(LogIntegral(4),30) does not give correct result.
## ... a list of heads all of which should be converted and then passed to evalf.
## N needs to be rewritten
function apprules(mx::Mxpr{:N})
    outer_N(margs(mx)...)
end

function outer_N(expr)
    do_N(expr)
end

function outer_N(expr, p)
    if p > 16
        pr = precision(BigFloat)
        dig = round(Int,p*3.322)
        set_mpmath_dps(dig)     # This is for some SymPy math. But, it is not clear when this will help us
        setprecision( BigFloat, dig) # new form
        res = meval(do_N(expr,p))
        setprecision(BigFloat, pr)
        restore_mpmath_dps()
        return res
    else
        do_N(expr)
    end
end

do_N(x::Float64,dig) = x  # Mma does this
do_N(x,dig) = x
do_N(x) = x
do_N(n::T) where {T<:AbstractFloat} = n
do_N(n::T) where {T<:Real} = float(n)
do_N(n::Complex{T}) where {T<:Real} = complex(float(real(n)), float(imag(n)))
do_N(n::T,p::V) where {T<:Real,V<:Integer}  = float_with_precision(n,p)

# p is number of decimal digits of precision
# Julia doc says set_bigfloat_precision uses
# binary digits, but it looks more like decimal.

function float_with_precision(x,p)
        res = BigFloat(x)
end

# These rely on fixed-point evaluation to continue with N, this is not efficient
# We need to do it all here.

function _make_N_body(d1,d2)
    quote begin
        get_attribute(mx,:NHoldAll) && return mx;
        len = length(mx)
        args = margs(mx)
        nargs = newargs(len)
        start::Int = 1
        if get_attribute(mx,:NHoldFirst)
            nargs[1] = args[1]
            for i in 2:len
                nargs[i] = $d2
            end
        elseif get_attribute(mx,:NHoldRest)
            nargs[1] = $d1
            for i in 2:len
                nargs[i] = args[i]
            end
        else
            @inbounds for i in 1:len
                nargs[i] = $d2
            end
        end
        res = mxpr(mhead(mx),nargs)
    end
  end
end

function make_Mxpr_N()
    _Nbody = _make_N_body(:(do_N(args[1])), :(do_N(args[i])) )
    @eval begin
        function do_N(mx::Mxpr)
            $_Nbody
            res
        end
    end

    _Nbody = _make_N_body(:(do_N(args[1],p)), :(do_N(args[i],p)) )
    @eval begin
        function do_N(mx::Mxpr,p::T) where T<:Integer
            $_Nbody
            res
        end
    end
end

make_Mxpr_N()


@sjdoc GoldenRatio """
    GoldenRatio

equal to `(1+Sqrt(5))/2`.
"""


# We need to use dispatch as well, not conditionals
function do_N(s::SJSym)
    if s == :Pi || s == :π
        return float(MathConstants.pi)
    elseif s == :E
        return float(MathConstants.e)
    elseif s == :EulerGamma
        return float(MathConstants.eulergamma)
    elseif s == :GoldenRatio
        return float(MathConstants.golden)
    elseif s == :Catalan
        return float(MathConstants.catalan)
    end
    return s
end

function do_N(s::SJSym,pr::T) where T<:Integer
    if s == :Pi || s == :π
        return float_with_precision(MathConstants.pi, pr)
    elseif s == :E
        return float_with_precision(MathConstants.e, pr)
    elseif s == :EulerGamma
        return float_with_precision(MathConstants.eulergamma, pr)
    elseif s == :GoldenRatio
        return float_with_precision(MathConstants.golden, pr)
    elseif s == :Catalan
        return float_with_precision(MathConstants.catalan, pr)
    end
    return s
end

@sjdoc Precision """
    Precision(x)

get the precision of a floating point number `x` in decimal digits

    Precision()

get the current precision of `BigFloat` arithmetic

    Precision(Binary => True)
    Precision(x, Binary => True)

give precision as defined by the effective number of bits in the mantissa.
"""

@mkapprule Precision :nodefault => true, :options => Dict( :Binary => false )

#apprules(mx::Mxpr{:Precision}) = do_Precision(mx,margs(mx)...)

# TODO: mkapprule should write the correct default rule if keywords are present
do_Precision(mx::Mxpr{:Precision},args...; kws...) = mx

function do_Precision(mx::Mxpr{:Precision},x::AbstractFloat; Binary=false)
    Binary ? precision(x) : round(Int,precision(x) * log10(2))
end

function do_Precision(mx::Mxpr{:Precision}; Binary=false)
    pr = precision(BigFloat)
    Binary ? pr : round(Int,pr * log10(2))
end

@sjdoc Re """
    Re(z)

return the real part of `z`.
"""

@sjdoc Im """
    Im(z)

return the imaginary part of `z`.
"""

# Using Julia would be faster in some cases for ReIm and AbsArg
@sjdoc ReIm """
    ReIm(z)

returns `[Re(z),Im(z)]`.
"""

@mkapprule ReIm
@doap ReIm(z) = mxpr(:List,mxpr(:Re,z), mxpr(:Im,z))


# FIXME Abs(I-3) returns 10^(1/2) instead of (2^(1/2))*(5^(1/2)). i.e. does not evaluate to a fixed point
@sjdoc AbsArg """
    AbsArg(z)

return `[Abs(z),Arg(z)]`.
"""

@mkapprule AbsArg
@doap AbsArg(z) = mxpr(:List,mxpr(:Abs,z), mxpr(:Arg,z))

# Re and Im code below is disabled so it does not interfere with SymPy.
# Mma allows complex numbers of mixed Real type. So does Julia: Complex{Real}(1//3, 3.0)
# Implementation not complete. eg  Im(a + I *b) --> Im(a) + Re(b)

# do_Re{T<:Real}(mx::Mxpr{:Re}, x::Complex{T}) = real(x)
# do_Re(mx::Mxpr{:Re}, x::Real) = x

# function do_Re(mx::Mxpr{:Re}, m::Mxpr{:Times})
#     f = m[1]
#     return is_imaginary_integer(f) ? do_Re_imag_int(m,f) : mx
# end

# # dispatch on type of f. Maybe this is worth something.
# function do_Re_imag_int(m,f)
#     nargs = copy(margs(m))
#     popfirst!(nargs)
#     if length(nargs) == 1
#         return mxpr(:Times,-imag(f),mxpr(:Im,nargs))
#     else
#         return mxpr(:Times,-imag(f),mxpr(:Im,mxpr(:Times,nargs)))
#     end
# end

# do_Im{T<:Real}(mx::Mxpr{:Im}, x::Complex{T}) = imag(x)
# do_Im(mx::Mxpr{:Im}, x::Real) = zero(x)

# function do_Im(mx::Mxpr{:Im}, m::Mxpr{:Times})
#     f = m[1]
#     return is_imaginary_integer(f) ? do_Im_imag_int(m,f) : mx
# end

# function do_Im_imag_int(m,f)
#     nargs = copy(margs(m))
#     popfirst!(nargs)
#     if length(nargs) == 1
#         return mxpr(:Times,imag(f),mxpr(:Re,nargs))
#     else
#         return mxpr(:Times,imag(f),mxpr(:Re,mxpr(:Times,nargs)))
#     end
# end

#### Complex

@sjdoc Complex """
    Complex(a,b)

return a complex number when `a` and `b` are Reals

The conversion is done when the expression is parsed, so it is much faster than `a + I*b`.
"""

# Complex with two numerical arguments is converted at parse time. But, the
# arguments may evaluate to numbers only at run time, so this is needed.
# mkapprule requires that the first parameter do_Complex be annotated with the Mxpr type.

@mkapprule Complex
do_Complex(mx::Mxpr{:Complex},a::T,b::T) where {T<:Number} = complex(a,b)
do_Complex(mx::Mxpr{:Complex},a::T) where {T<:Number} = complex(a)

@sjdoc Rational """
    Rational(a,b), or a/b

return a `Rational` for `Integer`s `a` and `b`.  This is done when the
expression is parsed, so it is much faster than `a/b`.
"""

# Same here. But we need to use mdiv to reduce rationals to ints if possible.
@mkapprule Rational
do_Rational(mx::Mxpr{:Rational},a::Number,b::Number) = mdiv(a,b)

#### Rationalize

@sjdoc Rationalize """
    Rationalize(x)

return a `Rational` approximation of `x`.

    Rationalize(x,tol)

return an approximation differing from `x` by no more than `tol`.
"""

@mkapprule Rationalize
do_Rationalize(mx::Mxpr{:Rationalize},x::AbstractFloat) = rationalize(x)
do_Rationalize(mx::Mxpr{:Rationalize},x::AbstractFloat,tol::Number) = rationalize(x,tol=float(tol))
function do_Rationalize(mx::Mxpr{:Rationalize},x::Symbolic)
    r = doeval(mxpr(:N,x))  # we need to redesign do_N so that we can call it directly. See above
    return isa(r,AbstractFloat) ? do_Rationalize(mx,r) : x
end
function do_Rationalize(mx::Mxpr{:Rationalize},x::Symbolic,tol::Number)
    ndig = round(Int,-log10(tol))      # This is not quite correct.
    r = doeval(mxpr(:N,x,ndig))  # we need to redesign do_N so that we can call it directly. See above.
    return isa(r,AbstractFloat) ? do_Rationalize(mx,r,tol) : x
end
do_Rationalize(mx::Mxpr{:Rationalize},x) = x

#### Numerator

@sjdoc Numerator """
    Numerator(expr)

return the numerator of `expr`.
"""

apprules(mx::Mxpr{:Numerator}) = do_Numerator(mx::Mxpr{:Numerator},margs(mx)...)
do_Numerator(mx::Mxpr{:Numerator},args...) = mx

do_Numerator(mx::Mxpr{:Numerator},x::Rational) = numerator(x)
do_Numerator(mx::Mxpr{:Numerator},x) = x
function do_Numerator(mx::Mxpr{:Numerator},x::Mxpr{:Power})
    find_numerator(x)
end

function do_Numerator(mx::Mxpr{:Numerator},m::Mxpr{:Times})
    nargsn = newargs()
    args = margs(m)
    for i in 1:length(args)
        arg = args[i]
        res = find_numerator(arg)
        if res != 1
            push!(nargsn,res)
        end
    end
    if isempty(nargsn)
        return 1  # which one ?
    end
    if length(nargsn) == 1
        return nargsn[1]
    end
    return mxpr(mhead(m),nargsn)
end

find_numerator(x::Rational) = numerator(x)
find_numerator(x::Mxpr{:Power}) = pow_sign(x,expt(x)) > 0 ? x : 1

function pow_sign(x, texpt::Mxpr{:Times})
    fac = texpt[1]
    return is_Number(fac) && fac < 0 ? -1 : 1
end

pow_sign(x, texpt::Number) = texpt < 0 ? -1 : 1
pow_sign(x, texpt) = 1
find_numerator(x) = x

### Chop

@mkapprule Chop  :nargs => 1:2

@sjdoc Chop """
    Chop(expr)

set small floating point numbers in `expr` to zero.

    Chop(expr,eps)

set floating point numbers smaller in magnitude than `eps` in `expr` to zero.
"""
do_Chop(mx::Mxpr{:Chop}, x) = zchop(x)
do_Chop(mx::Mxpr{:Chop}, x, zeps) = zchop(x,zeps)
const chop_eps = 1e-14
zchop(x::T, eps=chop_eps) where {T<:AbstractFloat} = abs(x) > eps ? x : 0  # don't follow abstract type
zchop(x::T, eps=chop_eps) where {T<:Number} = x
zchop(x::Complex{T}, eps=chop_eps) where {T<:AbstractFloat} = complex(zchop(real(x),eps),zchop(imag(x),eps))
zchop(a::T, eps=chop_eps) where {T<:AbstractArray} = (b = similar(a); for i in 1:length(a) b[i] = zchop(a[i],eps) end ; b)
zchop!(a::T, eps=chop_eps) where {T<:AbstractArray} = (for i in 1:length(a) a[i] = zchop(a[i],eps) end ; a)
zchop(x::Expr,eps=chop_eps) = Expr(x.head,zchop(x.args)...)
zchop(x) = x
zchop(x,eps) = x

function zchop(mx::T) where T<:Mxpr
    nargs = similar(margs(mx))
    for i in 1:length(nargs) nargs[i] = zchop(mx[i]) end
    mxpr(mhead(mx), nargs)
end

function zchop(mx::T,zeps) where T<:Mxpr
    nargs = similar(margs(mx))
    for i in 1:length(nargs) nargs[i] = zchop(mx[i],zeps) end
    mxpr(mhead(mx), nargs)
end

### Conjugate

@mkapprule Conjugate :nargs => 1

@doap Conjugate(z::Number) = conj(z)
@doap Conjugate(z::Mxpr{:Plus}) = mxpr(:Plus, mapmargs( x -> mxpr(:Conjugate, x), margs(z))...)
@doap Conjugate(z::Mxpr) = z |> sjtopy |> sympy.conjugate |> pytosj

### Exp

# The parser normally takes care of this,
# But, when converting expressions from Sympy, we get Exp, so we handle it here.
# TODO: don't throw away other args. (but, we hope this is only called with returns from Sympy)
apprules(mx::Mxpr{:Exp}) = mxpr(:Power,:E,mx[1])

# TODO: handle 3rd argument
#### Mod
@mkapprule  Mod  :nargs => 2
do_Mod(mx::Mxpr{:Mod}, x::T, y::V) where {T<:Integer, V<:Integer} = mod1(x,y)

#### DivRem
@mkapprule  DivRem  :nargs => 2
do_DivRem(mx::Mxpr{:DivRem}, x::T, y::V) where {T<:Integer, V<:Integer} = mxprcf(:List,divrem(x,y)...)


#### Abs

@sjdoc Abs """
    Abs(z)

the absolute value of `z`.
"""

@mkapprule Abs :nargs => 1

@doap Abs(n::T) where {T<:Number} = mabs(n)

@doap function Abs(x)
    x |> sjtopy |> sympy.Abs |> pytosj
end

# The following code may be useful, but it is not complete
# # Abs(x^n) --> Abs(x)^n  for Real n
function do_Abs(mx::Mxpr{:Abs}, pow::Mxpr{:Power})
     doabs_pow(mx,base(pow),expt(pow))
end

# SymPy does not do this one
doabs_pow(mx,b,e::T) where {T<:Real} = mxpr(:Power,mxpr(:Abs,b),e)

function doabs_pow(mx,b,e)
    mx[1] |> sjtopy |> sympy.Abs |> pytosj
end

# do_Abs(mx::Mxpr{:Abs},prod::Mxpr{:Times}) = do_Abs(mx,prod,prod[1])

# #doabs(mx,prod,s::Symbol)

# function do_Abs{T<:Number}(mx::Mxpr{:Abs},prod,f::T)
#     f >=0 && return mx
#     if f == -1
#         return doabsmone(mx,prod,f)
#     end
#     args = copy(margs(prod))
#     args[1] = -args[1]
#     return mxpr(:Abs,mxpr(:Times,args))
# end

# function doabsmone{T<:Integer}(mx,prod,f::T)
#     args = copy(margs(prod))
#     popfirst!(args)
#     if length(args) == 1
#         return mxpr(:Abs,args)
#     else
#         return mxpr(:Abs,mxpr(:Times,args))
#     end
# end

# TODO Fix canonical routines so that 1.0 * a is not simplifed to a
function doabsmone(mx,prod,f::T) where T<:Real
    args = copy(margs(prod))
    popfirst!(args)
    if length(args) == 1
        res = mxpr(:Times,one(f),mxpr(:Abs,args))
    else
        res = mxpr(:Times,one(f),mxpr(:Abs,mxpr(:Times,args)))
    end
    return res
end

#### Sign

@mkapprule Sign :nargs => 1

do_Sign(mx::Mxpr{:Sign}, x::Number) = sign_number(x)

function sign_number(z::Complex{T}) where T<:Real
    av = mpow(real(z*conj(z)),-1//2)
    av == 1 ? z : mxprcf(:Times, z,  av)
end

sign_number(x::Real) = convert(Int,sign(x))

function do_Sign(mx::Mxpr{:Sign}, x)
    x |> sjtopy |> sympy.sign |> pytosj
end

# SymPy does not do Sign((I+1)*a), so we catch this case here.
function do_Sign(mx::Mxpr{:Sign}, x::Mxpr{:Times})
    length(x) == 0 && return 1
    x1 = x[1]
    if isa(x1,Number)
        mxpr(:Times, sign_number(x1), mxpr(:Sign , mxpr(:Times,x[2:end]...)))
    else
        x |> sjtopy |> sympy.sign |> pytosj
    end
end

# do_Sign(mx::Mxpr{:Sign}, x) = mx

# # Maybe each symbol should be a type. Like irrational or Mxpr
# function do_Sign(mx::Mxpr{:Sign}, x::SJSym)
#     x == :Pi && return 1
#     x == :E && return 1
#     x == :EulerGamma && return 1
#     return mx
# end

#### LowerGamma

@mkapprule LowerGamma

do_LowerGamma(mx::Mxpr{:LowerGamma}, a, z) =  mxpr(:Gamma,a) - mxpr(:Gamma,a,z)

@sjdoc LowerGamma """
    LowerGamma(a,z)

the lower incomplete gamma function.
"""

#### Norm

# TODO: Implement p norm
@mkapprule Norm :nargs => 1

@doap Norm(x::Number) = Abs(x)

@doap function Norm(x::Mxpr{:List})
    vectorq(x) || return mx # TODO error message
    local sum0 = 0
    for i in 1:length(x)
        sum0 += mpow(Abs(x[i]),2)
    end
    mpow(sum0,1//2)
end

@sjdoc ExpandFunc """
    ExpandFunc(expr)

rewrite some multi-parameter special functions using simpler functions.
"""

@sjdoc I """
    I

the imaginary unit.
"""

@sjdoc E """
    E

the base of the natural logarithm.
"""

@sjdoc Pi """
    Pi

the trigonometric constant `π`.
"""

@sjdoc BellB """
    BellB(n)

gives the `n^{th}` Bell number, `B_n`.

    BellB(n, x)

gives the `n^{th}` Bell polynomial, `B_n(x)`.

    BellB(n, k, [x1, x2, ... x_{n-k+1}])
"""

### Identity

@sjdoc Identity """
    Identity(expr)

return `expr`.
"""

@mkapprule Identity

@doap Identity(x) = x


### Total

@mkapprule Total

@sjdoc Total """
    Total(list)

give the sum of elements in `list`
"""

@doap Total(x::AbstractArray) = sum(x)

## We could try some things to speed this up... check that all elements are numbers ?
## with all() it takes about as much time to check as it does to sum. Maybe use try/catch
#@doap Total(x::Mxpr{:List}) = mxpr(:Plus,margs(x))
@doap Total(x::Mxpr{:List}) = _apply_plus(x)

### CosPi, SinPi

## for non-float arguments
## Not sure it is good to do this: N(CosPi(100)) will be less accurate than CosPi(N(100))
@doap CosPi(x) = mxpr(:Cos, mmul(x,Pi))
@doap SinPi(x) = mxpr(:Sin, mmul(x,Pi))

@sjdoc CosPi """
    CosPi(x)

returns `cos(π x)`. This is more accurate than `Cos(Pi*x)` for
floating point `x`.
"""

@sjdoc SinPi """
    SinPi(x)

returns `sin(π x)`. This is more accurate than `Sin(Pi*x)` for
floating point `x`.
"""

### ProductLog

@mkapprule ProductLog :nargs => 1:2

@doap function ProductLog(z::FloatRC)
    LambertW.lambertw(z)
end

@doap function ProductLog(z)
    pytosj(sympy.LambertW(sjtopy(z)))
end

@doap function ProductLog(z::FloatRC, k::Number)
    LambertW.lambertw(z,k)
end

@doap function ProductLog(z,k)
    pytosj(sympy.LambertW(sjtopy(z), sjtopy(k)))
end

### DirichletEta

## Note: sympy does not automatically transform eta to a function of zeta.
## However, Mma transforms it immediately
## However, if we do ex = DirichletEta(s) and then substitute 1 for s, we get indeterminate.
## So, one could argue for only converting DirichletEta(s) explicitly

@sjdoc DirichletEta """
    DirichletEta(s)

is the Dirichlet eta function.
"""

@mkapprule DirichletEta  :nargs => 1
@doap DirichletEta(x::FloatRC) = SpecialFunctions.eta(x)
@doap DirichletEta(x::Number) = _dirichlet_eta(x)
@doap DirichletEta(x::Integer) = x == 1 ? Log2 : _dirichlet_eta(x)
@doap DirichletEta(x) = _dirichlet_eta(x)
_dirichlet_eta(x::Number) = (1 - mpow(2, (1 - x))) * Zeta(x)
_dirichlet_eta(x) = mminus(1, mpow(2, mminus(1, x))) * Zeta(x)

### PolyLog

@sjdoc PolyLog """
    PolyLog(s,z)

is the polylogarithm function.
"""

@mkapprule PolyLog :nargs => 2

@doap PolyLog(s, z) = _polylog(mx,s,z)

_polyzero(z) = mmul(z , mpow(mminus(1,z),-1))

function _polylog(mx,s::Integer,z)
    s == 1 && return -Log(mminus(1,z))
    s == 0 && return _polyzero(z)
    if s < 0
        u = gensym()
        r = _polyzero(u)
        for i in 1:(-s)
            r = doeval(mxpr(:Factor, u*mxpr(:D,r,u)))
        end
        return replaceall(r, mxpr(:Rule, u, z))
    end
    if z == 1//2
        if s == 2
            return (Pisq - 6*(mpow(Log2,2)))/12
        elseif s == 3
            return (4*Log2^3  - 2*Pisq*Log2 + 21*Zeta(3))/24
        end
    end
    if s == 2
        z == I && return mmul(I,:Catalan) - Pisq/48
        z == MinusI && return -mmul(I,:Catalan) - Pisq/48
    end
    _polylog1(mx,s,z)
end

_polylog(mx,s,z) = _polylog1(mx,s,z)

function _polylog1(mx,s,z::Integer)
    z == 1 && return Zeta(s)
    z == -1 && return -DirichletEta(s)
    _polylog2(mx,s,z)
end

_polylog1(mx,s,z) = _polylog2(mx,s,z)

function _polylog2(mx,s1::Number,z1::Number)
    (s,z) = promote(s1,z1)
    polylognum(mx,s,z)
end

_polylog2(mx,s,z) = mx

polylognum(mx,s::FloatRC,z::FloatRC) = sympy.polylog(s,z) |> pytosj
polylognum(mx,s,z) = mx

### ExpPolar

## sympy returns this sometimes. I am not sure what it is, but it usually just hinders
## desired evaluation. We could also handle this as a direct translation in sympy.jl.
## There may be some reason to preserve this.

@mkapprule ExpPolar
@doap ExpPolar(x) = Exp(x)

### Factorial

@sjdoc Factorial """
    Factorial(n)

is the factorial function.
"""
@mkapprule Factorial :nargs => 1
@doap Factorial(n::SJSym) = mx
@doap Factorial(n) = sjfactorial(n)

# function sjfactorial(mx::Mxpr{:DirectedInfinity})
#     _sjfactorial_inf(mx,mx[1])
# end

_sjfactorial_inf(mx, x, v) = mx
_sjfactorial_inf(mx,x,v::Real) = v > 0 ? Infinity : Indeterminate
_sjfactorial_inf(mx,x,v::Complex) = real(v) == 0 ? 0 : mx

@doap function Factorial(x::Mxpr{:DirectedInfinity})
    length(x) == 0 && return mx
    _sjfactorial_inf(mx,x,x[1])
end

### Sqrt

## we must have square root for when this is constructed programatically. eg. Map(Sqrt,[4])
## Sqrt is not always read and translated!
@mkapprule Sqrt :nargs => 1
@doap Sqrt(x) = mpow(x,1//2)

### CubeRoot

@mkapprule CubeRoot :nargs => 1

@doap CubeRoot(x::AbstractFloat) = cbrt(x)
@doap function CubeRoot(x::T) where T<:Union{Integer,Rational}
    x == 0 && return 0
    x > 0 && return mpow(x,1//3)
    return -mpow(-x,1//3)
end

@doap function CubeRoot(x::Complex)
    @symwarn("CubeRoot::preal: The parameter $x should be real valued.")
    return mx
end

@doap function CubeRoot(x::Mxpr)
    return mxpr(:Surd,x,3)
end

@doap function CubeRoot(x::Symbol)
    return mxpr(:Surd,x,3)
end

### Surd

@mkapprule Surd :nargs => 2

@doap function Surd(x,n::Integer)
    if iseven(n)
        @symwarn("Surd::noneg: Surd is not defined for even roots of negative values.")
        return mx
    end
    return _surd(mx,x,n)
end

function _surd(mx,x::Real,n)
    x >= 0 && return mpow(x,1//n)
    mminus(mpow(-x,1//n))
end

function _surd(mx,x::Complex,n)
    @symwarn("Surd::preal: The parameter I should be real valued")
    mx
end

_surd(mx,x,n) = mx

function _surd(mx,x::TimesT,n)
    length(x) < 2 && return mx
    isa(x[1],Real) && return mxpr(:Surd,x[1],n) * mxpr(:Surd, mxpr(:Times, x[2:end]...),n)
    mx
end

### Power

@mkapprule Power :nargs => 2

do_Power(mx::Mxpr{:Power}, b::Symbolic, n::Integer) = n == 1 ? b : n == 0 ? one(n) : mx

@doap Power(b::SJSym, expt) = b == :E ? dopowerE(mx, expt) : mx

@doap Power(b::SJSym, expt::Integer) = expt == 1 ? b : expt == 0 ? one(expt) :
    b == :E ? dopowerE(mx, expt) : mx

dopowerE(mx, expt::T) where {T<:AbstractFloat} = exp(expt)
dopowerE(mx, expt::Complex{T}) where {T<:AbstractFloat} = exp(expt)

function dopowerE(mx, expt)
    syexpt = sjtopy(expt)
    syres = sympy.exp(syexpt)
    res = pytosj(syres)
    if is_Mxpr(res, :Exp)
        res[1] == expt && return mx
        return mxpr(:Power, :E, margs(res)...)
    end
    res
end

# Don't handle this yet.
do_Power(mx::Mxpr{:Power},   b::Complex{T},expt::Rational{V}) where {T<:Integer,V<:Integer} = mx
do_Power(mx::Mxpr{:Power},   b::Complex{T},expt::Complex{Rational{V}}) where {T<:Integer,V<:Integer} = mx

do_Power(mx::Mxpr{:Power},   b::T,expt::V) where {T<:Number,V<:Number} = mpow(b,expt)

# For some reason, we need this integer rule. For instance for (a^2)^2 --> a^4
do_Power(mx::Mxpr{:Power}, b::Mxpr{:Power}, exp::T) where {T<:Integer} = mpow(base(b), mmul(exp,expt(b)))
do_Power(   mx::Mxpr{:Power}, b::Mxpr{:Power}, exp::T) where {T<:Real} = mpow(base(b), mmul(exp,expt(b)))

do_Power(mx::Mxpr{:Power},   b::Mxpr{:Power}, exp) = is_Number(expt(b)) ? mpow(base(b), mmul(expt(b),exp)) : mx

do_Power(mx::Mxpr{:Power},   b::SJSym,expt::Complex{T}) where {T<:AbstractFloat} = b == :E ? exp(expt) : mx

do_Power(mx::Mxpr{:Power},   b::Mxpr{:DirectedInfinity},expt::T) where {T<:Integer} = mpow(b,expt)
do_Power(mx::Mxpr{:Power},   b::Mxpr{:DirectedInfinity},expt::T) where {T<:Number} = mpow(b,expt)

function do_Power(mx::Mxpr{:Power},b::T,expt::V) where {T<:Integer, V<:Rational}
    mpow(b,expt)
end

do_Power(mx,b,e) = mx


### // (Rational)

apprules(mx::Mxpr{://}) = makerat(mx,mx[1],mx[2])
makerat(mx::Mxpr{://},n::T,d::T) where {T<:Number} = n//d
makerat(mx,n,d) = mx

### BI

@sjdoc BI """
    BI(n)

convert the number `n` to type `BigInt`. Symata neither
detects integer overflow, nor automatically promote integers to `BigInt`.
But, a literal integer will automatically be given a large enough storage type without using
`BI`.
"""

@sjseealso_group(BI,BF,Big,BigIntInput)

### BF

@sjdoc BF """
    BF(n), or BF"n"

convert the number, or string `n` to a BigFloat. Symata neither
detects overflow, nor automatically promotes types from fixed to arbitrary precision. The form
`BF"n"` is more efficient, being a julia macro that converts the string "n" upon parsing.
"""

@sjdoc Big """
    Big(n)

convert `n` a maximum precision representation, typically
`BigInt` or `BigFloat`.
"""

apprules(mx::Mxpr{:BI}) = dobigint(mx,mx[1])
dobigint(mx,x) = mx
dobigint(mx,x::T) where {T<:Number} = BigInt(x)
dobigint(mx,x::T) where {T<:AbstractString} = Meta.parse(BigInt,x)

apprules(mx::Mxpr{:BF}) = dobigfloat(mx,mx[1])
dobigfloat(mx,x) = mx
dobigfloat(mx,x::T) where {T<:Number} = BigFloat(x)
dobigfloat(mx,x::T) where {T<:AbstractString} = Meta.parse(BigFloat,x)

### Big

apprules(mx::Mxpr{:Big}) = do_Big(mx,mx[1])
do_Big(mx,x) = mx
do_Big(mx,x::T) where {T<:Number} = big(x)

### Minus

apprules(mx::Mxpr{:Minus}) = is_Number(mx[1]) ? -mx[1] : mmul(-1, mx[1])


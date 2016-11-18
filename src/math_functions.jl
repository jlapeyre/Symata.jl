## First pass at math functions. There are more domain restrictions to be implemented.

#### SetPrecision

@mkapprule SetPrecision :nargs => 1

@sjdoc SetPrecision """
    SetPrecison(n)

set the precsion of BigFloat numbers to `n` decimal digits. If `N` does not give the result you
want, you can use `SetPrecision`.
"""

@sjseealso_group(N,SetPrecision,BigFloatInput,BigIntInput,BI,BF)

@doap function SetPrecision(n::Real)
    setprecision(round(Int,3.322*n))
    n
end

# This idea can be removed. It is not going anywhere.
function evalmath(x)
    Symata.eval(x)
end

## We also need to use these to convert Symata expressions to Julia

## This function writes apprules for math functions. Usally dispatches floating point
## args to Julia functions. Some have a fall through to SymPy

## These tuples have 1,2, or 3 members. Symbols are for Julia, Symata, and SymPy
## If only one member is present, the second is constructed by putting an inital capital on the first.
## If only one or two members are present, we do not fall back to SymPy.
## The last list has tuples with 2 args for which we do not use any Julia function.

## This is far from complete

# Typical symbols for Julia, Symata, SymPy
function mtr(sym::Symbol)
    s = string(sym)
    sjf = ucfirst(s)
    (sym, Symbol(sjf), sym)
end

# We choose not to implement :cis. Julia has it. sympy and mma, not

# Maybe do these
# :Abs,:Chi, :exp, :ln, :log, :sqrt
# ?? :laguerre

# real_root vs real_roots ?

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
# BellB should not be "listable", maybe bell split into BellB and BellY

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
         (:sinc,),(:cosc,),
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


const single_arg_float_int_complex =
        [
#         (:conj,:Conjugate,:conjugate)
         ]

    const single_arg_float = [(:cbrt,:CubeRoot,:cbrt),
                        (:erfcinv,:InverseErfc,:erfcinv),(:invdigamma,:InverseDigamma)
                        ]

# FIXME  Ceiling should probably return integer type. So it can't be included in generic code here (or punt to sympy for everything)
    const single_arg_float_int = [(:signbit,:SignBit), (:ceil, :Ceiling, :ceiling), (:floor, :Floor, :floor)]

#  (:factorial,:Factorial, :factorial), <--- we do this by hand

    const single_arg_int = [(:isqrt,:ISqrt),(:ispow2,:IsPow2),(:nextpow2,:NextPow2),(:prevpow2,:PrevPow2),
                      (:isprime,:PrimeQ)
                        ]


    const two_arg_int = [(:binomial,:Binomial,:binomial)
                         ]

# Do NDigits by hand for now!
@mkapprule NDigits :nargs => 1:2
@doap NDigits{T<:Integer,V<:Integer}(n::T,b::V) = ndigits(n,b)
@doap NDigits{T<:Integer}(n::T) = ndigits(n)
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

# There is LambertW Julia code, but we do not use it.
# TODO: Migrate functions out this non-specific list
    const no_julia_function = [  (:ExpIntegralE, :expint),
                           (:GegenbauerC, :gegenbauer), (:HermiteH, :hermite),
                           (:BellB, :bell), (:BernoulliB, :bernoulli), (:CatalanNumber, :catalan),
                           (:EulerE, :euler), (:Subfactorial, :subfactorial), (:Factorial2, :factorial2),
                           (:FactorialPower, :FallingFactorial), (:Pochhammer, :RisingFactorial),
(:Fibonacci, :fibonacci), (:LucasL, :lucas), (:LeviCivita, :LeviCivita),
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


    for x in [(:ProductLog, :LambertW)]  # second arg restricted
        sjf = x[1]
        eval(macroexpand( :( @mkapprule $sjf)))
        write_sympy_apprule(x[1],x[2],1)
        write_sympy_apprule(x[1],x[2],2)
    end

    for x in no_julia_function_one_arg
        sjf = x[1]
        eval(macroexpand( :( @mkapprule $sjf)))
        write_sympy_apprule(x[1],x[2],1)
    end


    for x in no_julia_function_two_args
        sjf = x[1]
        eval(macroexpand( :( @mkapprule $sjf :nargs => 2)))
        write_sympy_apprule(x[1],x[2],2)
    end

    for x in no_julia_function_four_args
        nargs = 4
        sjf = x[1]
        eval(macroexpand( :( @mkapprule $sjf :nargs => $nargs)))
        write_sympy_apprule(x[1],x[2],nargs)
    end

    for x in no_julia_function_three_args
        nargs = 3
        sjf = x[1]
        eval(macroexpand( :( @mkapprule $sjf :nargs => $nargs)))
        write_sympy_apprule(x[1],x[2],nargs)
    end

    for x in no_julia_function_two_or_three_args
        sjf = x[1]
        eval(macroexpand( :( @mkapprule $sjf)))
        write_sympy_apprule(x[1],x[2],2)
        write_sympy_apprule(x[1],x[2],3)
    end


    for x in no_julia_function_one_or_two_int
        sjf = x[1]
        eval(macroexpand( :( @mkapprule $sjf)))
        write_sympy_apprule(x[1],x[2],1)
        write_sympy_apprule(x[1],x[2],2)
    end

    # TODO: update this code
    for x in no_julia_function
        set_up_sympy_default(x...)
        set_pattributes(string(x[1]))     # TODO: allow set_pattributes to take a symbol arg
    end

    # Ok, this works. We need to clean it up
    for x in single_arg_float_complex
        jf,sjf = only_get_sjstr(x...)
        eval(macroexpand( :( @mkapprule $sjf :nargs => 1 )))
        write_julia_numeric_rule(jf,sjf,"AbstractFloat")
        write_julia_numeric_rule(jf,sjf,"CAbstractFloat")
        if length(x) == 3
            write_sympy_apprule(sjf,x[3],1)
        end
        set_attribute(Symbol(sjf),:Listable)
    end

    for x in single_arg_float
        jf,sjf = only_get_sjstr(x...)
        eval(macroexpand( :( @mkapprule $sjf :nargs => 1 )))
        write_julia_numeric_rule(jf,sjf,"AbstractFloat")
        if length(x) == 3
            write_sympy_apprule(sjf,x[3],1)
        end
        set_attribute(Symbol(sjf),:Listable)
    end

    for x in single_arg_float_int
        jf,sjf = only_get_sjstr(x...)
        eval(macroexpand( :( @mkapprule $sjf :nargs => 1 )))
        write_julia_numeric_rule(jf,sjf,"Real")
        if length(x) == 3
            write_sympy_apprule(sjf,x[3],1)
        end
    end

    # This is all numbers, I suppose
    for x in single_arg_float_int_complex
        jf,sjf = only_get_sjstr(x...)
        eval(macroexpand( :( @mkapprule $sjf :nargs => 1 )))
        write_julia_numeric_rule(jf,sjf,"Real")
        write_julia_numeric_rule(jf,sjf,"CReal")
        if length(x) == 3
            write_sympy_apprule(sjf,x[3],1)
        end
    end

    for x in single_arg_int
        jf,sjf = only_get_sjstr(x...)
        eval(macroexpand( :( @mkapprule $sjf :nargs => 1 :argtypes => [Integer] )))
        write_julia_numeric_rule(jf,sjf,"Integer")
        if length(x) == 3
            write_sympy_apprule(sjf,x[3],1)
        end
    end

    for x in two_arg_int
        jf,sjf = only_get_sjstr(x...)
        eval(macroexpand( :( @mkapprule $sjf :nargs => 2 )))
        write_julia_numeric_rule(jf,sjf,"Integer", "Integer")
        if length(x) == 3
            write_sympy_apprule(sjf,x[3],2)
        end
    end

    # Mma allows one arg, as well
    for x in one_or_two_args1
        jf,sjf = only_get_sjstr(x...)
        eval(macroexpand(:( @mkapprule $sjf )))
        write_julia_numeric_rule(jf,sjf,"Integer", "AbstractFloat")
        write_julia_numeric_rule(jf,sjf,"Integer", "CAbstractFloat")
        if length(x) == 3
            write_sympy_apprule(sjf,x[3],2)
        end
    end

    for x in two_arg_float
        jf,sjf = only_get_sjstr(x...)
        eval(macroexpand( :( @mkapprule $sjf :nargs => 2 )))
        write_julia_numeric_rule(jf,sjf,"AbstractFloat", "AbstractFloat")
    end

    for x in two_arg_float_and_float_or_complex
        jf,sjf = only_get_sjstr(x...)
        eval(macroexpand( :( @mkapprule $sjf :nargs => 2 )))
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
    appstr = "do_$sjf{$annot}(mx::Mxpr{:$sjf},$protargs) = $jf($callargs)"
    eval(parse(appstr))
end

function only_get_sjstr(jf,sjf,args...)
    return jf, sjf
end

function only_get_sjstr(jf)
    st = string(jf)
    sjf = ucfirst(st)
    return jf, sjf
end

function get_sjstr(jf,sjf)
    do_common(sjf)
    return jf, sjf
end

function get_sjstr(jf)
    st = string(jf)
    sjf = ucfirst(st)
    do_common(sjf)
    return jf, sjf
end

# Handle functions that fall back on SymPy
function get_sjstr(jf, sjf, sympyf)
    set_up_sympy_default(sjf, sympyf)
    return jf, sjf
end

# Faster if we don't do interpolation
function write_sympy_apprule(sjf, sympyf, nargs::Int)
    callargs = Array(AbstractString,0)
    sympyargs = Array(AbstractString,0)
    for i in 1:nargs
        xi = "x" * string(i)
        push!(callargs, xi)
        push!(sympyargs, "sjtopy(" * xi * ")")
        end
    cstr = join(callargs, ", ")
    sstr = join(sympyargs, ", ")
    aprpy = "function do_$sjf(mx::Mxpr{:$sjf},$cstr)
               try
                 (sympy[:$sympyf]($sstr) |> pytosj)
               catch e
                 showerror(e)
                 mx
               end
            end"
    evalmath(parse(aprpy))
end


function set_up_sympy_default(sjf, sympyf)
    aprs = "Symata.apprules(mx::Mxpr{:$sjf}) = do_$sjf(mx,margs(mx)...)"
    aprs1 = "function do_$sjf(mx::Mxpr{:$sjf},x...)
               try
                 (sympy[:$sympyf](map(sjtopy,x)...) |> pytosj)
               catch
                   mx
               end
           end"
    evalmath(parse(aprs))
    evalmath(parse(aprs1))
    set_attribute(Symbol(sjf),:Protected)
    set_attribute(Symbol(sjf),:Listable)
end

# Handle functions that do *not* fall back on SymPy
function do_common(sjf)
    aprs = "Symata.apprules(mx::Mxpr{:$sjf}) = do_$sjf(mx,margs(mx)...)"
    aprs1 = "do_$sjf(mx::Mxpr{:$sjf},x...) = mx"
    evalmath(parse(aprs))
    evalmath(parse(aprs1))
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

#### InverseErf

@mkapprule InverseErf :nargs => 1:2

@doap InverseErf{T<:AbstractFloat}(x::T) = x < 1 && x > -1 ? erfinv(x) : mx
@doap InverseErf(x) = pytosj(sympy[:erfinv](sjtopy(x)))
@doap InverseErf(x,y) = pytosj(sympy[:erf2inv](sjtopy(x),sjtopy(y)))

register_sjfunc_pyfunc(:InverseErf,:erfinv)
register_only_pyfunc_to_sjfunc(:InverseErf,:erf2inv)

#### IntegerDigits

@sjdoc IntegerDigits """
    IntegerDigits(n,[, base][, pad])

return an array of the digits of `n` in the given `base`,
optionally padded with zeros to `pad` characters.

In contrast to Julia, more significant digits are at lower indexes.
"""

do_common("IntegerDigits")

do_IntegerDigits(mx::Mxpr{:IntegerDigits},n::Integer) = setfixed(mxpr(:List,reverse!(digits(n))...))
do_IntegerDigits(mx::Mxpr{:IntegerDigits},n::Integer,b::Integer) = setfixed(mxpr(:List,reverse!(digits(n,convert(Int,b)))...))
do_IntegerDigits(mx::Mxpr{:IntegerDigits},n::Integer,b::Integer,p::Integer) = setfixed(mxpr(:List,reverse!(digits(n,convert(Int,b),convert(Int,p)))...))

@sjdoc Primes """
    Primes(n)

return a collection of the prime numbers `<= n`.
"""


apprules(mx::Mxpr{:Primes}) = do_Primes(mx,margs(mx)...)
do_Primes(mx,args...) = mx
do_Primes(mx,n::Integer) = setfixed(mxpr(:List,primes(n)...))

@mkapprule Log :nargs => 1:2

# sjlog defined in wrappers.jl
@doap Log(x,y) = sjlog(x,y)
@doap Log(x) = sjlog(x)

#do_common("Log")
# do_Log(mx::Mxpr{:Log},x::AbstractFloat) = x > 0 ? log(x) : log(complex(x))
# do_Log{T<:AbstractFloat}(mx::Mxpr{:Log},x::Complex{T}) = log(x)
# do_Log{T<:AbstractFloat}(mx::Mxpr{:Log},b::Real,z::Complex{T}) = log(b,z)
# do_Log{T<:AbstractFloat}(mx::Mxpr{:Log},b::Real,z::T) = z > 0 ? log(b,z) : log(b,complex(z))
# @doap Log(x::Integer) = x == 0 ? MinusInfinity : mx
# do_Log(mx::Mxpr{:Log},pow::Mxpr{:Power}) = do_Log(mx,pow,base(pow),expt(pow))
# do_Log(mx::Mxpr{:Log},pow::Mxpr{:Power},b::SJSym,e::Integer) = b == :E ? e : mx
# do_Log(mx::Mxpr{:Log},b::SJSym) = b == :E ? 1 : mx

@sjdoc N """
    N(expr)

try to give a the numerical value of `expr`.

    N(expr,p)

try to give `p` decimal digits of precision.

Sometimes `N` does not give the number of digits requested. In this case, you can use `SetPrecision`.
"""

# N needs to be rewritten
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
do_N{T<:AbstractFloat}(n::T) = n
do_N{T<:Real}(n::T) = float(n)
do_N{T<:Real}(n::Complex{T}) = complex(float(real(n)), float(imag(n)))
do_N{T<:Real,V<:Integer}(n::T,p::V)  = float_with_precision(n,p)

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
        function do_N{T<:Integer}(mx::Mxpr,p::T)
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

set_attribute(:GoldenRatio, :Protected)

# We need to use dispatch as well, not conditionals
function do_N(s::SJSym)
    if s == :Pi || s == :π
        return float(pi)
    elseif s == :E
        return float(e)
    elseif s == :EulerGamma
        return float(eulergamma)
    elseif s == :GoldenRatio
        return float(golden)
    elseif s == :Catalan
        return float(catalan)
    end
    return s
end

function do_N{T<:Integer}(s::SJSym,pr::T)
    if s == :Pi || s == :π
        return float_with_precision(pi,pr)
    elseif s == :E
        return float_with_precision(e,pr)
    elseif s == :EulerGamma
        return float_with_precision(eulergamma,pr)
    elseif s == :GoldenRatio
        return float_with_precision(golden,pr)
    elseif s == :Catalan
        return float_with_precision(catalan,pr)
    end
    return s
end

@sjdoc Precision """
    Precision(x)

get the precision of a floating point number `x`, as defined by the
effective number of bits in the mantissa.
"""


apprules(mx::Mxpr{:Precision}) = do_Precision(mx,margs(mx)...)
do_Precision(mx::Mxpr{:Precision},args...) = mx
do_Precision(mx::Mxpr{:Precision},x::AbstractFloat) = precision(x)

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
# Mma allows complex numbers of mixed Real type. Julia does not.
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
#     shift!(nargs)
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
#     shift!(nargs)
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
do_Complex{T<:Number}(mx::Mxpr{:Complex},a::T,b::T) = complex(a,b)
do_Complex{T<:Number}(mx::Mxpr{:Complex},a::T) = complex(a)

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
zchop{T<:AbstractFloat}(x::T, eps=chop_eps) = abs(x) > eps ? x : 0  # don't follow abstract type
zchop{T<:Number}(x::T, eps=chop_eps) = x
zchop{T<:AbstractFloat}(x::Complex{T}, eps=chop_eps) = complex(zchop(real(x),eps),zchop(imag(x),eps))
zchop{T<:AbstractArray}(a::T, eps=chop_eps) = (b = similar(a); for i in 1:length(a) b[i] = zchop(a[i],eps) end ; b)
zchop!{T<:AbstractArray}(a::T, eps=chop_eps) = (for i in 1:length(a) a[i] = zchop(a[i],eps) end ; a)
zchop(x::Expr,eps=chop_eps) = Expr(x.head,zchop(x.args)...)
zchop(x) = x
zchop(x,eps) = x

function zchop{T<:Mxpr}(mx::T)
    nargs = similar(margs(mx))
    for i in 1:length(nargs) nargs[i] = zchop(mx[i]) end
    mxpr(mhead(mx), nargs)
end

function zchop{T<:Mxpr}(mx::T,zeps)
    nargs = similar(margs(mx))
    for i in 1:length(nargs) nargs[i] = zchop(mx[i],zeps) end
    mxpr(mhead(mx), nargs)
end

### Conjugate

@mkapprule Conjugate :nargs => 1

@doap Conjugate(z::Number) = conj(z)
@doap Conjugate(z::Mxpr{:Plus}) = mxpr(:Plus, map( x -> mxpr(:Conjugate, x), margs(z))...)
@doap Conjugate(z::Mxpr) = z |> sjtopy |> sympy[:conjugate] |> pytosj

### Exp

# The parser normally takes care of this,
# But, when converting expressions from Sympy, we get Exp, so we handle it here.
# TODO: don't throw away other args. (but, we hope this is only called with returns from Sympy)
apprules(mx::Mxpr{:Exp}) = mxpr(:Power,:E,mx[1])

# TODO: handle 3rd argument
#### Mod
@mkapprule  Mod  :nargs => 2
do_Mod{T<:Integer, V<:Integer}(mx::Mxpr{:Mod}, x::T, y::V) = mod1(x,y)

#### DivRem
@mkapprule  DivRem  :nargs => 2
do_DivRem{T<:Integer, V<:Integer}(mx::Mxpr{:DivRem}, x::T, y::V) = mxprcf(:List,divrem(x,y)...)


#### Abs

@sjdoc Abs """
    Abs(z)

the absolute value of `z`.
"""

@mkapprule Abs :nargs => 1

@doap Abs{T<:Number}(n::T) = mabs(n)

@doap function Abs(x)
    x |> sjtopy |> sympy[:Abs] |> pytosj
end

# The following code may be useful, but it is not complete
# # Abs(x^n) --> Abs(x)^n  for Real n
function do_Abs(mx::Mxpr{:Abs}, pow::Mxpr{:Power})
     doabs_pow(mx,base(pow),expt(pow))
end

# SymPy does not do this one
doabs_pow{T<:Real}(mx,b,e::T) = mxpr(:Power,mxpr(:Abs,b),e)

function doabs_pow(mx,b,e)
    mx[1] |> sjtopy |> sympy[:Abs] |> pytosj
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
#     shift!(args)
#     if length(args) == 1
#         return mxpr(:Abs,args)
#     else
#         return mxpr(:Abs,mxpr(:Times,args))
#     end
# end

# TODO Fix canonical routines so that 1.0 * a is not simplifed to a
function doabsmone{T<:Real}(mx,prod,f::T)
    args = copy(margs(prod))
    shift!(args)
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

function sign_number{T<:Real}(z::Complex{T})
    av = mpow(real(z*conj(z)),-1//2)
    av == 1 ? z : mxprcf(:Times, z,  av)
end

sign_number(x::Real) = convert(Int,sign(x))

function do_Sign(mx::Mxpr{:Sign}, x)
    x |> sjtopy |> sympy[:sign] |> pytosj
end

# SymPy does not do Sign((I+1)*a), so we catch this case here.
function do_Sign(mx::Mxpr{:Sign}, x::Mxpr{:Times})
    length(x) == 0 && return 1
    x1 = x[1]
    if isa(x1,Number)
        mxpr(:Times, sign_number(x1), mxpr(:Sign , mxpr(:Times,x[2:end]...)))
    else
        x |> sjtopy |> sympy[:sign] |> pytosj
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
# TODO: improve efficiency
@mkapprule Norm :nargs => 1

@doap Norm(x::Complex) = mxpr(:Abs,x)

@doap function Norm(x::Mxpr{:List})
    vectorq(x) || return mx # TODO error message
    local sum0 = 0
    for i in 1:length(x)
        sum0 += mxpr(:Power, mxpr(:Abs, x[i]),2)
    end
    mxpr(:Sqrt,sum0)
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

# We could try some things to speed this up... check that all elements are numbers ?
@doap Total(x::Mxpr{:List}) = mxpr(:Plus,margs(x))

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

### MittagLefflerE

@sjdoc MittagLefflerE """
    MittagLeffler(α,β,z)

    MittagLeffler(α,z)

represents the Mittag-Leffler function.
"""

## FIXME: organize this more like polylog is organized
## in general, we need a way to combine effective pattern matching with efficient dispatch.
## i.e. we want fewer `isa(...)`s.
## Maybe one of the Julia matching packages will do this for us.
## PatternDispatch.jl ?
## Match.jl appears to not write functions taking advantage of multiple dispatch
## Stefan mentioned something about ameliorating ambiguity hell in v0.6.
##
## for alpha == 0 , must have abs(z) < 1
## Mma does not return conditions, I think.
## We could optionally return conditions.

@mkapprule MittagLefflerE :nargs => 2:3

@doap MittagLefflerE(α,z) = mittagleffler(mx,α,z)

@doap function MittagLefflerE(α,β,z)
    β == 1  && return mittagleffler(mx,α,z)
    mittagleffler(mx,α,β,z)
end

function mittagleffler(mx,α,z)
    α == 1//2 && return Exp(mpow(z,2)) * Erfc(mminus(z))
    α == 0 && return mpow(mminus(1,z),-1)
    α == 1 && return Exp(z)
    α == 2 && return Cosh(Sqrt(z))
    α == 3 && return (1//3)* (Exp(mpow(z,(1//3))) + mmul(2,Exp(-mpow(z,1//3)/2)) * Cos(Sqrt(3)/2 * mpow(z,1//3)))
    # Following stopped working when we do not import *,+, etc. from main 2*Mxpr fails ??
    # But, we have defined a * method for these types!
    # MethodError(*,(2,:E^((-:z^(1//3))*2^(-1))))
    # ERROR: MethodError: no method matching *(::Int64, ::Symata.Mxpr{:Power})
    # Closest candidates are:
    #   *(::Any, ::Any, ::Any, ::Any...) at operators.jl:138
    #    α == 3 && return (1//3)* (Exp(mpow(z,(1//3))) + 2*Exp(-mpow(z,1//3)/2) * Cos(Sqrt(3)/2 * mpow(z,1//3)))    
    α == 4 && return (1//2)* ( Cosh(mpow(z,1//4)) + Cos(mpow(z,1//4)))
    if isa(α,AbstractFloat) || isa(z, FloatRC)
        return MittLeff.mittleff(float(α),float(z))
    end
    z == 0 && return mpow(Gamma(β),-1)
    mx
end

function mittagleffler(mx,α,β,z)
    if isa(α,AbstractFloat) || isa(β,AbstractFloat) || isa(z, FloatRC)
       return MittLeff.mittleff(float(α),float(β),float(z))
    end
    z == 0 && return mpow(Gamma(β),-1)
    if α == 1
        β == 2 && return (mmul(mplus(Exp(z),-1),mpow(z,-1)))
        return (mpow(:E,z)*mpow(z,mminus(1,β)) * mxpr(:GammaRegularized, mminus(β,1),0,z))
    end
    α == 2 && β == 2 && return mmul(Sinh(mpow(z,1//2)), mpow(mpow(z,1//2),-1))
    mx
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
@doap DirichletEta(x::FloatRC) = eta(x)
@doap DirichletEta(x::Number) = _dirichlet_eta(x)
@doap DirichletEta(x::Integer) = x == 1 ? Log2 : _dirichlet_eta(x)
@doap DirichletEta(x) = _dirichlet_eta(x)
_dirichlet_eta(x::Number) = (1-mpow(2,(1-x)))*Zeta(x)
_dirichlet_eta(x) = mminus(1, mpow(2, mminus(1,x)))*Zeta(x)


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

polylognum(mx,s::FloatRC,z::FloatRC) = sympy[:polylog](s,z) |> pytosj
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

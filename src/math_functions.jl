## First pass at math functions. There are more domain restrictions to be implemented.

# Taken from compat.jl
# Pull Request https://github.com/JuliaLang/julia/pull/13232
# Rounding and precision functions:
if VERSION >= v"0.5.0-dev+1182"
    import Base:
        setprecision, setrounding, rounding

else  # if VERSION < v"0.5.0-dev+1182"

    export setprecision
    export setrounding
    export rounding

    setprecision(f, ::Type{BigFloat}, prec) = with_bigfloat_precision(f, prec)
    setprecision(::Type{BigFloat}, prec) = set_bigfloat_precision(prec)

    # assume BigFloat if type not explicit:
    setprecision(prec) = setprecision(BigFloat, prec)
    setprecision(f, prec) = setprecision(f, BigFloat, prec)

    Base.precision(::Type{BigFloat}) = get_bigfloat_precision()

    setrounding(f, T, rounding_mode) =
        with_rounding(f, T, rounding_mode)

    setrounding(T, rounding_mode) = set_rounding(T, rounding_mode)

    rounding(T) = get_rounding(T)

end

# We want to put things into modules eventually
function evalmath(x)
    SJulia.eval(x)
end

## We also need to use these to convert SJulia expressions to Julia

## This function writes apprules for math functions. Usally dispatches floating point
## args to Julia functions. Some have a fall through to SymPy

## These tuples have 1,2, or 3 members. Symbols are for Julia, SJulia, and SymPy
## If only one member is present, the second is constructed by putting an inital capital on the first.
## If only one or two members are present, we do not fall back to SymPy.
## The last list has tuples with 2 args for which we do not use any Julia function.

## This is far from complete

# Typical symbols for Julia, SJulia, SymPy
function mtr(sym::Symbol)
    s = string(sym)
    sjf = capitalize_first_character(s)
    (sym, symbol(sjf), sym)
end

# We choose not to implement :cis. Julia has it. sympy and mma, not

# Maybe do these
# :Abs,:Chi, :exp, :ln, :log, :lowergamma, :sqrt
# ?? :laguerre

# real_root vs real_roots ?

# List of not yet implemented sympy functions from keys(sympy.functions)
# :E1,:Ei,:FallingFactorial,:Heaviside, :Id,
# :Max,:Min,:Piecewise,
# :Shi, :Ynm,:Ynm_c,:Znm,
# :__builtins__,:__doc__,:__file__,:__name__,:__package__,:__path__,
# :acos,:acosh,:acot,:acoth,:acsc,:adjoint,
#  :assoc_laguerre,:assoc_legendre,
#  :bspline_basis, :bspline_basis_set, :ceiling,:chebyshevt_root,
# :chebyshevu_root,:combinatorial,:conjugate,
#  :elementary,
# ::erf2,:erf2inv,
#  :exp_polar, :factorial2,:ff
# :floor, :hyper,:im, :jacobi_normalized, :jn, :jn_zeros, 
# :periodic_argument,:piecewise_fold,:polar_lift,
# :principal_branch,:re,:real_root,:rf,:root, :special,
# :transpose,:unbranched_argument,:yn

# TODO: FIX: sympy zeta may take two args
# two arg both float or complex : zeta(s,z)  (with domain restrictions)

# TODO: rename these arrays. Name by number of args plus identifying number.
# ie. two_args1, two_args2. Then explain in comment the particulars. A naming
# scheme to capture all possibilities is to cumbersome.

   const single_arg_float_complex =   # check, some of these can't take complex args
    [ mtr(:sin), mtr(:tan), (:sind,:SinD), (:cosd,:CosD),(:tand,:TanD),
         (:sinpi,:SinPi), (:cospi,:CosPi), mtr(:sinh), mtr(:cosh),
         mtr(:tanh), (:acos,:ArcCos,:acos), (:asin,:ArcSin,:asin),
         (:atan,:ArcTan,:atan),(:atan2,:ArcTan2,:atan2),(:acosd,:ArcCosD), (:asind,:ArcSinD),
         (:atand,:ArcTanD), mtr(:sec), mtr(:csc), mtr(:cot),(:secd,:SecD),(:csc,:CscD),(:cot,:CotD),
         (:asec,:ArcSec),(:acsc,:ArcCsc),(:acot,:ArcCot),  # (:acotd,:ArcCotD)
         (:csch,), mtr(:coth,),(:asinh,:ASinh,:asinh), (:acosh,:ACosh, :acosh),(:atanh,:ATanh, :atanh),
         (:asech,:ArcSech),(:acsch,:ArcCsch),(:acoth,:ArcCoth, :acoth),
         (:sinc,),(:cosc,),
         (:log1p,),(:exp2,),(:exp10,),(:expm1,),(:abs2,),
         mtr(:erf), mtr(:erfc), mtr(:erfi),(:erfcx,),(:dawson,),(:real,:Re, :re),(:imag,:Im, :im),
         (:angle,:Arg,:arg), (:gamma, :Gamma, :gamma), (:lgamma, :LogGamma, :loggamma),
    (:lfact,:LogFactorial), mtr(:digamma), mtr(:trigamma),
    (:airyai,:AiryAi,:airyai),
         (:airybi,:AiryBi,:airybi),(:airyaiprime,:AiryAiPrime,:airyaiprime),(:airybiprime,:AiryBiPrime,:airybiprime),
         (:besselj0,:BesselJ0),(:besselj1,:BesselJ1),(:bessely0,:BesselY0),(:bessely1,:BesselY1),
         (:eta,:DirichletEta,:dirichlet_eta), (:zeta,:Zeta,:zeta)
         ]

# (:log,),   removed from list above, because it must be treated specially (and others probably too!)

    const single_arg_float_int_complex =
        [
         (:conj,:Conjugate)
         ]

    const single_arg_float = [(:cbrt,:CubeRoot,:cbrt),(:erfinv,:InverseErf,:erfinv),
                        (:erfcinv,:InverseErfc,:erfcinv),(:invdigamma,:InverseDigamma)
                        ]

    const single_arg_float_int = [(:factorial,:Factorial, :factorial),(:signbit,:SignBit)]

    const single_arg_int = [(:isqrt,:ISqrt),(:ispow2,:IsPow2),(:nextpow2,:NextPow2),(:prevpow2,:PrevPow2),
                      (:isprime,:PrimeQ)
                        ]

    const two_arg_int = [(:binomial,:Binomial,:binomial), (:ndigits,:NDigits)
                         ]

# Complicated!
    const one_or_two_args1 = [(:polygamma, :PolyGamma, :polygamma)]

    const two_arg_float_and_float_or_complex =
     [
      (:besselj,:BesselJ, :besselj), (:besseljx,:BesselJx), (:bessely,:BesselY,:bessely),
      (:besselyx,:BesselYx), (:hankelh1,:HankelH1,:hankel1), (:hankelh1x,:HankelH1x),
      (:hankelh2,:HankelH2,:hankel2), (:hankelh2x,:HankelH2x), (:besseli,:BesselI, :besseli),
      (:besselix,:BesselIx), (:besselk,:BesselK,:besselk), (:besselkx,:BesselKx),
      ]

    const  two_arg_float = [ (:beta,),(:lbeta,:LogBeta),(:hypot,)]

    ## There are no Julia functions for these (or at least we are not using them).
    ## First symbol is for  SJulia, Second is SymPy

# There is LambertW Julia code, but we do not use it.
# TODO: Migrate functions out this non-specific list
    const no_julia_function = [  (:ExpIntegralE, :expint),
                           (:GegenbauerC, :gegenbauer), (:HermiteH, :hermite),
                           (:BellB, :bell), (:BernoulliB, :bernoulli), (:CatalanNumber, :catalan),
                           (:EulerE, :euler), (:Subfactorial, :subfactorial), (:Factorial2, :factorial2),
                           (:FactorialPower, :FallingFactorial), (:Pochhammer, :RisingFactorial),
(:Fibonacci, :fibonacci), (:LucasL, :lucas), (:LeviCivita, :LeviCivita),
(:KroneckerDelta, :KroneckerDelta),  (:HypergeometricPFQ, :hyper)
]
# LeviCivita is different than in mathematica

const no_julia_function_one_arg = [ (:EllipticK, :elliptic_k), (:HeavisideTheta,:Heaviside), (:LogIntegral, :li),
                                    (:CosIntegral, :Ci), (:SinIntegral, :Si),
                                    (:FresnelC, :fresnelc), (:FresnelS, :fresnels), (:DiracDelta, :DiracDelta),
                                    (:LogIntegral, :Li), (:Ei, :Ei), (:ExpandFunc, :expand_func)]

# elliptic_e take more than ints!
const no_julia_function_one_or_two_int = [ (:HarmonicNumber, :harmonic) , (:EllipticE, :elliptic_e) ]

const no_julia_function_two_args = [(:LegendreP, :legendre_poly), (:EllipticF, :elliptic_f),
                                    (:ChebyshevT, :chebyshevt), (:ChebyshevU, :chebyshevu),
                                    (:Cyclotomic, :cyclotomic_poly), (:SwinnertonDyer, :swinnerton_dyer_poly),
                                    (:PolyLog, :polylog)
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
    
    
    for x in no_julia_function
        set_up_sympy_default(x...)
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
        set_attribute(symbol(sjf),:Listable)
    end

    for x in single_arg_float
        jf,sjf = only_get_sjstr(x...)
        eval(macroexpand( :( @mkapprule $sjf :nargs => 1 )))
        write_julia_numeric_rule(jf,sjf,"AbstractFloat")
        if length(x) == 3
            write_sympy_apprule(sjf,x[3],1)
        end
        set_attribute(symbol(sjf),:Listable)
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
    sjf = capitalize_first_character(st)
    return jf, sjf
end

function get_sjstr(jf,sjf)
    do_common(sjf)
    return jf, sjf
end

function get_sjstr(jf)
    st = string(jf)
    sjf = capitalize_first_character(st)
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
        push!(sympyargs, "mxpr2sympy(" * xi * ")")
        end
    cstr = join(callargs, ", ")
    sstr = join(sympyargs, ", ")
    aprpy = "function do_$sjf(mx::Mxpr{:$sjf},$cstr)
        try
            (sympy.$sympyf($sstr) |> sympy2mxpr)
        catch
            mx
        end
    end"
    evalmath(parse(aprpy))
end


function set_up_sympy_default(sjf, sympyf)
    aprs = "SJulia.apprules(mx::Mxpr{:$sjf}) = do_$sjf(mx,margs(mx)...)"
    aprs1 = "function do_$sjf(mx::Mxpr{:$sjf},x...)
               try
                 (sympy.$sympyf(map(mxpr2sympy,x)...) |> sympy2mxpr)
               catch
                   mx
               end
           end"
    evalmath(parse(aprs))
    evalmath(parse(aprs1))
    set_attribute(symbol(sjf),:Protected)
    set_attribute(symbol(sjf),:Listable)
end

# Handle functions that do *not* fall back on SymPy
function do_common(sjf)
    aprs = "SJulia.apprules(mx::Mxpr{:$sjf}) = do_$sjf(mx,margs(mx)...)"
    aprs1 = "do_$sjf(mx::Mxpr{:$sjf},x...) = mx"
    evalmath(parse(aprs))
    evalmath(parse(aprs1))
    set_attribute(symbol(sjf),:Protected)
    set_attribute(symbol(sjf),:Listable)
end

make_math()

do_Abs2(mx::Mxpr{:Abs2},x::Integer) = x*x
do_Abs2{T<:Integer}(mx::Mxpr{:Abs2},z::Complex{T}) = ((x,y) = reim(z); x*x + y*y)

# use sympy
#do_ArcTan(mx::Mxpr{:ArcTan},x::Integer) = x == 1 ? 4 * :Pi : x == 0 ? 0 : mx
#do_ArcTan(mx::Mxpr{:ArcTan},x::SJSym) = x == :Infinity ? 1//2 * :Pi : mx

do_Gamma(mx::Mxpr{:Gamma},x) = sympy2mxpr(sympy.gamma(mxpr2sympy(x)))
do_Gamma(mx::Mxpr{:Gamma},x,y) = sympy2mxpr(sympy.uppergamma(mxpr2sympy(x),mxpr2sympy(y)))
do_Gamma(mx::Mxpr{:Gamma},x,y,z) = sympy2mxpr(sympy.uppergamma(mxpr2sympy(x),mxpr2sympy(y)))

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

# use SymPy instead
# do_Erf(mx::Mxpr{:Erf}, b::SJSym) = b == :Infinity ? 1 : mx
# do_Erf(mx::Mxpr{:Erf}, b::Integer) = b == 0 ? 0 : mx

# use SymPy instead
# do_Zeta(mx::Mxpr{:Zeta}, b::Integer) = b == 0 ? -1//2 : b == -1 ? 1//12 : b == 4 ? 1//90 * :Pi^4 : mx

# TODO, use to symp
do_common("Log")
do_Log(mx::Mxpr{:Log},x::AbstractFloat) = x > 0 ? log(x) : log(complex(x))
do_Log{T<:AbstractFloat}(mx::Mxpr{:Log},x::Complex{T}) = log(x)
do_Log{T<:AbstractFloat}(mx::Mxpr{:Log},b::Real,z::Complex{T}) = log(b,z)
do_Log{T<:AbstractFloat}(mx::Mxpr{:Log},b::Real,z::T) = z > 0 ? log(b,z) : log(b,complex(z))

# This is probably quite slow, but might be correct in many cases
# The same idea could be used for other functions, such as sqrts etc.
function do_Log(mx::Mxpr{:Log},b::Integer,x::Integer)
    res = round(Int,log(b,x))
    return b^res == x ? res : mx
end
do_Log(mx::Mxpr{:Log},pow::Mxpr{:Power}) = do_Log(mx,pow,base(pow),expt(pow))
do_Log(mx::Mxpr{:Log},pow::Mxpr{:Power},b,e) = mx
do_Log(mx::Mxpr{:Log},pow::Mxpr{:Power},b::SJSym,e::Integer) = b == :E ? e : mx
do_Log(mx::Mxpr{:Log},b::SJSym) = b == :E ? 1 : mx

@sjdoc N "
N(expr) tries to give a the numerical value of expr.
N(expr,p) tries to give p decimal digits of precision.
"

# N needs to be rewritten
function apprules(mx::Mxpr{:N})
    do_N(margs(mx)...)
end

do_N(x,dig) = x
do_N(x) = x
do_N{T<:AbstractFloat}(n::T) = n
do_N{T<:Real}(n::T) = float(n)
do_N{T<:Real}(n::Complex{T}) = complex(float(real(n)), float(imag(n)))
do_N{T<:Real,V<:Integer}(n::T,p::V)  = float_with_precision(n,p)

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
#        pr = precision(BigFloat)
        pr = precision(BigFloat)
        dig = round(Int,p*3.322)
#        set_bigfloat_precision(dig)  # deprecated
        setprecision( BigFloat, dig) # new form
        res = BigFloat(x)
#        set_bigfloat_precision(pr)
        setprecision(BigFloat, pr)
        return res
    else
        return float(x)
    end
end

# These rely on fixed-point evaluation to continue with N, this is not efficient
# We need to do it all here.
function do_N(m::Mxpr)
    len = length(m)
    args = margs(m)
    nargs = newargs(len)
    @inbounds for i in 1:len
        nargs[i] = do_N(args[i])
    end
    return mxpr(mhead(m),nargs)
end

function do_N{T<:Integer}(m::Mxpr,p::T)
    len = length(m)
    args = margs(m)
    nargs = newargs(len)
    @inbounds for i in 1:len
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

function do_N{T<:Integer}(s::SJSym,pr::T)
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
Precision(x) gets the precision of a floating point number x, as defined by the
effective number of bits in the mantissa.
"
apprules(mx::Mxpr{:Precision}) = do_Precision(mx,margs(mx)...)
do_Precision(mx::Mxpr{:Precision},args...) = mx
do_Precision(mx::Mxpr{:Precision},x::AbstractFloat) = precision(x)

@sjdoc Re "
Re(x) returns the real part of z.
"

@sjdoc Im "
Im(x) returns the imaginary part of z.
"

# Mma allows complex numbers of mixed Real type. Julia does not.
# Implementation not complete. eg  Im(a + I *b) --> Im(a) + Re(b)

# This is already created above
#@mkapprule1 Re

do_Re{T<:Real}(mx::Mxpr{:Re}, x::Complex{T}) = real(x)
do_Re(mx::Mxpr{:Re}, x::Real) = x

function do_Re(mx::Mxpr{:Re}, m::Mxpr{:Times})
    f = m[1]
    return is_imaginary_integer(f) ? do_Re_imag_int(m,f) : mx
end

# dispatch on type of f. Maybe this is worth something.
function do_Re_imag_int(m,f)
    nargs = copy(margs(m))
    shift!(nargs)
    if length(nargs) == 1
        return mxpr(:Times,-imag(f),mxpr(:Im,nargs))
    else
        return mxpr(:Times,-imag(f),mxpr(:Im,mxpr(:Times,nargs)))
    end
end

#@mkapprule1 Im
do_Im{T<:Real}(mx::Mxpr{:Im}, x::Complex{T}) = imag(x)
do_Im(mx::Mxpr{:Im}, x::Real) = zero(x)

function do_Im(mx::Mxpr{:Im}, m::Mxpr{:Times})
    f = m[1]
    return is_imaginary_integer(f) ? do_Im_imag_int(m,f) : mx
end

function do_Im_imag_int(m,f)
    nargs = copy(margs(m))
    shift!(nargs)
    if length(nargs) == 1
        return mxpr(:Times,imag(f),mxpr(:Re,nargs))
    else
        return mxpr(:Times,imag(f),mxpr(:Re,mxpr(:Times,nargs)))
    end
end

#### Complex

@sjdoc Complex "
Complex(a,b) returns a complex number when a and b are Reals. This is done when the
expression is parsed, so it is much faster than 'a + I*b'.
"

# Complex with two numerical arguments is converted at parse time. But, the
# arguments may evaluate to numbers only at run time, so this is needed.
# mkapprule requires that the first parameter do_Complex be annotated with the Mxpr type.

@mkapprule Complex
do_Complex{T<:Number}(mx::Mxpr{:Complex},a::T,b::T) = complex(a,b)
do_Complex{T<:Number}(mx::Mxpr{:Complex},a::T) = complex(a)

@sjdoc Rational "
Rational(a,b), or a//b, returns a Rational for Integers a and b.  This is done when the
expression is parsed, so it is much faster than 'a/b'.
"

# Same here. But we need to use mdiv to reduce rationals to ints if possible.
@mkapprule Rational
do_Rational(mx::Mxpr{:Rational},a::Number,b::Number) = mdiv(a,b)

#### Rationalize

@sjdoc Rationalize "
Rationalize(x) returns a Rational approximation of x.
Rationalize(x,tol) returns an approximation differing from x by no more than tol.
"

@mkapprule Rationalize
do_Rationalize(mx::Mxpr{:Rationalize},x::AbstractFloat) = rationalize(x)
do_Rationalize(mx::Mxpr{:Rationalize},x::AbstractFloat,tol::Number) = rationalize(x,tol=float(tol))
function do_Rationalize(mx::Mxpr{:Rationalize},x::Symbolic)
    r = doeval(mxpr(:N,x))  # we need to redesign do_N so that we can call it directly. See above
    return is_type_less(r,AbstractFloat) ? do_Rationalize(mx,r) : x
end
function do_Rationalize(mx::Mxpr{:Rationalize},x::Symbolic,tol::Number)
    ndig = round(Int,-log10(tol))      # This is not quite correct.
    r = doeval(mxpr(:N,x,ndig))  # we need to redesign do_N so that we can call it directly. See above.
    return is_type_less(r,AbstractFloat) ? do_Rationalize(mx,r,tol) : x
end
do_Rationalize(mx::Mxpr{:Rationalize},x) = x

#### Numerator

@sjdoc Numerator "
Numerator(expr) returns the numerator of expr.
"

apprules(mx::Mxpr{:Numerator}) = do_Numerator(mx::Mxpr{:Numerator},margs(mx)...)
do_Numerator(mx::Mxpr{:Numerator},args...) = mx

do_Numerator(mx::Mxpr{:Numerator},x::Rational) = num(x)
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

find_numerator(x::Rational) = num(x)
find_numerator(x::Mxpr{:Power}) = pow_sign(x,expt(x)) > 0 ? x : 1

function pow_sign(x, texpt::Mxpr{:Times})
    fac = texpt[1]
    return is_Number(fac) && fac < 0 ? -1 : 1
end

pow_sign(x, texpt::Number) = texpt < 0 ? -1 : 1
pow_sign(x, texpt) = 1
find_numerator(x) = x

## Chop

@sjdoc Chop "
Chop(expr) sets small floating point numbers in expr to zero.
Chop(expr,eps) sets floating point numbers smaller in magnitude than eps in expr to zero.
"

const chop_eps = 1e-14

zchop{T<:AbstractFloat}(x::T, eps=chop_eps) = abs(x) > eps ? x : 0  # don't follow abstract type
zchop{T<:Number}(x::T, eps=chop_eps) = x
zchop{T<:AbstractFloat}(x::Complex{T}, eps=chop_eps) = complex(zchop(real(x),eps),zchop(imag(x),eps))
zchop{T<:AbstractArray}(a::T, eps=chop_eps) = (b = similar(a); for i in 1:length(a) b[i] = zchop(a[i],eps) end ; b)
zchop!{T<:AbstractArray}(a::T, eps=chop_eps) = (for i in 1:length(a) a[i] = zchop(a[i],eps) end ; a)
zchop(x::Expr,eps=chop_eps) = Expr(x.head,zchop(x.args)...)
zchop(x) = x
zchop(x,eps) = x
# This does not work. Int is a collection in Julia. How can we distinguish an actual collection from a number ?
#zchop(x) = applicable(start,x) ? map(zchop,x) : x
#zchop(x,eps) =  applicable(start,x) ? map((x)->zchop(x,eps),x) : x
#zchop{T<:Union{AbstractString,Char}}(x::T,eps=chop_eps) = x
#zchop{T<:Irrational}(x::T,eps=chop_eps) = zchop(float(x),eps)

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

@mkapprule Chop  :nargs => 1:2
#apprules(mx::Mxpr{:Chop}) = do_Chop(mx,margs(mx)...)
do_Chop(mx::Mxpr{:Chop}, x) = zchop(x)
do_Chop(mx::Mxpr{:Chop}, x, zeps) = zchop(x,zeps)
do_Chop(mx::Mxpr{:Chop}, x...) = mx

#### Exp

# The parser normally takes care of this,
# But, when converting expressions from Sympy, we get Exp, so we handle it here.
# TODO: don't throw away other args. (but, we hope this is only called with returns from Sympy)
apprules(mx::Mxpr{:Exp}) = mxpr(:Power,:E,mx[1])


# TODO: handle 3rd argument
#### Mod
@mkapprule  Mod  :nargs => 2
do_Mod{T<:Integer, V<:Integer}(mx::Mxpr{:Mod}, x::T, y::V) = mod1(x,y)
#do_Mod{T<:Integer, V<:Integer, W<:Integer}(mx::Mxpr{:Mod}, x::T, y::V, d::W) = div(x,y) + d

#### DivRem
@mkapprule  DivRem  :nargs => 2
do_DivRem{T<:Integer, V<:Integer}(mx::Mxpr{:DivRem}, x::T, y::V) = mxprcf(:List,divrem(x,y)...)


#### Sign

@mkapprule Sign :nargs => 1

do_Sign{T<:Number}(mx::Mxpr{:Sign}, x::T) = sign(x)
do_Sign(mx::Mxpr{:Sign}, x) = mx

# Maybe each symbol should be a type. Like irrational or Mxpr
function do_Sign(mx::Mxpr{:Sign}, x::Symbol)
    x == :Pi && return 1
    x == :E && return 1
    x == :EulerGamma && return 1
    return mx
end

function do_Sign{T<:Real}(mx::Mxpr{:Sign}, z::Complex{T})
    av = mpow(real(z*conj(z)),-1//2)
    av == 1 ? z : mxprcf(:Times, z,  av)
end

@sjdoc ExpandFunc "
ExpandFunc(expr) rewrites some multi-parameter special functions using simpler functions.
"


@sjdoc I "
I is the imaginary unit
"

@sjdoc E "
E is the base of the natural logarithm
"

@sjdoc Pi "
Pi is the trigonometric constant π.
"

nothing

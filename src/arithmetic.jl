using Primes

# Divide and multiply integers and rationals
# like a CAS does. Always return exact result (no floats).
# Return Int if possible. We want to avoid infection Rationals
# and putting explicit conversions everywhere. Examples:

#  12/6 --> 2, not 2.0
#  13/6 --> 13//6
#  (13//6) * 6 --> 13
rat_to_int(r::Rational{T}) where {T<:Integer} = r.den == 1 ? r.num : r

@doc """
    mmul(x,y), mpow(x,y), mplus(x,y), mminus(x), mdiv(x,y)
arithmetic funtions for Symata. These are similar to `*`, `^`, `+`, etc. in Julia. But there are important differences.

- rational numbers with denominator equal to `1` are converted to an integer type.
- complex numbers with imaginary part equal to zero are converted to a real type.
- for symbolic arguments, or mixed symbolic-numeric arguments a symata expression of type `Mxpr` is constructed.
- integer factors are extracted from roots. (overzealously at the moment).
""" arithmetic

@doc (@doc arithmetic) mmul
@doc (@doc arithmetic) mplus
@doc (@doc arithmetic) mpow
@doc (@doc arithmetic) mdiv
@doc (@doc arithmetic) mminus

## FIXME: Int or Integer ?
mmul(x::Int, y::Rational{T}) where {T<:Integer} = (res = x * y; return res.den == 1 ? res.num : res )

mmul(x::Complex{V}, y::Rational{T}) where {T<:Integer,V<:Integer} = (res = x * y; return (res.im.den == 1 && res.re.den == 1) ? Complex(res.re.num,res.im.num) : res )
mmul(x::Rational{V}, y::Complex{T}) where {T<:Integer,V<:Integer} = (res = x * y; return (res.im.den == 1 && res.re.den == 1) ? Complex(res.re.num,res.im.num) : res )

## ???
## mmul{T<:Integer,U<:Rational}(x::Complex{T}, y::Complex{U}) = mmul(y,x)

function mmul(x::Complex{U}, y::Complex{T}) where {T<:Integer,U<:Rational}
    res = x * y
    isa(res,Complex) && return res
    res.den == 1 && return res.num
    res
end

## Julia returns different types depending on the order of x and y
function mmul(x::Complex{T}, y::Complex{U}) where {T<:Integer,U<:Rational}
    res = x * y
    if isa(res,Complex)
        imag(res).num != 0 && return res
        real(res).den == 1 && return real(res).num
    end
    res
end

mmul(x::Rational{T}, y::Int) where {T<:Integer} = (res = x * y; return res.den == 1 ? res.num : res )

# Oct 2016. Added generic mxpr(:Times,x,y). Does not break tests.
# Why ? This allows Compiled (ie. Julia) functions to take far more arguments.
# FIXME. Use mmul in Symata code instead of *. We want to remove undesired methods for *
mmul(x::T,y::V) where {T<:Number, V<:Number} = x * y
mmul(x,y) = mxpr(:Times, x, y)

mplus(x::Rational{T}, y::Rational{V}) where {T<:Integer,V<:Integer} = rat_to_int(x+y)

mplus(x::T,y::V) where {T<:Number, V<:Number} = x + y
mplus(x,y) = mxpr(:Plus, x, y)

mminus(x) = mxpr(:Times,-1,x)
mminus(x::Number) = -x

mminus(x,y) = mxpr(:Plus, x, mxpr(:Times,-1,y))
mminus(x::Number,y::Number) = x-y

# mdiv is apparently used in do_Rational, which seems to be the only entry point to mdiv.
mdiv(x::Integer, y::Integer) =  y == 0 ? ComplexInfinity : rem(x,y) == 0 ? div(x,y) : x // y
_mdiv_float(x, y) = y == 0 ? ComplexInfinity : div(x,y)
mdiv(x::Integer, y::AbstractFloat) = _mdiv_float(x, y)
mdiv(x::AbstractFloat, y::Integer) = _mdiv_float(x, y)
mdiv(x::AbstractFloat, y::AbstractFloat) = _mdiv_float(x, y)

mdiv(x::Integer, y::Rational) = (res = x / y; return res.den == 1 ? res.num : res )
mdiv(x::Number,y::Number) = x/y
# Should be a fixed Mxpr ?
mdiv(x,y) = mxpr(:Times, x, mxpr(:Power,y,-1))

function mpow(x,y)
#    @show (x,y)
    _mpow(x,y)
end
function _mpow(x::Number, y::Number)
    try
        x^y
    catch e
        println("Error computing $x^$y")
        rethrow(e)
    end
end
function _mpow(x::Rational,y::Integer)
    if x > 0 && y < 0
        my = -y
        (denominator(x)^(my))//(numerator(x)^(my))
    else
        x^y  # will fail
    end
end
function _mpow(x::Irrational, n::Number)
    n >= 0 && return x^n
    (x1,n1) = promote(x,n)
    x1^n1
end

_mpow(x,y) =  mxpr(:Power, x, y)

function _mpow(x::Integer, y::V) where V<:AbstractFloat
    x >= 0 && return convert(V,x)^y
    res = convert(Complex{V},x)^y
    imag(res) == 0 && return real(res)
    res
end

_mpow(x::Complex{T},y::V) where {T<:Integer,V<:AbstractFloat} = convert(Complex{V},x)^y
_mpow(x::Integer, y::Integer) = y >= 0 ? x^y : x == 0 ? ComplexInfinity : 1//(x^(-y))
_mpow(x::AbstractFloat, y::Integer) = y >= 0 ? x^y : x == 0 ? ComplexInfinity : x^y
_mpow(x::Symata.Mxpr{:DirectedInfinity}, y::T) where {T<:Real} = y > 0 ? x : 0
_mpow(x::Symata.Mxpr{:DirectedInfinity}, y::Complex{T}) where {T<:Real} = y.re > 0 ? x : 0
# could be more efficient: cospi(exp) + im * sinpi(exp)
_mpow(b::T,exp::V) where {T<:AbstractFloat,V<:Number} = b < 0 ? (cospi(exp) + im * sinpi(exp)) * abs(b)^exp : b^exp

# FIXME: This code and function do_Power{T<:Integer}(mx::Mxpr{:Power},b::T,e::Rational)
# in apprules are in conflict.
# Maybe Fixed. We are using this routine.
# Note: do_Power{T<:Integer}(mx::Mxpr{:Power},b::T,e::Rational) is disabled.
# We could research how to do this instead of rolling our own. But, this seems to
# work.
# function _mpow(x::T,y::Rational{V}) where {T<:Integer, V<:Integer}
function _mpow(x::Integer, y::Rational{V}) where {V<:Integer}
    x == 1 && return x
    local gotneg::Bool
    if x < 0
        x = -x
        gotneg = true
    else
        gotneg = false
    end
    facs = factor(x)
    newfacs = newargs()
    for (fac,mul) in facs
        (q,r) = divrem(mul,y.den)
        if r == 0
            nf = fac^(q*abs(y.num))
            nf != 1 ? y.num > 0 ? push!(newfacs,nf) : push!(newfacs,1//nf) : nothing
            continue
        else
            nf = fac^q
            nf != 1 ? y.num > 0 ? push!(newfacs,nf) : push!(newfacs,1//nf) : nothing
        end
        (nf1,r1) = divrem(r*y.num,y.den)
        if nf1 != 0
            newfac = mpow(fac,nf1)
            push!(newfacs,newfac)
        end
        if r1 != 0
            newexp = r1//(y.den)
            push!(newfacs,mxpr(:Power,fac,newexp)) # Optimize. at some point, do mxprfc and test
        end
    end
    if gotneg == true  # This at least not wrong, but it's not like Mma for y < 0
        (n,r) = divrem(y.num,y.den)
        fac = y.den == 2 ? I : mxpr(:Power, -1, r//y.den)
        if iseven(n)
            push!(newfacs, fac)
        else
            push!(newfacs,mxpr(:Times, -1, fac))
        end
    end
    newfacs = mpow_test_for_integer_factors(newfacs)
    length(newfacs) == 1 && return newfacs[1]
    # We may need to doeval this to use in Julia functions. Or doeval in the function. Eg.
    # pattern to test for perfect squares
    # t = _:?(:( (x) -> typeof(mpow(x,1//2)) <: Integer ))
    expr = mxpr(:Times, newfacs...)
    expr
end

# If all factors are integers, just multiply them
function mpow_test_for_integer_factors(facs)
    allints = true
    for x in facs
        typeof(x) <: Integer && continue
        allints = false
        break
    end
    return allints ? [*(facs...)] : facs
end


# The following expression will call this method:  (-27/64)^(2/3) == 9/16*((-1)^(2/3))
# Also called by Norm([1,I/2])
_mpow(x::Rational{T}, y::Rational{V}) where {T<:Integer, V<:Integer} =  mxpr(:Times,mpow(x.num,y), mxpr(:Power,mpow(x.den,y), -1))

### Code below handles z^n for z complex and n real.

# Is there not an automatic way ?
_divide_pow(x::T,y::T) where {T<:AbstractFloat} = x/y
_divide_pow(x::Complex{T},y::T) where {T<:AbstractFloat} = x/y
_divide_pow(x::T,y::T) where {T<:Integer} = x//y
_divide_pow(x::Complex{T},y::T) where {T<:Integer} = x//y
_divide_pow(x::Complex{Rational{T}},y::Rational{T}) where {T<:Integer} = x//y

function _mpow(base::Complex{T}, expt::V) where {T<:Real,V<:Union{AbstractFloat, Integer}}
    expt >= 0 && return base^expt
    if expt == -one(expt)
        if base == complex(zero(real(base)),one(real(base)))
            return complex(zero(real(base)),-one(real(base)))
        else
            return _divide_pow(conj(base),abs2(base))
        end
    else
        res = _divide_pow((conj(base)^(-expt)) , abs2(base)^(-expt))
        if imag(res) == zero(imag(res))
            return real(res)
        else
            return res
        end
    end
end

#### mabs

mabs(x::Number) = abs(x)

mabs(x) = mxpr(:Abs, x)

# TODO.  T Rational
function mabs(x::Complex{T}) where T<:Integer
    r,i = reim(x)
    sq = r*r + i*i
    x = isqrt(sq)
    if x * x == sq
        return x
    else
        res = mxpr(:Power,sq,1//2)
        setfixed(res)
        mergesyms(res,:nothing)
        return res
    end
end

# mpow for Rational is wrong often wrong.
# so this will be broken, too.
function mabs(x::Complex{Rational{T}}) where T<:Integer
    r,i = reim(x)
    sq = mplus(mmul(r,r),mmul(i,i))
    return mpow(sq,1//2)
end

## copied from base/operators, a great trick
for op = (:mplus, :mmul)
    @eval begin
        # note: these definitions must not cause a dispatch loop when +(a,b) is
        # not defined, and must only try to call 2-argument definitions, so
        # that defining +(a,b) is sufficient for full functionality.

        # ($op){T<:Number}(a::T, b::T, c::T)        = ($op)(($op)(a,b),c)
        # ($op){T<:Number}(a::T, b::T, c::T, xs::T...) = ($op)(($op)(($op)(a,b),c), xs...)

        # Oct 2016 Compiled Symata translates mxpr(:Times,x,y,z,...) to mmul(x,y,z,...).
        # In order for this to work, we allow any combination of numbers.
        ($op)(a::Number, b::Number, c::Number)        = ($op)(($op)(a,b),c)
        ($op)(a::Number, b::Number, c::Number, xs::Number...) = ($op)(($op)(($op)(a,b),c), xs...)
        # a further concern is that it's easy for a type like (Int,Int...)
        # to match many definitions, so we need to keep the number of
        # definitions down to avoid losing type information.
    end
end

## `Compile` and other functions may generate `mmul(x,y,z)` where some of the args are not numbers.
## These catch cases where some of args are not numbers.
## This may want to implment more logic for efficiency
mmul(args...) = mxpr(:Times, args...)
mplus(args...) = mxpr(:Times, args...)

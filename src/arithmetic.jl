using Primes

# Divide and multiply integers and rationals
# like a CAS does. Always return exact result (no floats).
# Return Int if possible. We want to avoid infection Rationals
# and putting explicit conversions everywhere. Examples:

#  12/6 --> 2, not 2.0
#  13/6 --> 13//6
#  (13//6) * 6 --> 13
rat_to_int{T<:Integer}(r::Rational{T}) = r.den == 1 ? r.num : r

@doc """
    mmul(x,y), mpow(x,y), mplus(x,y), mminus(x), mminus(x,y)
arithmetic funtions for Symata. These are similar to `*`, `^`, `+`, etc. n Julia. But there are important differences.

- rational numbers with denominator equal to `1` are converted to an integer type on return.
- complex numbers with imaginary part equal to zero are converted to a real type on return.
- for symbolic arguments, or mixed symbolic-numeric arguments a symata expression of type `Mxpr` is constructed.
- integer factors are extracted from roots. (overzealously at the moment)
""" arithmetic


@doc (@doc arithmetic) mmul
@doc (@doc arithmetic) mplus
@doc (@doc arithmetic) mpow
@doc (@doc arithmetic) mdiv
@doc (@doc arithmetic) mminus


mmul{T<:Integer}(x::Int, y::Rational{T}) = (res = x * y; return res.den == 1 ? res.num : res )

mmul{T<:Integer,V<:Integer}(x::Complex{V}, y::Rational{T}) = (res = x * y; return (res.im.den == 1 && res.re.den == 1) ? Complex(res.re.num,res.im.num) : res )
mmul{T<:Integer,V<:Integer}(x::Rational{V}, y::Complex{T}) = (res = x * y; return (res.im.den == 1 && res.re.den == 1) ? Complex(res.re.num,res.im.num) : res )

## ???
## mmul{T<:Integer,U<:Rational}(x::Complex{T}, y::Complex{U}) = mmul(y,x)

function mmul{T<:Integer,U<:Rational}(x::Complex{U}, y::Complex{T})
    res = x * y
    isa(res,Complex) && return res
    res.den == 1 && return res.num
    res
end

## Julia returns different types depending on the order of x and y
function mmul{T<:Integer,U<:Rational}(x::Complex{T}, y::Complex{U})
    res = x * y
    if isa(res,Complex)
        imag(res).num != 0 && return res
        real(res).den == 1 && return real(res).num
    end
    res
end

mmul{T<:Integer}(x::Rational{T}, y::Int) = (res = x * y; return res.den == 1 ? res.num : res )

# Oct 2016. Added generic mxpr(:Times,x,y). Does not break tests.
# Why ? This allows Compiled (ie. Julia) functions to take far more arguments.
# FIXME. Use mmul in Symata code instead of *. We want to remove undesired methods for *
mmul{T<:Number, V<:Number}(x::T,y::V) = x * y
mmul(x,y) = mxpr(:Times, x, y)

mplus{T<:Integer,V<:Integer}(x::Rational{T}, y::Rational{V}) = rat_to_int(x+y)

mplus{T<:Number, V<:Number}(x::T,y::V) = x + y
mplus(x,y) = mxpr(:Plus, x, y)

mminus(x) = mxpr(:Times,-1,x)
mminus(x::Number) = -x

mminus(x,y) = mxpr(:Plus, x, mxpr(:Times,-1,y))
mminus(x::Number,y::Number) = x-y

# mdiv is apparently used in do_Rational, but this should never be used now.
mdiv{T<:Integer,V<:Integer}(x::T, y::V) =  y == 0 ? ComplexInfinity : rem(x,y) == 0 ? div(x,y) : x // y
mdiv{T<:Integer}(x::Int, y::Rational{T}) = (res = x / y; return res.den == 1 ? res.num : res )

mdiv(x::Number,y::Number) = x/y
mdiv(x,y) = mxpr(:Times,x, mxpr(:Power,y,-1))

function mpow(x,y)
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

function _mpow{T<:Integer,V<:AbstractFloat}(x::T,y::V)
    x >= 0 && return convert(V,x)^y
    res = convert(Complex{V},x)^y
    imag(res) == 0 && return real(res)
    res
end

_mpow{T<:Integer,V<:AbstractFloat}(x::Complex{T},y::V) = convert(Complex{V},x)^y

_mpow{T<:Integer,V<:Integer}(x::T,y::V) = y >= 0 ? x^y : x == 0 ? ComplexInfinity : 1//(x^(-y))
_mpow{T<:Real}(x::Symata.Mxpr{:DirectedInfinity}, y::T) = y > 0 ? x : 0

_mpow{T<:Real}(x::Symata.Mxpr{:DirectedInfinity}, y::Complex{T}) = y.re > 0 ? x : 0

# could be more efficient: cospi(exp) + im * sinpi(exp)
_mpow{T<:AbstractFloat,V<:Number}(b::T,exp::V) = b < 0 ? (cospi(exp) + im * sinpi(exp)) * abs(b)^exp : b^exp

# FIXME: This code and function do_Power{T<:Integer}(mx::Mxpr{:Power},b::T,e::Rational)
# in apprules are in conflict.
# Maybe Fixed. We are using this routine.
# Note: do_Power{T<:Integer}(mx::Mxpr{:Power},b::T,e::Rational) is disabled.
# We could research how to do this instead of rolling our own. But, this seems to
# work.
function _mpow{T<:Integer, V<:Integer}(x::T,y::Rational{V})
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
#            newfac = mpow(fac,nf1)
            newfac = fac^nf1
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
_mpow{T<:Integer, V<:Integer}(x::Rational{T}, y::Rational{V}) =  mxpr(:Times,mpow(x.num,y), mxpr(:Power,mpow(x.den,y), -1))

### Code below handles z^n for z complex and n real.

# Is there not an automatic way ?
_divide_pow{T<:AbstractFloat}(x::T,y::T) = x/y
_divide_pow{T<:AbstractFloat}(x::Complex{T},y::T) = x/y
_divide_pow{T<:Integer}(x::T,y::T) = x//y
_divide_pow{T<:Integer}(x::Complex{T},y::T) = x//y
_divide_pow{T<:Integer}(x::Complex{Rational{T}},y::Rational{T}) = x//y

function _mpow{T<:Real,V<:Union{AbstractFloat, Integer}}(base::Complex{T}, expt::V)
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
function mabs{T<:Integer}(x::Complex{T})
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
function mabs{T<:Integer}(x::Complex{Rational{T}})
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

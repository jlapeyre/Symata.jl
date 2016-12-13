## Do not precompile. The pointer to gmp functions needs to be found at run time.

#module PolynomialSequences

using Polynomials
using Memoize

export fibpoly, lucaspoly

fibpoly(n::Int) = _fib_poly(n)
fibpoly(n::BigInt) = _fib_poly_big(n)

lucaspoly(n::Int) = _lucas_poly(n)
lucaspoly(n::BigInt) = _lucas_poly_big(n)

## TODO: refactor code

## This requires Dict maybe ?
let myzero = big(0), myone = big(1), xvar = Polynomials.Poly([myzero,myone]), zerovar = Polynomials.Poly([myzero]),  onevar = Polynomials.Poly([myone])
    global _fib_poly_big
    @memoize Dict function _fib_poly_big(n::BigInt)
        if n == 0
            return zerovar
        elseif n == 1
            return onevar
        else
            return xvar *  _fib_poly_big(n-1) +  _fib_poly_big(n-2)
        end
    end
end

let myzero = 0, myone = 1, xvar = Polynomials.Poly([myzero,myone]), zerovar = Polynomials.Poly([myzero]),  onevar = Polynomials.Poly([myone])
    global _fib_poly
    @memoize function _fib_poly(n::Int)
        if n == 0
            return zerovar
        elseif n == 1
            return onevar
        else
            return xvar *  _fib_poly(n-1) +  _fib_poly(n-2)
        end
    end
end

let  myzero = big(0), myone = big(1), mytwo = big(2), xvar = Polynomials.Poly([myzero,myone]),  twovar = Polynomials.Poly([mytwo])
    global _lucas_poly_big
    @memoize Dict function _lucas_poly_big(n::BigInt)
        if n == 0
            return twovar
        elseif n == 1
            return xvar
        else
            return xvar *  _lucas_poly_big(n-1) +  _lucas_poly_big(n-2)
        end
    end
end

let myzero = 0, myone = 1, mytwo = 2, xvar = Polynomials.Poly([myzero,myone]),  twovar = Polynomials.Poly([mytwo])
    global _lucas_poly
    @memoize function _lucas_poly(n::Int)
        if n == 0
            return twovar
        elseif n == 1
            return xvar
        else
            return xvar * _lucas_poly(n-1) +  _lucas_poly(n-2)
        end
    end
end

#end # module

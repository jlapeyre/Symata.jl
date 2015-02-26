using Base.Test

@ex ClearAll(a,b,x,y,z,p,q,res,f)

## Factor, Expand

@testex  Factor(Expand( (a+b)^2 )) == (a+b)^2
@testex  Expand( (a + f(x)) ^2 ) == a ^ 2 + 2 * a * f(x) + f(x) ^ 2
@ex      p = Expand((x-1)*(x-2)*(x-3)*(x^2 + x + 1))
@testex  p == -6 + 5 * x + -1 * (x ^ 2) + 6 * (x ^ 3) + -5 * (x ^ 4) + x ^ 5
@testex  Factor(p) == (-3 + x) * (-2 + x) * (-1 + x) * (1 + x + x ^ 2)

## Limit

@testex Limit(x, x => 0) == 0
@testex Limit(Sin(x)/x, x => Infinity) == 0
@ex     f = :( fff(x) = x^10 )
@testex Limit( (f(x+h) - f(x))/h, h => 0) == 10 * (x ^ 9)
# We need to fix this. Inf is a Float64. Convert it to SJulia
@testex Limit( 1/(x^(Log(Log(Log(Log((1/x)))))-1)), x => 0) == :( Inf )
# Mma 3 cannot do the following:
@testex Limit( Log(Log(x*Exp(x*Exp(x)) + 1)) - Exp(Exp(Log(Log(x)) + 1/x)), x => Infinity) == 0

## Integrate

@testex Integrate(x,x) == 1//2 * x^2
@testex Integrate(x,[x,0,1]) == 1//2
@testex Integrate(x,[x,0,1],y) == 1//2 * y

## D

@testex D(x^2,x) == 2 * x
@testex D(x^3,x,2) == 6 * x
# FIXME Exp is not translated properly
# We convert Exp(x) = E^x in an apprules. Probably better to convert it
# postprocessing sympy output or something. Anyway, the tests pass
@testex D(Exp(x),x) == Exp(x)
@testex  D(Exp(Exp(x)),x) == Exp(x) * Exp(Exp(x))

## Together, Apart

@ex     z = ( 1/x + 1/(x+1))
@testex Together(z) == (x ^ -1) * ((1 + x) ^ -1) * (1 + 2 * x)
@testex Apart(Together(z)) == z

## Simplify

@testex Simplify( Cos(x)^2 + Sin(x)^2) == 1

## Solve

@ex      res = Solve([x+y-1, x - y + 1], [x,y])
@testex  res[x] == 0
@testex  res[y] == 1
@testex  Solve(x^4-1,x) == [-1,1,-1I,I]
@testex  Solve(x^3-1,x) == [1,-1//2 + (-1//2*I) * (3 ^ (1//2)),-1//2 + (1//2*I) * (3 ^ (1//2))]

## Roots

@ex      q = x^2 - 8x + 8
@testex  Roots(q) == [[4 + -2 * (2 ^ (1//2)),1],[4 + 2 * (2 ^ (1//2)),1]]

@ex ClearAll(a,b,x,y,z,p,q,rex,f)

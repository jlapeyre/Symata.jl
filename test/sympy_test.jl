using Base.Test



@testex testUserSyms

#### Limit

@testex Limit(x, x => 0) == 0
@testex Limit(Sin(x)/x, x => Infinity) == 0
@ex     f = :( fff(x) = x^10 )
@testex Limit( (f(x+h) - f(x))/h, h => 0) == 10 * (x^9)
# We need to fix this. Inf is a Float64. Convert it to SJulia
@testex Limit( 1/(x^(Log(Log(Log(Log((1/x)))))-1)), x => 0) == :( Inf )
# Mma 3 cannot do the following:
@testex Limit( Log(Log(x*Exp(x*Exp(x)) + 1)) - Exp(Exp(Log(Log(x)) + 1/x)), x => Infinity) == 0

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


## Orthoganal Polynomials, etc.

@testex JacobiP(1,2,3,x)  == -1//2 + (7//2) * x

## Trig
@testex Sin(Pi/4) == (1//2) * (2 ^ (1//2))

@testex Integrate(DiracDelta(x-1), [x,-Infinity, Infinity]) == 1
@testex Integrate(DiracDelta(x-1), [x,2, Infinity]) == 0
@testex Integrate(DiracDelta(x-1), [x,-1000, 1000]) == 1
@testex Head(DiracDelta(0)) ==  DiracDelta
@testex DiracDelta(1) == 0

@ex ClearAll(x,i,conds,h,res,t,s)

@ex ex = x^5 - x^3 - x^2 + 1
@testex FactorSquareFree(ex) == ((-1 + x) ^ 2) * (1 + 2 * x + 2 * (x ^ 2) + x ^ 3)
@testex Factor(ex) == ((-1 + x) ^ 2) * (1 + x) * (1 + x + x ^ 2)
@ex ClearAll(x,ex)


#### BellB

@testex BellB(30) == 846749014511809332450147
@testex BellB(6,4,[x1,x2,x3]) == 45 * (x1 ^ 2) * (x2 ^ 2) + 20 * (x1 ^ 3) * x3
@testex BellB(4,t) == t + 7 * (t ^ 2) + 6 * (t ^ 3) + t ^ 4


#### Divisors

@testex 10000/Divisors(10000) == Reverse(Divisors(10000))



@ex ClearAll(x1,x2,x3,t,x)
@testex testUserSyms

################

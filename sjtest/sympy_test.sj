### Limit

T Limit(x, x => 0) == 0
T Limit(Sin(x)/x, x => Infinity) == 0

  f = J( x -> mpow(x,10) )
T Limit( (f(x+h) - f(x))/h, h => 0) == 10 * (x^9)

  f = J( x -> x^10)
T Limit( (f(x+h) - f(x))/h, h => 0) == 10 * (x^9)


# We need to fix this. Inf is a Float64. Convert it to SJulia
T Limit( 1/(x^(Log(Log(Log(Log((1/x)))))-1)), x => 0) == Infinity
# Mma 3 cannot do the following:
T Limit( Log(Log(x*Exp(x*Exp(x)) + 1)) - Exp(Exp(Log(Log(x)) + 1/x)), x => Infinity) == 0

ClearAll(isdef)

## Solve

#    res = Solve([x+y-1, x - y + 1], [x,y])
# T  res[x] == 0
# T  res[y] == 1
T  Solve([x+y-1, x - y + 1], [x,y]) == [[y => 1,x => 0]]  # todo. sort lexically
T  Solve(x^4-1,x) == [[x => (-1)],[x => 1],[x => -I],[x => I]]
#T  Solve(x^4-1,x) == [-1,1,-1I,I]
#T  Solve(x^3-1,x) == [1,-1//2 + (-1//2*I) * (3 ^ (1//2)),-1//2 + (1//2*I) * (3 ^ (1//2))]
T  Solve(x^3-1,x) == [[x => 1],[x => (-1/2 + (I * -1/2)*(3^(1/2)))],[x => (-1/2 + (I * 1/2)*(3^(1/2)))]]

# FIXME: Solve(x^3-1) should return rules as in examples above.
#T  Solve(x^3-1) == Solve(x^3-1,x)

## Roots

   q = x^2 - 8x + 8
T  Roots(q) == [[4 + -2 * (2 ^ (1//2)),1],[4 + 2 * (2 ^ (1//2)),1]]

 ClearAll(a,b,x,y,z,p,q,rex,f)

## Orthogonal Polynomials, etc.

T JacobiP(1,2,3,x)  == -1//2 + (7//2) * x

## Trig
T Sin(Pi/4) == (1//2) * (2 ^ (1//2))

### DiracDelta

T Integrate(DiracDelta(x-1), [x,-Infinity, Infinity]) == 1
T Integrate(DiracDelta(x-1), [x,2, Infinity]) == 0
T Integrate(DiracDelta(x-1), [x,-1000, 1000]) == 1
T Head(DiracDelta(0)) ==  DiracDelta
T DiracDelta(1) == 0

 ClearAll(x,i,conds,h,res,t,s)

 ex = x^5 - x^3 - x^2 + 1
T FactorSquareFree(ex) == ((-1 + x) ^ 2) * (1 + 2 * x + 2 * (x ^ 2) + x ^ 3)
T Factor(ex) == ((-1 + x) ^ 2) * (1 + x) * (1 + x + x ^ 2)
 ClearAll(x,ex)


### BellB

T BellB(30) == 846749014511809332450147
T BellB(6,4,[x1,x2,x3]) == 45 * (x1 ^ 2) * (x2 ^ 2) + 20 * (x1 ^ 3) * x3
T BellB(4,t) == t + 7 * (t ^ 2) + 6 * (t ^ 3) + t ^ 4


### Divisors

T 10000/Divisors(10000) == Reverse(Divisors(10000))

 ClearAll(x1,x2,x3,t,x)



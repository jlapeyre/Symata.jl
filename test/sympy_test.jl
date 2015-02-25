using Base.Test

@ex ClearAll(a,b,x,y,z,p,q,res)
@testex  Factor(Expand( (a+b)^2 )) == (a+b)^2
@testex  Expand( (a + f(x)) ^2 ) == a ^ 2 + 2 * a * f(x) + f(x) ^ 2
@ex      p = Expand((x-1)*(x-2)*(x-3)*(x^2 + x + 1))
@testex  p == -6 + 5 * x + -1 * (x ^ 2) + 6 * (x ^ 3) + -5 * (x ^ 4) + x ^ 5
@testex  Factor(p) == (-3 + x) * (-2 + x) * (-1 + x) * (1 + x + x ^ 2)

@testex Limit(x, x => 0) == 0

@testex Integrate(x,x) == 1//2 * x^2
@testex Integrate(x,[x,0,1]) == 1//2
@testex Integrate(x,[x,0,1],y) == 1//2 * y

@testex D(x^2,x) == 2 * x
@testex D(x^3,x,2) == 6 * x

@ex     z = ( 1/x + 1/(x+1))
@testex Together(z) == (x ^ -1) * ((1 + x) ^ -1) * (1 + 2 * x)
@testex Apart(Together(z)) == z

@testex Simplify( Cos(x)^2 + Sin(x)^2) == 1

@ex      res = Solve([x+y-1, x - y + 1], [x,y])
@testex  res[x] == 0
@testex  res[y] == 1
@testex  Solve(x^4-1,x) == [-1,1,-1I,I]
@testex  Solve(x^3-1,x) == [1,-1//2 + (-1//2*I) * (3 ^ (1//2)),-1//2 + (1//2*I) * (3 ^ (1//2))]

@ex      q = x^2 - 8x + 8
@testex  Roots(q) == [[4 + -2 * (2 ^ (1//2)),1],[4 + 2 * (2 ^ (1//2)),1]]

@ex ClearAll(a,b,x,y,z,p,q,rex)

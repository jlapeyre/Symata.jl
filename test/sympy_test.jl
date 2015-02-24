using Base.Test

@ex ClearAll(a,b,x,y,z,res)
@testex Factor(Expand( (a+b)^2 )) == (a+b)^2
@testex Limit(x, x => 0) == 0
@testex Integrate(x,x) == 1//2 * x^2
@testex Integrate(x,[x,0,1]) == 1//2
@testex Integrate(x,[x,0,1],y) == 1//2 * y
@testex D(x^2,x) == 2 * x
@ex     z = ( 1/x + 1/(x+1))
@testex Together(z) == (x ^ -1) * ((1 + x) ^ -1) * (1 + 2 * x)
@testex Apart(Together(z)) == z
@testex Simplify( Cos(x)^2 + Sin(x)^2) == 1
@ex   res = Solve([x+y-1, x - y + 1], [x,y])
@testex  res[x] == 0
@testex  res[y] == 1
@ex ClearAll(a,b,x,y,z,rex)

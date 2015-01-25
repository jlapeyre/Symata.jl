using Base.Test

@ex ClearAll(a,b,c,p,f,d)
@testex ReplaceAll( f([a,b]) + f(c) , f([x_,y_]) => p(x+y)) == f(c) + p(a+b)
@testex Apply(List,ReplaceAll( f([a,b]) + f(c) , f([x_,y_]) => p(x+y))) == [f(c),p(a + b)]
@testex ReplaceAll([a/b, 1/b^2, 2/b^2] , b^n_ => d(n)) == [a*d(-1),d(-2),2*d(-2)]
@testex ReplaceAll( [a,b,[a,b]] , [x_,y_,[x_,y_]] => 1 ) == 1
@testex ReplaceAll( [b,a,[a,b]] , [x_,y_,[x_,y_]] => 1 ) == [b,a,[a,b]]

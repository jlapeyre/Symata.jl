using Base.Test

@ex ClearAll(a,b,c,p,f,d)
@testex ReplaceAll( f([a,b]) + f(c) , f([x_,y_]) => p(x+y)) == f(c) + p(a+b)
@testex Apply(List,ReplaceAll( f([a,b]) + f(c) , f([x_,y_]) => p(x+y))) == [f(c),p(a + b)]
@testex ReplaceAll([a/b, 1/b^2, 2/b^2] , b^n_ => d(n)) == [a*d(-1),d(-2),2*d(-2)]
@testex ReplaceAll( [a,b,[a,b]] , [x_,y_,[x_,y_]] => 1 ) == 1
@testex ReplaceAll( [b,a,[a,b]] , [x_,y_,[x_,y_]] => 1 ) == [b,a,[a,b]]

@testex MatchQ( 1, _Integer) == true
@testex MatchQ( 1.0, _Integer) == false
@testex MatchQ( "zebra", _String) == true
@testex MatchQ( 1.0, _String) == false
@ex Clear(a,b,gg,xx)
@ex a = 1
@testex MatchQ( a^2, x_^2) == false
@testex MatchQ( b^2, x_^2) == true
@testex MatchQ( b^2, _^2) == true
@testex MatchQ(f(b^2), f(x_^2)) == true
@testex MatchQ( gg(xx) , _gg)
@ex Clear(a)

@ex ClearAll(f,a,x)
@ex f(x_) := Module([a],(a=1,x+a))
@testex  f(3) == 4
@ex ClearAll(f,a,x)

using Base.Test

@ex ClearAll(a,b,c,p,f,d)
@testex ReplaceAll( f([a,b]) + f(c) , f([x_,y_]) => p(x+y)) == f(c) + p(a+b)
@testex Apply(List,ReplaceAll( f([a,b]) + f(c) , f([x_,y_]) => p(x+y))) == [f(c),p(a + b)]
@testex ReplaceAll([a/b, 1/b^2, 2/b^2] , b^n_ => d(n)) == [a*d(-1),d(-2),2*d(-2)]
@testex ReplaceAll( [a,b,[a,b]] , [x_,y_,[x_,y_]] => 1 ) == 1
@testex ReplaceAll( [b,a,[a,b]] , [x_,y_,[x_,y_]] => 1 ) == [b,a,[a,b]]

# This is broken. Tried for hours to fix it.
# Mma Does this successfully. We use a workaround in apprules.jl
@ex     ClearAll(a,b)
@ex     m = a ^ 3 + 3 * (a ^ 2) * b + 3 * a * (b ^ 2) + b ^ 3
@testex ReplaceRepeated(m, x_Integer => 1) == a + a*b + b
@ex     ClearAll(a,b)

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
@ex ClearAll(f,a,b,c,d,p,x,gg,xx,n,y)

@ex ClearAll(c,y)
@testex ReplaceAll(c, c => y) == y
@testex ReplaceAll(1, 1 => y) == y
@testex ReplaceAll(c(1), c => y) == y(1)
@ex ClearAll(c,y)

# We have to write rules in List(), because [x=>y] is parsed by Julia as deprecated Dict construction.
@ex ClearAll(r1,r2,zz,b,c)
@ex zz = 10 * b^2 * (c+d)
@testex ReplaceAll(zz, List(c => 3,d => 2) ) == 50*b^2
@ex ClearAll(r1,r2,zz,b,c)

@ex       ClearAll(pf,x,y,a,b)
@ex       pf(x_,y_) :=  x(y)
@testex   pf(a,b) == a(b)
@ex       ClearAll(pf,x,y,a,b)

@testex Count(Range(10), 2) == 1
@testex Count(Range(10), _Integer) == 10
@testex Count(Range(10), _String) == 0


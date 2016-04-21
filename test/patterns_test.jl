using Base.Test

SJulia.@ex ClearAll(a,b,c,d,p,f,d)
@testex ReplaceAll( f([a,b]) + f(c) , f([x_,y_]) => p(x+y)) == f(c) + p(a+b)
@testex Apply(List,ReplaceAll( f([a,b]) + f(c) , f([x_,y_]) => p(x+y))) == [f(c),p(a + b)]
SJulia.@testex ReplaceAll([a/b, 1/b^2, 2/b^2] , b^n_ => d(n)) == [a*d(-1),d(-2),2*d(-2)]
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
@testex MatchQ( "zebra", _AbstractString) == true
@testex MatchQ( 1.0, _AbstractString) == false
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

@ex       ClearAll(pf,x,y,a,b,f)
@ex       pf(x_,y_) :=  x(y)
@testex   pf(a,b) == a(b)
@ex       ClearAll(pf,x,y,a,b)

@testex Count(Range(10), 2) == 1
@testex Count(Range(10), _Integer) == 10
@testex Count(Range(10), _AbstractString) == 0

@testex ReplaceAll( [x,x^2,a,b],  x => 3 ) == [3,9,a,b]
@testex ReplaceAll( [x,x^2,x^3,a,b],  x^2  => y) == [x,y,x^3,a,b]
@testex ReplaceAll( [x,x^2,x^3,a,b],  x^n_  => f(n) ) == [x,f(2),f(3),a,b]

# write test for this. all return the same
@ex ClearAll(a,b,x,y,z)

@testex ReplaceAll( [x,x^2,y,z], x => [a,b]) == [[a,b],[a ^ 2,b ^ 2],y,z]
@testex ReplaceAll(Sin(x), Sin => Cos) == Cos(x)
@testex ReplaceAll( 1 + x^2 + x^4 , x^p_ => f(p)) == 1 + f(2) + f(4)

# FIXME.  1) give correct answer (it is 1).  2. handle error more gracefully
# sjulia > ReplaceAll(x , [x -> 1, x -> 3, x -> 7])
# ERROR: MethodError: no method matching Rule_to_PRule(::SJulia.Mxpr{RuleDelayed})
#  in eval(::Module, ::Any) at ./boot.jl:237


@testex Replace(x^2, x^2 => a + b) == a + b
@testex Replace(1 + x^2, x^2 => a + b)  == 1 + x ^ 2
@testex ReplaceAll( x + y , List(x => a, y => b)) == a + b

@ex result = ReplaceAll( [x,x,x,x,x],  x  => RandomReal() )
@testex result[1] == result[2] == result[3]

@ex result = ReplaceAll( [x,x,x,x,x],  x  -> RandomReal() )
@testex result[1] != result[2] != result[3]

@ex b = 1
@testex ReplaceAll( [x,x,x,x,x],  x -> Increment(b)) == [1,2,3,4,5]
@ex b = 1
@testex ReplaceAll( [x,x,x,x,x],  x => Increment(b)) == [1,1,1,1,1]

@ex b = 3
@ex r1 = a => b
@ex r2 = a -> b
@ex Clear(b)
@testex Replace( a, r1) == 3
@testex Replace( a, r2) == b
@ex b = 4
@testex Replace( a, r1) == 3
@testex Replace( a, r2) == 4

@testex ReplaceAll(x^2 + y^6 , List(x => 2 + a, a => 3))  == (2 + a) ^ 2 + y ^ 6

# FIXME. This returns unevaluated
# ReplaceRepeated(x^2 + y^6 , List(x => 2 + a, a => 3))

@ex ClearAll(result,r1,r2)

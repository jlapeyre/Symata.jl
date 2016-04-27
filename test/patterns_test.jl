using Base.Test

@testex Length(UserSyms()) == 0

SJulia.@ex ClearAll(a,b,c,d,p,f,d)
@testex ReplaceAll( f([a,b]) + f(c) , f([x_,y_]) => p(x+y)) == f(c) + p(a+b)
@testex Apply(List,ReplaceAll( f([a,b]) + f(c) , f([x_,y_]) => p(x+y))) == [f(c),p(a + b)]
SJulia.@testex ReplaceAll([a/b, 1/b^2, 2/b^2] , b^n_ => d(n)) == [a*d(-1),d(-2),2*d(-2)]
@testex ReplaceAll( [a,b,[a,b]] , [x_,y_,[x_,y_]] => 1 ) == 1
@testex ReplaceAll( [b,a,[a,b]] , [x_,y_,[x_,y_]] => 1 ) == [b,a,[a,b]]

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

@testex ReplaceAll(x , [x .> 1, x .> 3, x .> 7]) ==  1

# FIXME Looks like Mma does threading over the lists of rules in this case. Not documented ?
# It is documented, but not on the /. page. Just the example is shown
# x /. {{x -> 1}, {x -> 3}, {x -> 7}}

# FIXME ReplaceAll should do currying with the rules, not the expression.
# ReplaceAll(x => a)([x, x^2, y, z])

@testex ReplaceAll( [a,b,c] , List .> f)  == f(a,b,c)

@testex ReplaceAll( Hold(x + x) , x => 7) == Hold(7 + 7)

# .> works better than ->, which would take ast logic and rewriting.
# This reproduces Mma behavior
@testex ReplaceAll( Hold(x + x) , x => 2^2 ) == Hold(4 + 4)
@testex ReplaceAll( Hold(x + x) , (x .> 2^2) ) == Hold(2 ^ 2 + 2 ^ 2)

@testex ReplaceAll( Hold(x + x) , Rule(x, 2^2) ) == Hold(4 + 4)
@testex ReplaceAll( Hold(x + x) , RuleDelayed(x , 2^2) ) == Hold(2 ^ 2 + 2 ^ 2)

@testex Replace(x^2, x^2 => a + b) == a + b
@testex Replace(1 + x^2, x^2 => a + b)  == 1 + x ^ 2
@testex ReplaceAll( x + y , List(x => a, y => b)) == a + b

@ex result = ReplaceAll( [x,x,x,x,x],  x  => RandomReal() )
@testex result[1] == result[2] == result[3]

@ex result = ReplaceAll( [x,x,x,x,x],  x  .> RandomReal() )
@testex result[1] != result[2] != result[3]

@ex b = 1
@testex ReplaceAll( [x,x,x,x,x],  x .> Increment(b)) == [1,2,3,4,5]
@ex b = 1
@testex ReplaceAll( [x,x,x,x,x],  x => Increment(b)) == [1,1,1,1,1]

@testex ReplaceAll([x^2, x^3, x^4] , List(x^3 => u, x^n_ => p(n))) == [p(2),u,p(4)]

# fixed bug. we were matching sublevels before current level.
@testex ReplaceAll(h(x + h(y)) , h(u_) => u^2)  == (x + h(y))^2

@testex ReplaceAll([x^2, y^3]  , [x => y, y => x])  == [y ^ 2,x ^ 3]

# This works. But, we have to use parens. Not satisfactory
# We should not write many tests with infix symbols, etc. Because the choices change.
@testex x^2 ./ (x => (1+y)) ./ (y => b)  == (1 + b) ^ 2

@ex b = 3
@ex r1 = a => b
@ex r2 = a .> b
@ex Clear(b)
@testex Replace( a, r1) == 3
@testex Replace( a, r2) == b
@ex b = 4
@testex Replace( a, r1) == 3
@testex Replace( a, r2) == 4
@ex ClearAll(a,b)

@testex ReplaceAll(x^2 + y^6 , List(x => 2 + a, a => 3))  == (2 + a) ^ 2 + y ^ 6

@testex ReplaceRepeated(x^2 + y^6 , List(x => 2 + a, a => 3)) == 25 + y ^ 6

@testex ReplaceRepeated( Expand( (a+b)^3 ) , x_Integer => 1)  == a +  a * b + b

@ex      rules = [Log(x_ * y_) => Log(x) + Log(y), Log(x_^k_) => k * Log(x)]
@testex  ReplaceRepeated(Log(Sqrt(a*(b*c^d)^e)), rules) == 1/2 * (Log(a) + e * (Log(b) + d * Log(c)))
@testex  ReplaceAll(Log(Sqrt(a*(b*c^d)^e)), rules) == 1/2 * Log(a * ((b * (c ^ d)) ^ e))

# Why do we get this ? Looks like we perform the currying. Mma does not do it.
# sjulia > f(a)(b)(c)(d)
# f(a,b,c,d)

# tests a bug that caused julia-level error in the following line
@ex ex = Hold(f(a)(b)(c)(d))
@testex ReplaceAll(Hold(f(a)(b)), f(x_) => g) == Hold(g(b))

@ex ClearAll(result,r1,r2, a, b, d, c, e, f, m, n, p, x, y, z, rules, k, u, ex, g, h)

@ex If( Length(UserSyms()) > 0 ,  Println("**********", UserSyms()))
@testex Length(UserSyms()) == 0

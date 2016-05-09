
testUserSyms

T Head(_) == Blank
T Head(__) == BlankSequence
T Head(___) == BlankNullSequence
T _b == Blank(b)
T __b == BlankSequence(b)
T ___b == BlankNullSequence(b)

ClearAll(a,b,c,d,p,f,d)

T ReplaceAll( f([a,b]) + f(c) , f([x_,y_]) => p(x+y)) == f(c) + p(a+b)
T Apply(List,ReplaceAll( f([a,b]) + f(c) , f([x_,y_]) => p(x+y))) == [f(c),p(a + b)]
T ReplaceAll([a/b, 1/b^2, 2/b^2] , b^n_ => d(n)) == [a*d(-1),d(-2),2*d(-2)]
T ReplaceAll( [a,b,[a,b]] , [x_,y_,[x_,y_]] => 1 ) == 1
T ReplaceAll( [b,a,[a,b]] , [x_,y_,[x_,y_]] => 1 ) == [b,a,[a,b]]

T MatchQ( 1, _Integer) == true
T MatchQ( 1.0, _Integer) == false
T MatchQ( "zebra", _AbstractString) == true
T MatchQ( 1.0, _AbstractString) == false
 Clear(a,b,gg,xx)
 a = 1
T MatchQ( a^2, x_^2) == false
T MatchQ( b^2, x_^2) == true
T MatchQ( b^2, _^2) == true
T MatchQ(f(b^2), f(x_^2)) == true
T MatchQ( gg(xx) , _gg)
 Clear(a)

 ClearAll(f,a,x)
 f(x_) := Module([a],(a=1,x+a))
T  f(3) == 4
 ClearAll(f,a,b,c,d,p,x,gg,xx,n,y)

 ClearAll(c,y)
T ReplaceAll(c, c => y) == y
T ReplaceAll(1, 1 => y) == y
T ReplaceAll(c(1), c => y) == y(1)
 ClearAll(c,y)

# We have to write rules in List(), because [x=>y] is parsed by Julia as deprecated Dict construction.
 ClearAll(r1,r2,zz,b,c)
 zz = 10 * b^2 * (c+d)
T ReplaceAll(zz, List(c => 3,d => 2) ) == 50*b^2
 ClearAll(r1,r2,zz,b,c)

       ClearAll(pf,x,y,a,b,f)
       pf(x_,y_) :=  x(y)
T   pf(a,b) == a(b)
       ClearAll(pf,x,y,a,b)

T Count(Range(10), 2) == 1
T Count(Range(10), _Integer) == 10
T Count(Range(10), _AbstractString) == 0

T ReplaceAll( [x,x^2,a,b],  x => 3 ) == [3,9,a,b]
T ReplaceAll( [x,x^2,x^3,a,b],  x^2  => y) == [x,y,x^3,a,b]
T ReplaceAll( [x,x^2,x^3,a,b],  x^n_  => f(n) ) == [x,f(2),f(3),a,b]

# write test for this. all return the same
 ClearAll(a,b,x,y,z)

T ReplaceAll( [x,x^2,y,z], x => [a,b]) == [[a,b],[a ^ 2,b ^ 2],y,z]
T ReplaceAll(Sin(x), Sin => Cos) == Cos(x)
T ReplaceAll( 1 + x^2 + x^4 , x^p_ => f(p)) == 1 + f(2) + f(4)

T ReplaceAll(x , [x :> 1, x :> 3, x :> 7]) ==  1

# FIXME Looks like Mma does threading over the lists of rules in this case. Not documented ?
# It is documented, but not on the /. page. Just the example is shown
# x /. {{x -> 1}, {x -> 3}, {x -> 7}}

# FIXME ReplaceAll should do currying with the rules, not the expression.
# ReplaceAll(x => a)([x, x^2, y, z])

T ReplaceAll( [a,b,c] , List :> f)  == f(a,b,c)

T ReplaceAll( Hold(x + x) , x => 7) == Hold(7 + 7)

# :> works better than ->, which would take ast logic and rewriting.
# This reproduces Mma behavior
T ReplaceAll( Hold(x + x) , x => 2^2 ) == Hold(4 + 4)
T ReplaceAll( Hold(x + x) , (x :> 2^2) ) == Hold(2 ^ 2 + 2 ^ 2)

T ReplaceAll( Hold(x + x) , Rule(x, 2^2) ) == Hold(4 + 4)
T ReplaceAll( Hold(x + x) , RuleDelayed(x , 2^2) ) == Hold(2 ^ 2 + 2 ^ 2)

T Replace(x^2, x^2 => a + b) == a + b
T Replace(1 + x^2, x^2 => a + b)  == 1 + x ^ 2
T ReplaceAll( x + y , List(x => a, y => b)) == a + b

 result = ReplaceAll( [x,x,x,x,x],  x  => RandomReal() )
T result[1] == result[2] == result[3]

 result = ReplaceAll( [x,x,x,x,x],  x  :> RandomReal() )
T result[1] != result[2] != result[3]

 b = 1
T ReplaceAll( [x,x,x,x,x],  x :> Increment(b)) == [1,2,3,4,5]
 b = 1
T ReplaceAll( [x,x,x,x,x],  x => Increment(b)) == [1,1,1,1,1]

T ReplaceAll([x^2, x^3, x^4] , List(x^3 => u, x^n_ => p(n))) == [p(2),u,p(4)]

# fixed bug. we were matching sublevels before current level.
T ReplaceAll(h(x + h(y)) , h(u_) => u^2)  == (x + h(y))^2

T ReplaceAll([x^2, y^3]  , [x => y, y => x])  == [y ^ 2,x ^ 3]

# This works. But, we have to use parens. Not satisfactory
# We should not write many tests with infix symbols, etc. Because the choices change.
T x^2 ./ (x => (1+y)) ./ (y => b)  == (1 + b) ^ 2

 b = 3
 r1 = a => b
 r2 = a :> b
 Clear(b)
T Replace( a, r1) == 3
T Replace( a, r2) == b
 b = 4
T Replace( a, r1) == 3
T Replace( a, r2) == 4
 ClearAll(a,b)

T ReplaceAll(x^2 + y^6 , List(x => 2 + a, a => 3))  == (2 + a) ^ 2 + y ^ 6

T ReplaceRepeated(x^2 + y^6 , List(x => 2 + a, a => 3)) == 25 + y ^ 6

T ReplaceRepeated( Expand( (a+b)^3 ) , x_Integer => 1)  == a +  a * b + b

      rules = [Log(x_ * y_) => Log(x) + Log(y), Log(x_^k_) => k * Log(x)]
T  ReplaceRepeated(Log(Sqrt(a*(b*c^d)^e)), rules) == 1/2 * (Log(a) + e * (Log(b) + d * Log(c)))
T  ReplaceAll(Log(Sqrt(a*(b*c^d)^e)), rules) == 1/2 * Log(a * ((b * (c ^ d)) ^ e))

# Why do we get this ? Looks like we perform the currying. Mma does not do it.
# sjulia > f(a)(b)(c)(d)
# f(a,b,c,d)

# tests a bug that caused julia-level error in the following line
 ex = Hold(f(a)(b)(c)(d))
T ReplaceAll(Hold(f(a)(b)), f(x_) => g) == Hold(g(b))

#### Currying

   countprimes = Count(_:?(PrimeQ))
T  countprimes(Range(100)) == 25
   ClearAll(countprimes)

# Use a Julia function to list the perfect squares less than 100.
T  Cases(Range(100), _:?(:( (x) -> typeof(mpow(x,1//2)) <: Integer )) ) == [1,4,9,16,25,36,49,64,81,100]

# Alternatives does nothing yet
T Head( a | b ) == Alternatives

 ClearAll(result,r1,r2, a, b, d, c, e, f, m, n, p, x, y, z, rules, k, u, ex, g, h)

 testUserSyms


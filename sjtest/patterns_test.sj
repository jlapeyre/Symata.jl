
testUserSyms

# Only Blank is currently useful
T Head(_) == Blank
T Head(__) == BlankSequence
T Head(___) == BlankNullSequence
T _b == Blank(b)
T __b == BlankSequence(b)
T ___b == BlankNullSequence(b)

# Alternatives is implemented
T a | b == Alternatives(a,b)

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

# Alternatives
T MatchQ( 1, _Integer | _String)
T MatchQ( "dog", _Integer | _String)
T ReplaceAll( [a, b, c, d, a, b, b, b],  a | b => x) == [x,x,c,d,x,x,x,x]

f(x_, x_ | y_String) := [x,y]

# y is replaced by Sequence()
T f(2,2) == [2]

# Here, it is not
T f(2,"cat") == [2,"cat"]

# No match
T  Head(f(2,3)) == f

g(x_, x_ | h(y_String)) := [x,y]
T g(2,2) == [2]
T Head(g(2,"dog")) == g
T g(2,h("dog")) == [2,"dog"]

ClearAll(h,p,a,bc,d)
h(a | b) := p
T [h(a), h(b), h(c), h(d)] == [p,p,h(c),h(d)]

T ReplaceAll([1, x, x^2, x^3, y^2] , (x | x^_) => q) == [1,q,q,q,y^2]

# FIXME. This is not parsed correctly. We get Span
# (f : (a | b))(x_) => r(f, x)

ClearAll(a,b,c,f,g,h,p,x,y)

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
T Count(Range(10), b_Integer) == 10
T Count(Range(10), _AbstractString) == 0
# We translate 'String' to avoid Julia's warning
T Count(Range(10), _String) == 0
# For the moment, we don't consider Integer's to be Real. Real is translated to AbstractFloat
T Count([1,2,3.0] , _Real) == 1
T Count([1,2,3.0] , _Float) == 1

ClearAll(b)

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

# FIXME, we need to implement level specifications
# Replace([1, 7, "Hi", 3, Indeterminate], Except(_:?NumericQ) :> 0, 1)

T ReplaceAll(x^2 + y^6 , List(x => 2 + a, a => 3))  == (2 + a) ^ 2 + y ^ 6

T ReplaceRepeated(x^2 + y^6 , List(x => 2 + a, a => 3)) == 25 + y ^ 6

T ReplaceRepeated( Expand( (a+b)^3 ) , x_Integer => 1)  == a +  a * b + b

      rules = [Log(x_ * y_) => Log(x) + Log(y), Log(x_^k_) => k * Log(x)]
T  ReplaceRepeated(Log(Sqrt(a*(b*c^d)^e)), rules) == 1/2 * (Log(a) + e * (Log(b) + d * Log(c)))
T  ReplaceAll(Log(Sqrt(a*(b*c^d)^e)), rules) == 1/2 * Log(a * ((b * (c ^ d)) ^ e))

# Why do we get this ? Looks like we perform the currying immediately. Mma defers evaluation.
# sjulia > f(a)(b)(c)(d)
# f(a,b,c,d)

# tests a fix for bug that caused julia-level error in the following line
 ex = Hold(f(a)(b)(c)(d))
T ReplaceAll(Hold(f(a)(b)), f(x_) => g) == Hold(g(b))

T ReplaceAll( [[[x]]], x => 3) == [[[3]]]
ClearAll(x)

#### Currying

   countprimes = Count(_:?(PrimeQ))
T  countprimes(Range(100)) == 25
   ClearAll(countprimes)

# Use a Julia function to list the perfect squares less than 100.
T  Cases(Range(100), _:?(:( (x) -> typeof(mpow(x,1//2)) <: Integer )) ) == [1,4,9,16,25,36,49,64,81,100]

T Cases([1,2.0,3,"dog"], _String) == ["dog"]
T DeleteCases([1,2.0,3,"dog"], _String) == [1,2.0,3]

# Currying, or operator form.
dstring = DeleteCases(_String)
T dstring([1,2.0,3,"dog"]) == [1,2.0,3]

T Cases([f(1),2.0,3,f("dog")], f(x_)) == [f(1),f("dog")]
T Cases([f(1),2.0,3,f("dog")], f(x_String)) == [f("dog")]

#### Except

T MatchQ( "cat",  Except("dog"))
T MatchQ( "cat",  Except("cat")) == False
T MatchQ( "cat",  Except(3,"cat"))
T MatchQ( "cat",  Except(3,"dog")) == False
T MatchQ( "cat",  Except("cat","dog")) == False

T Cases([a, b, 0, 1, 2, x, y], Except(_Integer)) == [a,b,x,y]
T Cases([1, 0, 2, 0, 3], Except(0)) == [1,2,3]
T Cases([a, b, 0, 1, 2, x, y], Except(0,_Integer)) == [1,2]

# This works, but only with parens around IntegerQ, otherwise it is interpreted as
# conditional. Syntax needs work.
T MatchQ(1, Except(_:?(IntegerQ))) == False
T MatchQ(a, Except(_:?(IntegerQ)))

ClearAll(dstring,result,r1,r2, a, b, d, c, e, f, m, n, p, x, y, z, rules, k, u, ex, g, h)

# Optional is parsed, but is not yet implemented (used) anywhere.
T x_y:0 == Optional(Pattern(x,Blank(y)),0)
T x_y:(a*b) == Optional(Pattern(x,Blank(y)),Times(a,b))

ClearAll(a,b,x, y)

T p_ == Pattern(p,Blank())
T p_q  == Pattern(p,Blank(q))
T p_Integer:?(PrimeQ) == PatternTest(Pattern(p,Blank(Integer)),PrimeQ)
T p_:?(PrimeQ) == PatternTest(Pattern(p,Blank()),PrimeQ)
T _^_ == Power(Blank(),Blank())

# This should evaluate to True, but does not.
# Mma has PatternTest evaluation parts of held expressions. So 2 + 3 should be evaluated,
# at least to match their behavior. No explanation is given for why this is so.
#T MatchQ(Hold(2 + 3), Hold(_:?IntegerQ))

# Try using :: for Pattern
# Pattern is not implemented with this usage. We only test parsing here.
# It will raise an error if you try to use it.
# NB We use both :: and :, while Mma uses : in all instances below.
# It would be nice if we could use one symbol
# We use : for Span, as well. Mma uses :: for Message and ;; for Span.
T a::b == Pattern(a,b)
T a::(b_^c_) == Pattern(a,Power(Pattern(b,Blank()),Pattern(c,Blank())))
T _:0 == Optional(Blank(),0)

ClearAll(p,q,a,b,c)

#### Condition

# Condition is implemented for the case that the first arguement is a pattern.
# It is also used with SetDelayed and RuleDelayed, but these are not yet implemented.
T  MatchQ( -2 , Condition( x_ , x < 0))
T  MatchQ( 1 , Condition( x_ , x < 0)) == False
T  ReplaceAll([6, -7, 3, 2,-1,-2], Condition( x_ , x < 0) => w ) == [6,w,3,2,w,w]
T  Cases( [[a, b], [1, 2, 3], [[d, 6], [d, 10]]], Condition([x_, y_], ! ListQ(x) && ! ListQ(y))) == [[a,b]]

ClearAll(x,y,a,b,d)



 testUserSyms


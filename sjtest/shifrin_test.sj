# These tests are taken from "Mathematica Programming: An Advanced Introduction"
# by Leonid Shifrin.

T Map(AtomQ,[x,Sin(x),1+2*I,2/3]) == [True, False, True, True]

ClearAll(f,g,h,x)
# no longer differs from Mma.
#T f(g)(h)(x) == f(g,h,x)

# 1. Mma has Int64 -> Integer, Float64 -> Real, and only Complex and Rationa.
# 2. Complex{Int64} is not valid Symata.
# Map(Head, [f,2,Pi,3.14,"abc",2/3,1+I]) == [Symbol,Int64,Symbol,Float64,String,Rational{Int64},Complex{Int64}]

T [Plus(1,2,3,4), Times(1,2,3,4)] == [10,24]

T ReplaceAll( [a,c,d,c], a => b ) == [b,c,d,c]

f(x_) := x^2

T [f(2), f("word"), f(Newton)] == [4,"word"^2, Newton^2]

T DownValues(f) == [HoldPattern(f(x_)) :> (x^2)]

ClearAll(f)

f(x_Integer) := x^2

T Map(Head, [f(2), f("word"), f(Pi), f(Newton)]) == [If(BigIntInput(), BigInt, Int64), f, f, f]

Apply(ClearAll, UserSyms())

# pg 101
T ReplaceAll( [x, Sin(x), x^2, x*y, x+y, g(x,y), h(x,y,z), Cos(y)], f_(x) => f(10)) == [x,Sin(10),x^2,x*y,x + y,g(x,y),h(x,y,z),Cos(y)]

T ReplaceAll( [x, Sin(x), x^2, x*y, x+y, g(x,y), h(x,y,z), Cos(y)], f_(x,z_) => f(10,z)) == [x,Sin(x),100,10y,10 + y,g(10,y),h(x,y,z),Cos(y)]

# pg 102
T ReplaceAll( [x, Sin(x), x^2, x*y, x+y, g(y,x), Cos(y)], f_(x,z_) => [ ["f now ", f], ["z now ", z]] ) == [x,Sin(x),[["f now ",Power],["z now ",2]],[["f now ",Times],["z now ",y]],[["f now ",Plus],["z now ",y]],g(y,x),Cos(y)]

T ReplaceAll( [x, Sin(x), x^2, x*y, x+y, g(y,x), h(x,y,z), Cos(y)], [ f_(x,z_) => f(10,z), f_(z_,x) => f(z,10)] ) == [x,Sin(x),100,10y,10 + y,g(y,10),h(x,y,z),Cos(y)]

T ReplaceAll( [x, Sin(x), x^2, x*y, x+y, g(y,x), h(x,y,z), Cos(y)], [f_(x) => f(10), f_(x,z_) => f(10,z), f_(z_,x) => f(z,10)] ) == [x,Sin(10),100,10y,10 + y,g(y,10),h(x,y,z),Cos(y)]

T ReplaceAll( [x, Sin(x), x^2, x*y, x+y, g(y,x), h(x,y,z), Cos(y)], [f_(x) => f(10), f_(x,z_) => f(10,z), f_(z_,x) => f(z,10), f_(x,y_,z_) => f(10,y,z)] ) == [x,Sin(10),100,10y,10 + y,g(y,10),h(10,y,z),Cos(y)]

# pg 103
T ReplaceAll([g(x,y,z), g(p,q), h(x,y)], g(t__) => [t]) == [[x,y,z],[p,q],h(x,y)]

T ReplaceAll([g(x,y,z), h(x,y)], f_(t__) => f(t,a)) == [g(x,y,z),h(x,y),a]

T ReplaceAll([g(x,y,z), h(x,y)], Condition( f_(t__), UnsameQ(f , List )) => f(t,a)) == [g(x,y,z,a),h(x,y,a)]

T Replace([g(x,y,z), h(x,y)], f_(t__)  => f(t,a), 1) == [g(x,y,z,a),h(x,y,a)]

# pg 104

p = Condition(f_(t__), UnsameQ(f, List)) :> (ReplaceAll( f(t), x => 10))
T ReplaceAll( [x, Sin(x), x^2, x*y, x+y, g(x,y), h(x,y,z), Cos(y)], p) == [x,Sin(10),100,10y,10 + y,g(10,y),h(10,y,z),Cos(y)]

p1 = Condition(f_(t__), UnsameQ(f, List)) => (ReplaceAll( f(t), x => 10))
T ReplaceAll( [x, Sin(x), x^2, x*y, x+y, g(x,y), h(x,y,z), Cos(y)], p1) == [x,Sin(x),x^2,x*y,x + y,g(x,y),h(x,y,z),Cos(y)]

# pg 107
T ReplaceAll( [x, Sin(x), x^2, x*y, x+y, g(y,x), h(x,y,z), Cos(y)], f_Sin :> (ReplaceAll( f, x => 10))) == [x,Sin(10),x^2,x*y,x + y,g(y,x),h(x,y,z),Cos(y)]

T ReplaceAll( [x, Sin(x), x^2, x*y, x+y, g(y,x), h(x,y,z), Cos(y)], f_Plus :> (ReplaceAll( f, x => 10))) == [x,Sin(x),x^2,x*y,10 + y,g(y,x),h(x,y,z),Cos(y)]

T ReplaceAll( [x, Sin(x), x^2, x*y, x+y, g(y,x), h(x,y,z), Cos(y)], f_Power :> (ReplaceAll( f, x => 10))) == [x,Sin(x),100,x*y,x + y,g(y,x),h(x,y,z),Cos(y)]

T ReplaceAll( [x, Sin(x), x^2, x*y, x+y, g(y,x), h(x,y,z), Cos(y)], Sin(x) => Sin(10)) == [x,Sin(10),x^2,x*y,x + y,g(y,x),h(x,y,z),Cos(y)]

# pg 108
# FIXME. raises error
# T ReplaceAll( [x, Sin(x), x^2, x*y, x+y, g(y,x), h(x,y,z), Cos(y)], Plus(t__) :> (ReplaceAll( Plus(t), x => 10)))

# This does work. The example above is meant to show a failed solution
T  ReplaceAll( [x, Sin(x), x^2, x*y, x+y, g(y,x), h(x,y,z), Cos(y)], HoldPattern(Plus(t__)) :> (ReplaceAll( Plus(t), x => 10))) == [x,Sin(x),x^2,x*y,10 + y,g(y,x),h(x,y,z),Cos(y)]

# pg 109
T ReplaceAll( [f(x), g(x), f(x,y), Sin(x+y), f(), f(x,y,z) ] , f(t__) :> a * f(t)) == [a*f(x),g(x),a*(f(x,y)),Sin(x + y),f(),a*(f(x,y,z))]
T ReplaceAll( [f(x), g(x), f(x,y), Sin(x+y), f(), f(x,y,z) ] , x_f :> a * x) == [a*f(x),g(x),a*(f(x,y)),Sin(x + y),a*f(),a*(f(x,y,z))]

# FIXME: BlankNullSequence not working completely
# T ReplaceAll( [f(x), g(x), f(x,y), Sin(x+y), f(), f(x,y,z) ] , f(t___) :> a * f(t)) == [a*f(x),g(x),a*(f(x,y)),Sin(x + y),a*f(),a*(f(x,y,z))]

# pg 110
testexpr = Expand((1+x)^10)
T testexpr ./ (Plus => List) ==  [1,10x,45(x^2),120(x^3),210(x^4),252(x^5),210(x^6),120(x^7),45(x^8),10(x^9),x^10]

T testexpr ./ (Plus => List) ./ ( x^(_:?(EvenQ)) => a) == [1,10x,45a,120(x^3),210a,252(x^5),210a,120(x^7),45a,10(x^9),a]

T testexpr ./ (Plus => List) ./ ( x^(y_:?(EvenQ)) :> f(x^y)) == [1,10x,45f(x^2),120(x^3),210f(x^4),252(x^5),210f(x^6),120(x^7),45f(x^8),10(x^9),f(x^10)]

# There is some notation on 110 that I don't yet know


# pg 112
# NOTE: we can use  => for the first definition, but Julia v0.4 cannot parse this.
frules = [ fact(1) :> 1, fact(n_Integer) :> n * fact(n-1)]
T fact(5) .//  frules == 120

# pg 115
T [a, "cat", 3] ./ (x_String :> StringReverse(x)) == [a,"tac",3]

# pg 119
T Range(30) ./ ( Condition( x_, IntegerQ(Sqrt(x))) :> [Sqrt(x)] ) == [[1],2,3,[2],5,6,7,8,[3],10,11,12,13,14,15,[4],17,18,19,20,21,22,23,24,[5],26,27,28,29,30]

# pg 120
T Range(100) ./ (Condition( x_Integer, Not(PrimeQ(x))) :> Sequence() ) == [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

testlist = [[1,14],[2,6],[10,20],[19,14],[6,3],[17,8],[11,13],[19,18],[1,11],[5,14],[19,16],[16,16],[10,10],[16,10],[16,7],[7,19],[17,11],[11,13],[20,12],[6,12]]
exchangerule = Condition( [x_, y_ ] , EvenQ(x) && OddQ(y)) :>  [y,x]

T testlist ./ exchangerule == [[1,14],[2,6],[10,20],[19,14],[3,6],[17,8],[11,13],[19,18],[1,11],[5,14],[19,16],[16,16],[10,10],[16,10],[7,16],[7,19],[17,11],[11,13],[20,12],[6,12]]


# pg 121
T [x, 2, Pi , 3/2, 2/5, 4, Sin(y), 8, Cos(z)] ./ ( x_Integer | x_Rational  => Sqrt(x)) == [x,2^(1/2),Pi,(2^(-1/2))*(3^(1/2)),(2^(1/2))*(5^(-1/2)),2,Sin(y),2(2^(1/2)),Cos(z)]

T [x, 2, Pi , 3/2, 2/5, 4, Sin(y), 8, Cos(z)] ./ ( Condition(x_, Isa(x,Integer) || Isa(x,Rational)) => Sqrt(x)) == [x, 2, Pi , 3/2, 2/5, 4, Sin(y), 8, Cos(z)] ./ ( x_Integer | x_Rational  => Sqrt(x))

Apply(ClearAll, UserSyms())

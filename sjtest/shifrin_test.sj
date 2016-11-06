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
frules = [ fact(1) => 1, fact(n_Integer) :> n * fact(n-1)]
T fact(5) .//  frules == 120

#pg 115
T [a, "cat", 3] ./ (x_String :> StringReverse(x)) == [a,"tac",3]

Apply(ClearAll, UserSyms())

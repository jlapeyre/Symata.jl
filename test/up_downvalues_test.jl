using Base.Test

@testex testUserSyms

@ex ClearAll(fib)
@ex fib(1) := 1
@ex fib(2) := 1
@ex fib(n_) := fib(n-1) + fib(n-2)
@testex fib(10) == 55
@ex Clear(fib)   # does not remove definition
@testex fib(11) == fib(10) + fib(9)
@ex ClearAll(fib)  # removes definition
@testex Head(fib(11)) == fib  # does not evaluate to number

# Test sorting of downvalues
# Enter downvalues in wrong order.
@ex fib(n_) := fib(n-1) + fib(n-2)
@ex fib(1) := 1
@ex fib(2) := 1
@testex fib(10) == 55
@ex ClearAll(fib)

## Condition on pattern

# Careful, I am mixing '=' and ':=' here
@ex g(1) := "cat"
@ex g(x_Integer) = "int"
@ex g(x_Float64) = "float"

@testex  g(1) == "cat"
@testex  g(2) == "int"
@testex  g(2.0) == "float"

@ex ClearAll(g,h,a,b)
@ex h(x_^2) := x
@testex  h((a+b)^2) == a + b
@ex ClearAll(h)

@ex h(x_,x_) := 1
@ex h(x_,y_) := 2
@testex h(a,a) == 1
@testex h(a,b) == 2
@ex ClearAll(h)

## Restrictions on patterns. Match head and/or "pattern test"

@ex      ClearAll(stringgt4,g,gt5,h)
@ex      stringgt4(x_) := StringLength(x) > 4
@ex      gt5(x_) := x > 5
@ex      g(x_Integer:?(EvenQ)) := x
@ex      g(x_AbstractString:?(stringgt4)) = "Greater than 4"
@ex      g(x_AbstractFloat:?(gt5)) = 1
@testex  Head(g(3)) == g
@testex  g(4) == 4
@testex  Head(g(5)) == g
@testex  Head(g("cat")) == g
@testex  g("zebra") == "Greater than 4"
@testex         Head(g(4.0)) == g
@testex         g(6.0) == 1
@ex             h(x_AbstractFloat:?(:((y)-> y < 3) )) = 1
@testex         Head(h(2)) == h
@testex         Head(h(4)) == h
@testex         Head(h(2)) == h
@testex         Head(h(4.0)) == h
@testex         h(2.0) == 1
@ex      ClearAll(a,b,stringgt4,g,gt5)

#### UpValues

@ex     rate(m1) ^= 1/2
@ex     rate(m2) ^= 3/4
@testex rate(m1)/rate(m2) == 2/3

# All subtypes of Integer match. All subtypes of FloatingPoint match.
@ex ClearAll(a,p)
@ex a^3 ^= p
@testex a^3 == p
@testex Apply(List,a^2) == [a,2]
@testex a^BI(3) == p
@ex ClearAll(a,p)

@ex ClearAll(a,p)
@ex a^4.0 ^= p
# Fix this, (Fix what ?)
@testex (a^4 == p) != True
@testex a^BF(4) == p
@testex a^4.0 == p
@ex ClearAll(a,p)

@ex z(a(y_)) ^= y^2
@testex  z(a(3)) == 9

@ex y = 100
@ex h([x_,y_]) := x^y
@ex y = 101
@testex h([3,4]) == 81
@testex y == 101

#### UpSetDelayed

@ex     UpSetDelayed( f(g(x_)) , fg(x) )
@testex [f(g(2)), f(h(2))] == [fg(2),f(h(2))]

# FIXME.  Implement logical and operator  &&, And
@ex      UpSetDelayed( rand(int) , Random(Integer))
@ex      tabsum = Apply(Plus,Table(rand(int), [100]))
@testex  tabsum != 0
@testex  tabsum != 100

@ex     f(h(0)) ^= h0
@ex     UpSetDelayed( f(h(x_)) , 2 * f(h(x - 1)))
@testex f(h(10)) == 1024 * h0

# FIXME. We cannot yet handle more than one arg on the lhs
# @ UpSetDelayed( area(sq, s_) , s^2)

@ex      UpSetDelayed( area(sq(s_)), s^2)
@testex  area(sq(3)) == 9

@ex   ClearAll(g,h)
@ex      UpSetDelayed( meth(g(x_), h(y_)) ,  fgh(x, y) )
@testex  UpValues(h) == UpValues(g)  == [HoldPattern(meth(g(x_),h(y_))) .> (fgh(x,y))]

@ex       ClearAll(f,h)
@ex       UpSetDelayed( f(h(x_)) ,  f1(x))
@testex   UpValues(h) == [HoldPattern(f(h(x_))) .> f1(x)]
@ex        UpSetDelayed( f(h(x_)) ,  f2(x))
@testex   UpValues(h) == [HoldPattern(f(h(x_))) .> f2(x)]
@ex ClearAll(f1,f2,g,h)

# FIXME Need to implement tags, which should be compared to upsetdelayed here.
@ex      UpSetDelayed( f2( g(x_), h(y_) ) ,  gh(x * y))
@testex  UpValues(g) == [HoldPattern(f2(g(x_),h(y_))) .> gh(x * y)]
@testex  UpValues(h) == [HoldPattern(f2(g(x_),h(y_))) .> gh(x * y)]
@testex  f2(g(3),h(4)) == gh(12)
@ex ClearAll(f2,g,h,gh)

# FIXME:  WARNING: Symbol 'Pattern' is protected
#  UpSetDelayed( a_mod + b_mod ,  modPlus(a, b))


@ex ClearAll(f,g,fg,h,a,z,y,x,rate,m1,m2, rand, int, tabsum, h0, s, area, sq, fgh, meth)

@testex testUserSyms

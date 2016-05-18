
T testUserSyms

 ClearAll(fib)
 fib(1) := 1
 fib(2) := 1
 fib(n_) := fib(n-1) + fib(n-2)
T fib(10) == 55
 Clear(fib)   # does not remove definition
T fib(11) == fib(10) + fib(9)
 ClearAll(fib)  # removes definition
T Head(fib(11)) == fib  # does not evaluate to number

# Test sorting of downvalues
# Enter downvalues in wrong order.
 fib(n_) := fib(n-1) + fib(n-2)
 fib(1) := 1
 fib(2) := 1
T fib(10) == 55
 ClearAll(fib)

## Condition on pattern

# Careful, I am mixing '=' and ':=' here
 g(1) := "cat"
 g(x_Integer) = "int"
 g(x_Float64) = "float"

T  g(1) == "cat"
T  g(2) == "int"
T  g(2.0) == "float"

 ClearAll(g,h,a,b)
 h(x_^2) := x
T  h((a+b)^2) == a + b
 ClearAll(h)

 h(x_,x_) := x
 h(x_,y_) := 2
T h(a,a) == a
T h(a,b) == 2
 ClearAll(h,a,b)

## Restrictions on patterns. Match head and/or "pattern test"

      ClearAll(stringgt4,g,gt5,h)
      stringgt4(x_) := StringLength(x) > 4
      gt5(x_) := x > 5
      g(x_Integer:?(EvenQ)) := x
      g(x_AbstractString:?(stringgt4)) = "Greater than 4"
      g(x_AbstractFloat:?(gt5)) = 1
T  Head(g(3)) == g
T  g(4) == 4
T  Head(g(5)) == g
T  Head(g("cat")) == g
T  g("zebra") == "Greater than 4"
T         Head(g(4.0)) == g
T         g(6.0) == 1
             h(x_AbstractFloat:?(:((y)-> y < 3) )) = 1
T         Head(h(2)) == h
T         Head(h(4)) == h
T         Head(h(2)) == h
T         Head(h(4.0)) == h
T         h(2.0) == 1
      ClearAll(a,b,stringgt4,g,gt5)

#### UpValues

 rate(m1) ^= 1/2
 rate(m2) ^= 3/4
T rate(m1)/rate(m2) == 2/3

# All subtypes of Integer match. All subtypes of FloatingPoint match.
 ClearAll(a,p)
 a^3 ^= p
T a^3 == p
T Apply(List,a^2) == [a,2]
T a^BI(3) == p
 ClearAll(a,p)

 ClearAll(a,p)
 a^4.0 ^= p
# Fix this, (Fix what ?)
T (a^4 == p) != True
T a^BF(4) == p
T a^4.0 == p
 ClearAll(a,p)

 z(a(y_)) ^= y^2
T  z(a(3)) == 9

 y = 100
 h([x_,y_]) := x^y
 y = 101
T h([3,4]) == 81
T y == 101

#### UpSetDelayed

  UpSetDelayed( f(g(x_)) , fg(x) )
T [f(g(2)), f(h(2))] == [fg(2),f(h(2))]

UpSetDelayed( rand(int) , Random(Integer))
tabsum = Apply(Plus,Table(rand(int), [100]))
T  tabsum != 0
T  tabsum != 100

 f(h(0)) ^= h0
 UpSetDelayed( f(h(x_)) , 2 * f(h(x - 1)))
T f(h(10)) == 1024 * h0

# FIXME. We cannot yet handle more than one arg on the lhs
# @ UpSetDelayed( area(sq, s_) , s^2)

      UpSetDelayed( area(sq(s_)), s^2)
T  area(sq(3)) == 9

ClearAll(g,h)

# The rule is associated with all symbols occuring on level one in lhs
UpSetDelayed( meth(g(x_), h(y_)) ,  fgh(x, y) )
T  UpValues(h) == UpValues(g)  == [HoldPattern(meth(g(x_),h(y_))) :> (fgh(x,y))]

       ClearAll(f,h)
       UpSetDelayed( f(h(x_)) ,  f1(x))
T   UpValues(h) == [HoldPattern(f(h(x_))) :> f1(x)]
        UpSetDelayed( f(h(x_)) ,  f2(x))
T   UpValues(h) == [HoldPattern(f(h(x_))) :> f2(x)]
 ClearAll(f1,f2,g,h)

# FIXME Need to implement tags, which should be compared to upsetdelayed here.
      UpSetDelayed( f2( g(x_), h(y_) ) ,  gh(x * y))
T  UpValues(g) == [HoldPattern(f2(g(x_),h(y_))) :> gh(x * y)]
T  UpValues(h) == [HoldPattern(f2(g(x_),h(y_))) :> gh(x * y)]
T  f2(g(3),h(4)) == gh(12)
 ClearAll(f2,g,h,gh)

# FIXME:  WARNING: Symbol 'Pattern' is protected
#  UpSetDelayed( a_mod + b_mod ,  modPlus(a, b))

# Alternatives in DownValue rules
h(x_Integer | x_Float) := x^2

T h(3) == 9
T h(3.0) == 9
T Head(h(1/2)) == h

ClearAll(f,g,fg,h,a,z,y,x,rate,m1,m2, rand, int, tabsum, h0, s, area, sq, fgh, meth)

# Use Set, rather than SetDelayed.
f(x_) = x^2
T f(3) == 9   # 9, rather than 3^2

ClearAll(f)

T testUserSyms

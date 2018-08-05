##############################
## WARNING!! Do not run these tests
## Changes to comparison code broken many of them
##############################

  ClearAll(a,b,c,d,x)

T 1 == 1
T !(1 != 1)
T !(1 > 1)
T !(1 < 1)
T 1 >= 1
T 1 <= 1
T 1 == 1.0
T 1 < 2
T 2 > 1
T !( 1 > 2)
T !( 2 < 1)
T !( 2 == 1)
T 2 != 1
T 2 >= 1
T 1 <= 2
T !( 1 >= 2 )
T !( 2 <= 1 )
T 1 < 2 < 3
T 3 > 2 > 1
T !( 3 < 2 < 1)

T (1 < 2 < b) == (2 < b)


T ((1 < 2 < b < c == c < 4 < 10)) == ((2 < b) && (b < c) && (c < 4))

T ( (a < c/d) != True )

ClearAll(b,c)
#  1 < x < 1  --> 1 < x && x < 1).   FIXME. this should return false.

# This, at least, works.
T Apply(List, 1 < 2 < b < c == c < 4 < 10) == [2 < b,b < c,c < 4]

# Agrees with Mma
T (1 < x >  1) == ( 1 < x && (x > 1) )

T Apply(List, b>2) == [b,>,2]
T Apply(List, 2>b) == [2,>,b]

T Apply(List, a < b) == List(a, < , b)
T Apply(List, a <= b) == List(a, <= , b)
T Apply(List, a >= b) == List(a, >= , b)
T Apply(List, a < 1) == List(a, < , 1)
T Apply(List, a == 1) == List(a, == , 1)
T Apply(List, f(a) < 1) == List(f(a), < , 1)
T Apply(List, f(a) < 1.0) == List(f(a), < , 1.0)
T (a == a ) == True
T (a != a) == False
T (a <= a) == True
T (a >= a) == True
T (a < 1) == (a < 1)

T Sqrt(2) > 0
T -Sqrt(2) < 0
T 3 < Pi < 4
T 2 < E < 3
T 0 < EulerGamma < 1

### Infinity

T 1 != Infinity
T Not(1 == Infinity)
T 1.0 != Infinity
T Not(1.0 == Infinity)

### Not

T Not(a != a) == True
T Not(a == a) == False
T Not(a < b) == (a >= b)
T Head(Not(3)) == Not

### Or

T Or() == False
T Or(1) == 1
T Or(True) == True
T Or(True,False) == True
T Or(False,False) == False
T Or(True,True) == True
T Or(False,True) == True
T Or(True,False,False) == True

# Test flatten with Or
T Args(a || b || c || d ) == [a,b,c,d]

### And

T And() == True
T And(1) == 1
T And(True) == True
T And(False,True) == False
T And(True,False) == False
T And(True,True) == True
T And(False,False) == False
T And(True,True,True) == True

# Test flatten with And
T Args(a && b && c && d ) == [a,b,c,d]
  ClearAll(a,b,c,d,f,x)

### Refine

# This belongs elsewhere
  ClearAll(aaa)
  Assume(aaa,Positive)
T Refine(Abs(aaa)) == aaa
  ClearAll(aaa,a)

### String

T Not(f(x) == "cat")

### SameQ

T SameQ(a,a)
T Not(SameQ(a,b))
T Not(SameQ(3,3.0))
T SameQ(3,3)
T SameQ("cat", "cat")

### Equal

T Equal() == True
T Equal(3,3)
T Equal(3,3.0)
T Head(Equal(x,y)) == Equal
T Equal(x,x)
T Equal(2,bf"2")
T Equal(2,big"2")
T Equal(2.0,bf"2")
T Equal(2.0,big"2")

T Not(Unequal(3,3))
T Not(Unequal(3,3.0))
T Not(Unequal(y,y))
T Head(Unequal(x,y)) == Unequal

ClearAll(f,x)
Apply(ClearAll,UserSyms())

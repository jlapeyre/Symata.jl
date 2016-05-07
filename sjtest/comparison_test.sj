 ClearAll(a,b,c,d,x)

T testUserSyms

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
T  2 != 1
T  2 >= 1
T  1 <= 2
T  !( 1 >= 2 )
T  !( 2 <= 1 )
T  1 < 2 < 3
T  3 > 2 > 1
T  !( 3 < 2 < 1)

T  (1 < 2 < b) == (2 < b)

# T  (1 < 2 < b < c == c < 4 < 10) == (2 < b && (b < c) && (c < 4))  # FIXME. This should be true. it returns unevaluated

#  1 < x < 1  --> 1 < x && x < 1).   FIXME. this should return false.

# This, at least, works.
T  Apply(List, 1 < 2 < b < c == c < 4 < 10) == [2 < b,b < c,c < 4]

# Agrees with Mma
T  (1 < x >  1) == ( 1 < x && (x > 1) )

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

#### Infinity

T  1 != Infinity
T  Not(1 == Infinity)
T  1.0 != Infinity
T  Not(1.0 == Infinity)

#### Not

T Not(a != a) == True
T Not(a == a) == False
T Not(a < b) == (a >= b)
T Head(Not(3)) == Not

#### Or

T Or() == False
T Or(1) == 1
T Or(True) == True
T Or(True,False) == True
T Or(False,False) == False
T Or(True,True) == True
T Or(False,True) == True
T Or(True,False,False) == True

#### And

T And() == True
T And(1) == 1
T And(True) == True
T And(False,True) == False
T And(True,False) == False
T And(True,True) == True
T And(False,False) == False
T And(True,True,True) == True

 ClearAll(a,b,c,f,x)

T testUserSyms



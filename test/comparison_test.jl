@ex ClearAll(a,b,c,d,x)

@testex testUserSyms

@testex 1 == 1
@testex !(1 != 1)
@testex !(1 > 1)
@testex !(1 < 1)
@testex 1 >= 1
@testex 1 <= 1
@testex 1 == 1.0
@testex 1 < 2
@testex 2 > 1
@testex !( 1 > 2)
@testex !( 2 < 1)
@testex !( 2 == 1)
@testex  2 != 1
@testex  2 >= 1
@testex  1 <= 2
@testex  !( 1 >= 2 )
@testex  !( 2 <= 1 )
@testex  1 < 2 < 3
@testex  3 > 2 > 1
@testex  !( 3 < 2 < 1)

@testex  (1 < 2 < b) == (2 < b)

# @testex  (1 < 2 < b < c == c < 4 < 10) == (2 < b && (b < c) && (c < 4))  # FIXME. This should be true. it returns unevaluated

#  1 < x < 1  --> 1 < x && x < 1).   FIXME. this should return false.

# This, at least, works.
@testex  Apply(List, 1 < 2 < b < c == c < 4 < 10) == [2 < b,b < c,c < 4]

# Agrees with Mma
@testex  (1 < x >  1) == ( 1 < x && (x > 1) )

@testex Apply(List, b>2) == [b,>,2]
@testex Apply(List, 2>b) == [2,>,b]

@testex Apply(List, a < b) == List(a, < , b)
@testex Apply(List, a <= b) == List(a, <= , b)
@testex Apply(List, a >= b) == List(a, >= , b)
@testex Apply(List, a < 1) == List(a, < , 1)
@testex Apply(List, a == 1) == List(a, == , 1)
@testex Apply(List, f(a) < 1) == List(f(a), < , 1)
@testex Apply(List, f(a) < 1.0) == List(f(a), < , 1.0)
@testex (a == a ) == True
@testex (a != a) == False
@testex (a <= a) == True
@testex (a >= a) == True
@testex (a < 1) == (a < 1)

#### Not

@testex Not(a != a) == True
@testex Not(a == a) == False
@testex Not(a < b) == (a >= b)
@testex Head(Not(3)) == Not

#### Or

@testex Or() == False
@testex Or(1) == 1
@testex Or(True) == True
@testex Or(True,False) == True
@testex Or(False,False) == False
@testex Or(True,True) == True
@testex Or(False,True) == True
@testex Or(True,False,False) == True

#### And

@testex And() == True
@testex And(1) == 1
@testex And(True) == True
@testex And(False,True) == False
@testex And(True,False) == False
@testex And(True,True) == True
@testex And(False,False) == False
@testex And(True,True,True) == True

@ex ClearAll(a,b,c,f,x)

@testex testUserSyms



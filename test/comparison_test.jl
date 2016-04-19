@ex ClearAll(a,b,c,d,x)

# TODO. Comparisons need more careful testing
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
@testex Not(a != a) == True
@testex Not(a == a) == False
@testex Not(a < b) == (a >= b)
@testex Head(Not(3)) == Not
@testex (a < 1) == (a < 1)




@ex ClearAll(a,b,c,d,x)

# TODO. Comparisons need more careful testing
@testex Apply(List, a < b) == List(a, < , b)
@testex Apply(List, a < 1) == List(a, < , 1)
@testex Apply(List, a == 1) == List(a, == , 1)
@testex Apply(List, f(a) < 1) == List(f(a), < , 1)
@testex Apply(List, f(a) < 1.0) == List(f(a), < , 1.0)



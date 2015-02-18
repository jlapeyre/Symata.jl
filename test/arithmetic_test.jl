using Base.Test

# Using Int64T instead of, say, Int64, is meant to be temporary.
@testex Head(1//1) == Int64T
@testex Head(3//1) == Int64T

@ex ClearAll(a,b)
@testex Head(Re(a)) == Re
@testex Re(I*a)[2] == Im(a)
@testex Im(I*a) == Re(a)
@testex Im(I*a*b) == Re(a*b)
@testex Re(I*a*b) == -Im(a*b)

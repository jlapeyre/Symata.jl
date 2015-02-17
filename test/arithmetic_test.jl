using Base.Test

# Using Int64T instead of, say, Int64, is meant to be temporary.
@testex Head(1//1) == Int64T
@testex Head(3//1) == Int64T



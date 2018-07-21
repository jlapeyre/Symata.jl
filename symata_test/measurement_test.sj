ClearAll(a,f)

T LeafCount(a) == 1
T LeafCount(a+f) == 3
T LeafCount(I) == 3
T LeafCount(1/2) == 3
T LeafCount(f(1,2,3,4,f(5))) == 7
T LeafCount([[1,[1, [1,[1,[1]]]]]]) == 11

T ByteCount(a) == 1
T ByteCount("cat") == 3

T Length(1) == 0
T Length(a) == 0
T Length(1/2) == 0
T Length([1,2,3]) == 3
T Length([]) == 0
T Length(f(1,2,3)) == 3
T Length(f()) == 0
T Length("cat") == 1
T StringLength("cat") == 3

T Depth(1) == 1
T Depth("cat") == 1
T Depth([1]) == 2
T Depth([1,[2]]) == 3
T Depth([1,f(2)]) == 3


T Dimensions(1) == 0
T Dimensions("dog") == 0
T Dimensions(a) == 0
T Dimensions([]) == []
T Dimensions([1]) == [1]
T Dimensions([1,1]) == [2]
T Dimensions([[1],[1]]) == [2,1]
T Dimensions(Transpose([[1],[1]])) == [1,2]
T Dimensions(Array(f,[2,3,4])) == [2,3,4]
T Dimensions(ReplaceAll(Array(f,[2,3,4]), f => List)) ==  [2,3,4,3]
T Dimensions(ReplaceAll(Array(f,[2,3,4]), List => f)) ==  [2,3,4,3]

T ArrayDepth(ReplaceAll(Array(f,[2,3,4]), List => f)) ==  4

  m = Array(f,[4,2,3])
T Dimensions(m) == [4,2,3]  
T Depth(m) == 5
  m[1,1,1] = [ f(1,1,1)]
T Depth(m) == 6
T Dimensions(m) == [4,2,3]
T Dimensions(IdentityMatrix(10)) == [10,10]

T Dimensions(Array(f,[1,2,3,4])) == [1,2,3,4]
T Dimensions(Array(f,[4,3,2,1])) == [4,3,2,1]
T Dimensions(Array(List,[4,3,2,1])) == [4,3,2,1,4]

ClearAll(m,f)

# Array creates these
ClearTemporary()









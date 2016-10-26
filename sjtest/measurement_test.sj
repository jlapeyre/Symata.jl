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




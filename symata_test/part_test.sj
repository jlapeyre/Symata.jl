Apply(ClearAll,UserSyms())

ex = [[1,2],[3,4], [a,b,c,[d,1]]]

## four ways to specify list of replacements
T ReplacePart(ex, [[2,2], 1, [3,4,2]] => zz) == [zz,[3,zz],[a,b,c,[d,zz]]]
T ReplacePart(ex, 1 => zz ) == [zz,[3,4],[a,b,c,[d,1]]]
T ReplacePart(ex,  [2,2]  => zz) == [[1,2],[3,zz],[a,b,c,[d,1]]]
T ReplacePart(ex, [1  => zz, 2 => cc] ) == [zz,cc,[a,b,c,[d,1]]]
## negative indices
T ReplacePart(ex, 1  => zz) == ReplacePart(ex, -3  => zz)
T ReplacePart(ex, [3,4] => zz) == ReplacePart(ex, [-1,-1]  => zz)
## Replace head
T ReplacePart(ex,  0  => zz) == zz([1,2],[3,4],[a,b,c,[d,1]])
T ReplacePart(ex,  0  => zz(x)) == zz(x)([1,2],[3,4],[a,b,c,[d,1]])
T ReplacePart(ex, [3,4,0] => gg) == [[1,2],[3,4],[a,b,c,gg(d,1)]]

## ReplacePart as operator
T ReplacePart([2,2]  => zz)(ex) == [[1,2],[3,zz],[a,b,c,[d,1]]]

## ReplacePart returns an altered copy
T ex == [[1,2],[3,4], [a,b,c,[d,1]]]

## Delayed rule evaluates rhs every time
rop = ReplacePart([2,2]  => RandomReal())
ropd = ReplacePart([2,2]  .> RandomReal())
T rop(ex) == rop(ex)
T ! BooleanQ(ropd(ex) == ropd(ex))

## fix inconsistency in Set(Part,...) : typeof(mx) == Mxpr{sym1} and mx.head=sym2 and sym1 != sym2
ex = [[1,2],[3,4], [a,b,c,[d,1]]]
ex[3,4,0] = ff
T Head(ex[3,4]) == ff
## Before bug fix, following returned True
T ListQ(ex[3,4]) == False

## Not sure if we want this, but it is current behavior.
## tests avoiding comparing :a to zero
ex = [[1,2],[3,4], [a,b,c,[d,1]]]
ex[3,4,2] = J( Dict( :a => "cat"))
ex[3,4,2,a] == "cat"


Apply(ClearAll,UserSyms())

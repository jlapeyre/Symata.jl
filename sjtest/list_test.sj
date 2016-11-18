Apply(ClearAll,UserSyms())

### Union

## TODO: sorting
T Head(Union(a)) == Union
T Union([1,1]) == [1]
T Union(Range(10)) == Range(10)
T Union(Range(10),Range(10)) == Range(10)
T Union([a,b,c],[a],[c,d,e]) == [a,b,c,d,e]
T Union(f(a, b), f(c, a), f(b, b, a)) == f(a,b,c)

### Accumulate

T Accumulate([a,b,c]) == [a,a + b,a + b + c]

### Array

ClearAll(f,g,b,a)

T Array(f,3) == [f(1),f(2),f(3)]

g = J( x -> mplus(x^2 , :a))
T Array(g,3) == [1 + a,4 + a,9 + a]

b = 3
g = J( x -> mplus(x^2, :b))
T Array(g,3) == [4,7,12]

T Array(a,[2,3]) == [[a(1,1),a(1,2),a(1,3)],[a(2,1),a(2,2),a(2,3)]]

f = Compile([x,y], x^2 + 2*y)
Array(f, [3,4]) == [[3,5,7,9],[6,8,10,12],[11,13,15,17]]

T Array( J( x -> x^2 ), 5) == [1,4,9,16,25]

T Array(a, 5, 0) == [a(0),a(1),a(2),a(3),a(4)]

T Array(a, 5, [0,1])  == [a(0),a(1/5),a(2/5),a(3/5),a(4/5),a(1)]

T Array(J((x,y)->0),[2,2]) == [[0,0],[0,0]]

ClearAll(f)

T Array(f, [3,3], [-3,0])  == [[f((-3),0),f((-3),1),f((-3),2)],[f((-2),0),f((-2),1),f((-2),2)],[f((-1),0),f((-1),1),f((-1),2)]]

T Array(f, [2,2,2], [-2,2,-2]) == [[[f((-2),2,-2),f((-2),2,-1)],[f((-2),3,-2),f((-2),3,-1)]],[[f((-1),2,-2),f((-1),2,-1)],[f((-1),3,-2),f((-1),3,-1)]]]

T Array(f, [2,2], [[-1/2,1/2], [-1/2,1/2]]) == [[f((-1/2),-1/2),f((-1/2),0),f((-1/2),1/2)],[f(0,-1/2),f(0,0),f(0,1/2)],[f((1/2),-1/2),f((1/2),0),f((1/2),1/2)]]

ClearAll(f,g,b)

### First

T First([1,2,3]) == 1
T First(f(a,b,c)) == a

### Join

T Join([1,2,3],[4,5,6]) == [1,2,3,4,5,6]
T Head(Join(f(1,2,3),[4,5,6])) == Join
T Join(f(1,2,3),f(4,5,6)) == f(1,2,3,4,5,6)

### Last

T Last([a,b,c]) == c
T Last(f(a,b,c)) == c
n = 1
T Last([x],Increment(n)) == x
T n == 1
T Last([],Increment(n)) == 1
T n == 2

### Most

T Most(Range(5)) == [1,2,3,4]

### Rest

T Rest([1]) == []
T Rest([1,2,3]) == [2,3]
T Rest(f(a,b,c)) == f(b,c)

### Flatten

T  Flatten( [ [a, b], [c, [d], e], [f, [g, h]]] , 1) == [a,b,c,[d],e,f,[g,h]]
T  Flatten( [ [a, b], [c, [d], e], [f, [g, h]]] , 2) == [a,b,c,d,e,f,g,h]

T Flatten(f(1,2,3,f(4,5,f(f(6)))), 1 ) == f(1,2,3,4,5,f(f(6)))
T Flatten(f(1,2,3,f(4,5,f(f(6)))), 2 ) == f(1,2,3,4,5,f(6))
T Flatten(f(1,2,3,f(4,5,f(f(6)))), 3 ) == f(1,2,3,4,5,6)
T Flatten(f(1,2,3,f(4,5,f(f(6))))) == f(1,2,3,4,5,6)
T Flatten(f(1,2,3,f(4,5,f(f(6)))), Infinity) == f(1,2,3,4,5,6)
T Flatten(f(g(u, v), f(x, y)), Infinity, g) == f(u,v,f(x,y))
T Flatten(f(g(u, v), f(x, y)), Infinity, f) == f((g(u,v)),x,y)

ex = Range(5)
T Flatten([ex,x,ex,x]) == [1,2,3,4,5,x,1,2,3,4,5,x]

ClearAll(a,b,c,d,e,f,g,h,x,ex,n)

### Fold

T Fold(f,x,[a,b,c]) == f((f((f(x,a)),b)),c)
T Fold(f,[a,b,c]) == f((f(a,b)),c)
T Fold(f,p(a,b,c)) == f((f(a,b)),c)
T Fold(Plus,0,Range(10^3)) == 500500
T Apply(Plus,Range(10^3))  == 500500

### FoldList

T FoldList(f,x,[a,b,c]) == [x,f(x,a),f((f(x,a)),b),f((f((f(x,a)),b)),c)]
T FoldList(f,[a,b,c]) == [a,f(a,b),f((f(a,b)),c)]
T FoldList(f,x, p(a,b,c)) == p(x,f(x,a),f((f(x,a)),b),f((f((f(x,a)),b)),c))

ClearAll(a,b,c,f,x,p)

### Nest

T Nest(f,x,0) == x
T Nest(f,x,1) == f(x)
T Nest(f,x,3) == f(f(f(x)))
T Nest(J( x -> mplus(x, 1)), x,100) == 100 + x
T Chop(Nest( J( x -> (x + 2/x)/2 ), 1.0, 5) - Sqrt(2.0)) == 0

ClearAll(f,x)

### NestList

T NestList(f,x,0) == [x]
T NestList(f,x,1) ==  [x,f(x)]
T NestList(f,x,3) == [x,f(x),f(f(x)),f(f(f(x)))]

### NestWhile

T NestWhile(Log,100, Function(xx, N(xx)>0)) == Log(Log(Log(Log(100))))
T NestWhile( x -> x/2 , 123456, EvenQ) == 1929

### NestWhileList

T NestWhileList( x -> x/2 , 123456, EvenQ) == [123456,61728,30864,15432,7716,3858,1929]
T NestWhileList(Log, 100, x -> N(x) > 0) == [100,Log(100),Log(Log(100)),Log(Log(Log(100))),Log(Log(Log(Log(100))))]
T [res = NestWhileList( y -> Mod(2*y, 19) , 2, x -> x != 1 ), Length(res)] == [[2,4,8,16,13,7,14,9,18,17,15,11,3,6,12,5,10,1],18]

ClearAll(f,x)

T  Range(0) == []
T  Range(1) == [1]
T  Range(3) == [1,2,3]
T  Range(2,4) == [2,3,4]
T  Range(2,12,3) == [2,5,8,11]
T  Range(5,1,-1) == [5,4,3,2,1]
T  Range(5,1,-2) == [5,3,1]
T  Range(3.0) == [1.0,2.0,3.0]
T  Range(100,102.1) == [100,101,102]  # Mma returns integers, as well.

T  Range(3.0,1,-.5) == [3.0,2.5,2.0,1.5,1.0]
T  Abs(Apply(Plus, Range(1,2,.2) -[1,1.2,1.4,1.6,1.8,2.0])) < 1e-15

T  Range(x, x + 4,1) == [x,1 + x,2 + x,3 + x,4 + x]

# Not putting paren around argument to T causes gensym to appear
# in UserSyms(). No idea why
T ( m = Range(x, x + 5) == [x,1 + x,2 + x,3 + x,4 + x,5 + x])

# T  Syms(m) == [x]  FIXME
T  Range(x+y, x + y  + 4) == [x + y,1 + x + y,2 + x + y,3 + x + y,4 + x + y]

# T  Range(5,1)  FIXME, do something or graceful error

T  Range(5+x,x,-1) == [5 + x,4 + x,3 + x,2 + x,1 + x,x]

# For some reason, this, but not the examples above, triggered a bug.
T Map(Function(x,Range(x+1,2*x)), Range(4)) == [[2],[3,4],[4,5,6],[5,6,7,8]]

ClearAll(x,y,a,b,c,d,m,p,z,f,j)

### ConstantArray

a = ConstantArray(1+d^2,3)
a[2,2,2] = 3
T a[2] == 1 + d^3
T a[1] == 1 + d^2
T a[3] == 1 + d^2

T ConstantArray(0,[2,3,4]) == Array(x -> 0,[2,3,4])
T ConstantArray(c,[2,3,4]) == Array(x -> c,[2,3,4])
T ConstantArray(1+d^2,[2,3,4]) == Array(x -> 1+d^2,[2,3,4])
T Head(ConstantArray(0,[a,b])) == ConstantArray

ClearAll(a,d,i)

### Map

a = J( [1,2,3] )
f = J( x -> x^2 )
T Map(f,a) == [1,4,9]

T Map(g)([1,a,"cat"]) == [g(1),g(a),g("cat")]
T Map(EvenQ)(Range(4)) == [False,True,False,True]

## fix bug because Sqrt had no rule (only translated on input)
T Map(Sqrt,[4]) == [2]

ClearAll(f,a,b,c,d,e)

T Map(f,[a,b,c,d,e]) == [f(a),f(b),f(c),f(d),f(e)]
T Function(x,x^2) % [1,2,3,4] == [1,4,9,16]
T Map(f, [[a,b],[c,d]])  == [f([a,b]),f([c,d])]
T Map(f, [[a,b],[c,d]],[2]) == [[f(a),f(b)],[f(c),f(d)]]
T Map(f,[[a,b],[c,d]],2) == [f([f(a),f(b)]),f([f(c),f(d)])]
T Map(f)([a,b,c,d,e]) == [f(a),f(b),f(c),f(d),f(e)]
ex = [[[[[a]]]]]
T Map(f,ex) == [f([[[[a]]]])]
T Map(f,ex,2) == [f([f([[[a]]])])]
T Map(f,ex,[2]) == [[f([[[a]]])]]
T Map(f,ex,[0,2]) == f([f([f([[[a]]])])])
T Map(f,ex,3) == [f([f([f([[a]])])])]
T Map(f,ex,Infinity) == [f([f([f([f([f(a)])])])])]
T Map(f,ex,[0,Infinity]) == f([f([f([f([f([f(a)])])])])])
T Map(f,ex,-1) == [f([f([f([f([f(a)])])])])]
T Map(f,ex,-2) == [f([f([f([f([a])])])])]
T Map(f,ex,-3) == [f([f([f([[a]])])])]
T Map(f,ex,[2,-3]) == [[f([f([[a]])])]]     
T Map(f, h0(h1(h2(h3(h4(a))))), [2, -3]) == h0(h1(f(h2(f(h3(h4(a)))))))
T Map(f, a + b + c + d) == f(a) + f(b) + f(c) + f(d)
T Map(f, x^2 + y^2, 2) == f(f(x)^f(2)) + f(f(y)^f(2))

ClearAll(a,f,ex)

### Nothing

T Nothing() == Nothing
T Nothing(a,b) == Nothing
T Nothing(1,[3,4]) == Nothing
T [Nothing] == []
T [1,Nothing] == [1]
T [a,Nothing,b,Nothing,Nothing,c] == [a,b,c]
T Length(f(a,Nothing,b)) == 3

# Hold prevents removal of Nothing
ex = Hold([Nothing,1,2])
T ex[1,3] == 2

T  ReplaceAll([a,b,c,d], c => Nothing) == [a,b,d]

T [[ a, b, Nothing], [c, d, Nothing]] == [[a,b],[c,d]]

T ReplaceAll( Range(20) ,  _:?(PrimeQ) => Nothing) == [1,4,6,8,9,10,12,14,15,16,18,20]

ClearAll(a,b,c,d,f,ex)

### Subdivide

T Subdivide(5) == [0,1/5,2/5,3/5,4/5,1]
T Subdivide(2,5) == [0,2/5,4/5,6/5,8/5,2]
T Subdivide(1,2,5) == [1,6/5,7/5,8/5,9/5,2]


### Take

ClearAll(a,b,c,d,e,f,expr)

expr = a + b + c + d + e + f

T Take(expr,1) == a
T Take(expr,2) == a + b
T Take(expr,3) == a + b + c
T Take(expr,None) == 0
T Take(expr,All) == expr
T Take(expr,-1) == f
T Take(expr,-2) == e + f
T Take(expr,[2,4]) == b + c + d
T Take(expr,[1,6,2]) == a + c + e
T Take(Range(3), UpTo(4)) == Range(3)
T Take(Range(10), [-1,1,-1]) == Reverse(Range(10))
expr = Array(f,[3,3,3])
Take(r,[2],[2],[2]) == [[[f(2,2,2)]]]

### Drop

T Drop(Range(10),5) == Range(6,10)
T Drop(Range(10),-1) == Range(9)
T Drop(Range(10),1) == Range(2,10)
T Drop(Range(10),10) == []
T Drop(Range(10),-10) == []
T Drop(Range(10),11) == []   # Mma gives an error here
T Drop(Range(10),UpTo(11)) == []
T Drop(Range(10),[3,6]) == [1,2,7,8,9,10]

T TakeDrop(Range(10),[3,6]) == [Take(Range(10),[3,6]), Drop(Range(10),[3,6])]


T Split([a,a, b,b,b,b,c,c,c,c]) == [[a,a],[b,b,b,b],[c,c,c,c]]

### Partition

ClearAll(a,b,c,d,e,f,expr,unflatten,lst,ftest)

# Convert a flat list into an array with dimensions d = [n1,n2,...]
ftest(x_) :=  IntegerQ(x) && Positive(x)
unflatten(e_, [d__:?(ftest)]) :=  Condition(
           Fold(Partition, e, 
             Take([d], [-1, 2, -1])), (Length(e) == Times(d)) == True)
lst = [a,b,c,d,e,f]

# Note: Length(e) == Times(d) fails if BigIntInput(True)
# FIXME: d__:?(ftest) does not correctly apply the test

T unflatten(lst , [2,3])  == [[a,b,c],[d,e,f]]
T unflatten(lst , [3,2])  == [[a,b],[c,d],[e,f]]
T Head(unflatten(lst , [3,3])) == unflatten

T Dimensions(unflatten(Range(Apply(Times,[8,9,10])), [8,9,10])) == [8,9,10]

T ListCorrelate([x,y], Range(10)) == [x + 2y,2x + 3y,3x + 4y,4x + 5y,5x + 6y,6x + 7y,7x + 8y,8x + 9y,9x + 10y]
T ListConvolve([x,y], Range(10)) == [2x + y,3x + 2y,4x + 3y,5x + 4y,6x + 5y,7x + 6y,8x + 7y,9x + 8y,10x + 9y]

### ComposeList

T ComposeList([f,g,h],x) == [h(x),g(h(x)),f(g(h(x)))]

### Thread

T Thread(f([a, b, c], x)) == [f(a,x),f(b,x),f(c,x)]
T Thread(f([a, b, c], [x, y, z])) == [f(a,x),f(b,y),f(c,z)]
T Thread([a, b, c] == [x, y, z]) == [a == x,b == y,c == z]
T Thread(Log(x == y), Equal) == (Log(x) == Log(y))
T Thread(f([a, b], [r, s], [u, v], [x, y]), List) == [f(a,r,u,x),f(b,s,v,y)]
T Thread(f([a, b], [r, s], [u, v], [x, y]), List, All) == [f(a,r,u,x),f(b,s,v,y)]
T Thread(f([a, b], [r, s], [u, v], [x, y]), List, None) == f(([a,b]),([r,s]),([u,v]),[x,y])
T Thread(f([a, b], [r, s], [u, v], [x, y]), List, 2) == [f(a,r,([u,v]),[x,y]),f(b,s,([u,v]),[x,y])]
T Thread(f([a, b], [r, s], [u, v], [x, y]), List, -2) == [f(a,r,u,[x,y]),f(b,s,v,[x,y])]
T Thread(f([a, b], [r, s], [u, v], [x, y]), List, [2]) == [f(([a,b]),r,([u,v]),[x,y]),f(([a,b]),s,([u,v]),[x,y])]
T Thread(f([a, b], [r, s], [u, v], [x, y]), List, [2, 4]) == [f(([a,b]),r,u,x),f(([a,b]),s,v,y)]
T Thread(f([a, b], [r, s], [u, v], [x, y]), List, [1, -1, 2]) == [f(a,([r,s]),u,[x,y]),f(b,([r,s]),v,[x,y])]
T Thread(f([a, b], [c, d])) == [f(a,c),f(b,d)]
T Thread(f([a, b], [c, d]), List) == [f(a,c),f(b,d)]
T Thread(f(a + b, c + d)) == f((a + b),c + d)
T Thread(f(a + b, c + d), Plus) == f(a,c) + f(b,d)
T Thread(f([a, b, c], h, [x, y, z])) == [f(a,h,x),f(b,h,y),f(c,h,z)]
T Thread([a, b, c] => [1, 2, 3]) == [a => 1,b => 2,c => 3]
T Thread([[a, b, c], [x, y, z]])  == Transpose([[a, b, c], [x, y, z]])
T Thread(Unevaluated(D([x, x * y, x * z], [x, y, z]))) == [1,x,x]

### MapThread

T MapThread(D, [[x, x * y, x * z], [x, y, z]]) == [1,x,x]


Apply(ClearAll,UserSyms())

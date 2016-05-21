T testUserSyms

#### First

T First([1,2,3]) == 1
T First(f(a,b,c)) == a

#### Last

T Last([a,b,c]) == c
T Last(f(a,b,c)) == c
n = 1
T Last([x],Increment(n)) == x
T n == 1
T Last([],Increment(n)) == 1
T n == 2

#### Most

T Most(Range(5)) == [1,2,3,4]

#### Rest

T Rest([1]) == []
T Rest([1,2,3]) == [2,3]
T Rest(f(a,b,c)) == f(b,c)

#### Flatten

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

#### Fold

T Fold(f,x,[a,b,c]) == f((f((f(x,a)),b)),c)
T Fold(f,[a,b,c]) == f((f(a,b)),c)
T Fold(f,p(a,b,c)) == f((f(a,b)),c)
T Fold(Plus,0,Range(10^3)) == 500500
T Apply(Plus,Range(10^3))  == 500500

#### FoldList

T FoldList(f,x,[a,b,c]) == [x,f(x,a),f((f(x,a)),b),f((f((f(x,a)),b)),c)]
T FoldList(f,[a,b,c]) == [a,f(a,b),f((f(a,b)),c)]
T FoldList(f,x, p(a,b,c)) == p(x,f(x,a),f((f(x,a)),b),f((f((f(x,a)),b)),c))

ClearAll(a,b,c,f,x,p)

#### Nest

T Nest(f,x,0) == x
T Nest(f,x,1) == f(x)
T Nest(f,x,3) == f(f(f(x)))
T Nest(:( x -> x + 1), x,100) == 100 + x
T Chop(Nest( :( x -> (x + 2/x)/2 ), 1.0, 5) - Sqrt(2.0)) == 0

ClearAll(f,x)

#### NestList

T NestList(f,x,0) == [x]
T NestList(f,x,1) ==  [x,f(x)]
T NestList(f,x,3) == [x,f(x),f(f(x)),f(f(f(x)))]

ClearAll(f,x)

#### Table

# A gensym is added to user symbols somewhere here.
# It is not removed and is not displayed properly.

T  Table(i,[i,3]) == [1,2,3]
T  Table(a(i^2),[i,3]) == [a(1),a(4),a(9)]
      a(9) := "zebra"
      a(x_) := x + 1
T  Table(a(i^2),[i,4])  == [2,5,"zebra",17]

# Tests two bug fixes
T  Table(x^n+2*x,[n,3]) == [3*x,2*x + x^2,2*x + x^3]
T  Table(x^i + x*i + 1,[i,5]) == [1 + 2*x,1 + 2*x + x^2,1 + 3*x + x^3,1 + 4*x + x^4,1 + 5*x + x^5]

ClearAll(p,z,i)
p = 1
z := Increment(p)
T  Table(i, [i, [z,z,z]]) == [1,2,3]
T  Table( i, [i, Range(10)]) == Range(10)
T  Table(i^2, [i, [a+b,c,d]]) == [(a + b)^2,c^2,d^2]
T  Table(f(i), [i, 10, -5, -2]) == [f(10),f(8),f(6),f(4),f(2),f(0),f(-2),f(-4)]
T  Table(Table( i * j , [i,3]), [j,3]) == [[1,2,3],[2,4,6],[3,6,9]]

T Table( f(i,j) , [i,3], [j,4]) == Table(Table( f(i,j) , [j,4]), [i,3])

T Table(i + j - 1, [i, 4], [j, i]) == [[1],[2,3],[3,4,5],[4,5,6,7]]

# FIXME  Does not work
#  Table(2^x + x, [x, a, a + 5 *n, n])
# Neither is this implemented for Range
# FIXME Does not work
#  Table( zz(x), [zz(x), 5])

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

ClearAll(x,y,a,b,c,d,m,p,z,f,j)

#### ConstantArray

a = ConstantArray(1+d^2,3)
a[2,2,2] = 3
T a[2] == 1 + d^3
T a[1] == 1 + d^2
T a[3] == 1 + d^2

ClearAll(a,d,i)

#### Nothing

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

T Table(Nothing,[10]) == []

ClearAll(a,b,c,d,f,ex)

T testUserSyms

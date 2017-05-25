#### SJIter1

T  Table(x,[0]) == []
T  Table(x,[3]) == [x,x,x]

#### SJIter2

T  Table(i,[i,3]) == [1,2,3]
T  Table( i, [i, Range(10)]) == Range(10)

T  Table(a(i^2),[i,3]) == [a(1),a(4),a(9)]
      a(9) := "zebra"
      a(x_) := x + 1
T  Table(a(i^2),[i,4])  == [2,5,"zebra",17]

# Tests two bug fixes
T  Table(x^n+2*x,[n,3]) == [3*x,2*x + x^2,2*x + x^3]
T  Table(x^i + x*i + 1,[i,5]) == [1 + 2*x,1 + 2*x + x^2,1 + 3*x + x^3,1 + 4*x + x^4,1 + 5*x + x^5]

T  Table(Table( i * j , [i,3]), [j,3]) == [[1,2,3],[2,4,6],[3,6,9]]

T Table( f(i,j) , [i,3], [j,4]) == Table(Table( f(i,j) , [j,4]), [i,3])

T Table(i + j - 1, [i, 4], [j, i]) == [[1],[2,3],[3,4,5],[4,5,6,7]]

## Dynamic scope for iteration variable

ClearAll(y,n,g)

g(y_) := y + n + 1

T Table(g(n), [n,3]) == [3,5,7]

n = 8

T Table(g(n), [n,3]) == [3,5,7]

T n == 8

ClearAll(y,n,g)


#  FIXME Does not work
#  Table( zz(x), [zz(x), 5])

###### SJIter3

T Table(i, [i,4,8]) == [4,5,6,7,8]

###### SJIter4

T  Table(f(i), [i, 1, 8, 2]) == [f(1),f(3),f(5),f(7)]
T  Table(f(i), [i, 10, -5, -2]) == [f(10),f(8),f(6),f(4),f(2),f(0),f(-2),f(-4)]

## Symbolic increment
## FIXME: v0.6 broken
#T Table(2^x + x, [x, a, a + 5 *n, n]) == [2^a + a,2^(a + n) + a + n,2^(a + 2n) + a + 2n,2^(a + 3n) + a + 3n,2^(a + 4n) + a + 4n,2^(a + 5n) + a + 5n]

####### SJIterList

ClearAll(p,z,i)
p = 1
z := Increment(p)
T  Table(i, [i, [z,z,z]]) == [1,2,3]
T  Table(i^2, [i, [a+b,c,d]]) == [(a + b)^2,c^2,d^2]

T Table(Nothing,[10]) == []

ClearAll(it)

it = [i,3]

T Table( i^2 , Evaluate(it)) == [1,4,9]

ClearAll(it)

ClearAll(x,y,a,b,c,d,m,p,z,f,j)
ClearAll(a,b,c,d,f,ex)

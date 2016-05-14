
T testUserSyms

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
T(  m = Range(x, x + 5) == [x,1 + x,2 + x,3 + x,4 + x,5 + x])

# T  Syms(m) == [x]  FIXME
T  Range(x+y, x + y  + 4) == [x + y,1 + x + y,2 + x + y,3 + x + y,4 + x + y]

# T  Range(5,1)  FIXME, do something or graceful error

T  Range(5+x,x,-1) == [5 + x,4 + x,3 + x,2 + x,1 + x,x]

      ClearAll(x,y,a,d,m)

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

ClearAll(a,b,c,d,ex)

T testUserSyms




T testUserSyms

#### Do

 ClearAll(a,i,x,q)

 a = 0
 Do(a = a + 1 , [10])
T a == 10

 (i = 0; Do( (i += 1; If(i>3, Break())) , [10]))
T i == 4

# Test that i is localized
 a = 0
 i = "zebra"
 Do(a = a + i , [i,10])

T  a == 55
T i == "zebra"
 a = 0
 Do(a = a + i , [i,10,1,-1])
T  a == 55
 a = 0
 Do(a = a + x , [x,q, 5*q,q])
T a == 15*q

  a = 0
  Do(a = a + x , [x,5*q,q,-q])
T  a == 15*q

 a = 0
 Do(a = a + x , [x,[1,2,3]])
T a == 6
 ClearAll(a,i,x,q)

#### For

 m = [0,0,0]
 For(i=1, i <= 3, i = i + 1, m[i] = i + 1)
T [2,3,4] == m

 For(i=1,i<10, (Increment(i); If(i>3, Break())))
T i == 4
 For(i=10,i>0, (Decrement(i); If(i<5, Break())))
T i == 4

# Giving the wrong number of arguments
# TODO. Implement Quiet to silence the warning messages.
T Head(For(1)) == For
T Head(For(1,2,3,4,5)) == For

ClearAll(m,i,res)


#### While

 ClearAll(i)
 i = 0
 While(i < 5, i = i + 1)
T i == 5
 i = 0
T While(i < 5, (i = i + 1; If(i>2, Break()))) == Null
T i == 3
 ClearAll(i)

 n = 1
  While(n<10,
          (n += 1; If(n>3, Break()); n))
T n == 4
 ClearAll(n)

 ClearAll(i,sum)
 sum = 0
 Do( sum += i, [i,[1,2,3,4,5]])
T Head(i) == Symbol
T sum == 15
 ClearAll(i,sum)

 n = 1
T While( (n += 1) < 4) == Null
T n == 4
 ClearAll(n)

 (m = 0; n = 0)
T While( (n += 1) < 10, ( If(n>3, Continue()); m += n) ) == Null
T n == 10
T m == 6


#### If

T If(True,1) == 1
T If(False,1,True)
T If("cat",1,2,True)
T If(False,1) == Null
 ClearAll(retv)

#### Break, Continue, Return

T (1;2;Return(0);3;4) == Return(0)

 n = 0
T Do( (n += i; If(n>3, Return("dog"))) , [i,10]) == "dog"

 n = 0
T For(i=1, i<100, i += 1, (If(i>3, Continue()); n += i)) == Null
T i == 100
T n == 6

T For(i=1, i<100, i += 1, If(i>3,Return(0))) == Return(0)
T i == 4

 ClearAll(n,i)

 n = 0
T Do( ( If(i>3, Continue()); n += i ) , [i,100]) == Null
T n == 6
T Head(i) == Symbol

 ClearAll(n,i,m)
 ClearTemporary()

T testUserSyms

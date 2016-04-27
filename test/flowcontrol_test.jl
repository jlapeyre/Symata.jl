using Base.Test

@ex If( Length(UserSyms()) > 0 ,  Println("**********", UserSyms()))
@testex Length(UserSyms()) == 0

#### Do

@ex ClearAll(a,i,x,q)

@ex a = 0
@ex Do(a = a + 1 , [10])
@testex a == 10

@ex (i = 0; Do( (i += 1; If(i>3, Break())) , [10]))
@testex i == 4

@ex a = 0
@ex i = "zebra"
@ex Do(a = a + i , [i,10])  # what am I doing here ?

@ex a == 55
@ex i == "zebra"
@ex a = 0
@ex Do(a = a + i , [i,10,1,-1])
@ex a == 55
@ex a = 0
@ex Do(a = a + x , [x,q, 5*q,q])
@ex a == 15*q

# disabling (a*b)^number -> a^number * b^number has broken this
# @ex a = 0
# @ex Do(a = a + x , [x,5*q,q,-q])
# @ex a == 15*q

@ex a = 0
@ex Do(a = a + x , [x,[1,2,3]])
@ex a == 6
@ex ClearAll(a,i,x,q)

#### For

@ex m = [0,0,0]
@ex For(i=1, i <= 3, i = i + 1, m[i] = i + 1)
@ex res = ([2,3,4] == m)
@test symval(:res) == true

@ex For(i=1,i<10, (Increment(i); If(i>3, Break())))
@testex i == 4

@testex Head(For(1)) == For
@testex Head(For(1,2,3,4,5)) == For

@ex ClearAll(m,i,res)


#### While

@ex ClearAll(i)
@ex i = 0
@ex While(i < 5, i = i + 1)
@testex i == 5
@ex i = 0
@testex While(i < 5, (i = i + 1; If(i>2, Break()))) == Null
@testex i == 3
@ex ClearAll(i)

@ex n = 1
@ex  While(n<10,
          (n += 1; If(n>3, Break()); n))
@testex n == 4
@ex ClearAll(n)

@ex ClearAll(i,sum)
@ex sum = 0
@ex Do( sum += i, [i,[1,2,3,4,5]])
@testex Head(i) == Symbol
@testex sum == 15
@ex ClearAll(i,sum)

@ex n = 1
@testex While( (n += 1) < 4) == Null
@testex n == 4
@ex ClearAll(n)

@ex (m = 0; n = 0)
@testex While( (n += 1) < 10, ( If(n>3, Continue()); m += n) ) == Null
@testex n == 10
@testex m == 6


#### If

@testex If(True,1) == 1
@testex If(False,1,True)
@testex If("cat",1,2,True)
@testex If(False,1) == Null
@ex ClearAll(retv)

#### Break, Continue, Return

@testex (1;2;Return(0);3;4) == Return(0)

@ex n = 0
@testex Do( (n += i; If(n>3, Return("dog"))) , [i,10]) == "dog"

@ex n = 0
@testex For(i=1, i<100, i += 1, (If(i>3, Continue()); n += i)) == Null
@testex i == 100
@testex n == 6

@testex For(i=1, i<100, i += 1, If(i>3,Return(0))) == Return(0)
@testex i == 4

@ex ClearAll(n,i)

@ex n = 0
@testex Do( ( If(i>3, Continue()); n += i ) , [i,100]) == Null
@testex n == 6
@testex Head(i) == Symbol

@ex ClearAll(n,i,m)

@testex Length(UserSyms()) == 0

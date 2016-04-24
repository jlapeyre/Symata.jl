using Base.Test

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
@ex a = 0
@ex Do(a = a + x , [x,5*q,q,-q])
@ex a == 15*q
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

#### If

@testex If(True,1) == 1
@testex If(False,1,True)
@testex If("cat",1,2,True)
@testex If(False,1) == Null
@ex ClearAll(retv)

@testex Length(UserSyms()) == 0

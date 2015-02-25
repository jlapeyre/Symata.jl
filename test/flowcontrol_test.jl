using Base.Test

SJulia.@ex ClearAll(a,i,x,q)
@ex a = 0
@ex Do(a = a + 1 , [10])
@testex a == 10
@ex a = 0
@ex i = "zebra"
@ex Do(a = a + i , [i,10])
@ex a == 55
@ex i == "zebra"
@ex a = 0
@ex Do(a = a + i , [i,10,1,-1])
@ex a == 55
SJulia.@ex a = 0
SJulia.@ex Do(a = a + x , [x,q, 5*q,q])
SJulia.@ex a == 15*q
@ex a = 0
@ex Do(a = a + x , [x,5*q,q,-q])
@ex a == 15*q
@ex a = 0
@ex Do(a = a + x , [x,[1,2,3]])
@ex a == 6
SJulia.@ex ClearAll(a,i,x,q)

using Base.Test

@ex      ClearAll(a,x)
@testex  Table(i,[i,3]) == [1,2,3]
@testex  Table(a(i^2),[i,3]) == [a(1),a(4),a(9)]
@ex      a(9) := "zebra"
@ex      a(x_) := x + 1
@testex  Table(a(i^2),[i,4])  == [2,5,"zebra",17]

@testex  Range(0) == []
@testex  Range(1) == [1]
@testex  Range(3) == [1,2,3]
@testex  Range(2,4) == [2,3,4]
@testex  Range(2,12,3) == [2,5,8,11]
@testex  Range(5,1,-1) == [5,4,3,2,1]
@testex  Range(5,1,-2) == [5,3,1]
@testex  Range(3.0) == [1.0,2.0,3.0]
@testex  Range(100,102.1) == [100.0,101.0,102.0]
@testex  Range(3.0,1,-.5) == [3.0,2.5,2.0,1.5,1.0]
@testex  Abs(Apply(Plus, Range(1,2,.2) -[1,1.2,1.4,1.6,1.8,2.0])) < 1e-15
@testex  Range(x, x + 4,1) == [x,1 + x,2 + x,3 + x,4 + x]
@testex  Range(x, x + 5) == [x,1 + x,2 + x,3 + x,4 + x,5 + x]
@testex  Range(x+y, x + y  + 4) == [x + y,1 + x + y,2 + x + y,3 + x + y,4 + x + y]
# @testex  Range(5,1)  FIX
@testex  Range(5+x,x,-1) == [5 + x,4 + x,3 + x,2 + x,1 + x,x]


@ex      ClearAll(a,x)

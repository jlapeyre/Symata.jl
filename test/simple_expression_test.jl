@ex ClearAll(a,b)
@testex  b^0 == 1
@testex  b^1 == b
@testex  b*1 == b
@testex  1*b == b
@testex  -(a+b) == -a - b  # lhs is expanded
@testex  -1*(a+b) == -a - b
@ex ClearAll(a,b)

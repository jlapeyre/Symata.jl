using Base.Test

SJulia.@ex ClearAll(a,b)
SJulia.@ex mx = ExpandA((a+b)^3)
SJulia.@testex  mx == Plus(Power(a,3),Times(3,Power(a,2),b),Times(3,a,Power(b,2)),Power(b,3))
SJulia.@testex Fixed(mx) == true
SJulia.@ex a = 1
SJulia.@ex mx
SJulia.@testex Fixed(mx) == false
SJulia.@ex ClearAll(a)
SJulia.@ex mx
SJulia.@testex Fixed(mx) == true

SJulia.@ex ClearAll(a,b,c,d,z)
SJulia.@testex ExpandA(1) == 1
SJulia.@testex ExpandA(a) == a
SJulia.@testex ExpandA(a+b) == a+b
SJulia.@testex ExpandA(a*b) == a*b
SJulia.@testex (ExpandA(a*(b+c)) == a*b + a*c) == True
SJulia.@testex ExpandA(z*(b+c)) == b*z + c*z  # tests diffent branch in doexpand because z < a,b,c.
SJulia.@testex ExpandA(3*(b+c)) == 3*b + 3*c
@testex ExpandA(z*(c+d)*(a+b)) == a*c*z + b*c*z + a*d*z + b*d*z
SJulia.@testex ExpandA((a+1)^3) == 1 + 3*a + 3*(a^2) + a^3
@testex ExpandA((a+b)*(c+d)) == a*c + b*c + a*d + b*d
@testex Apply(List,ExpandA(z*a*(c+d)*(a+b))) == [(a^2)*c*z,a*b*c*z,(a^2)*d*z,a*b*d*z]
@ex ClearAll(a,b,c,d,z,mx)

SJulia.@ex ClearAll(m,f,g)
@ex m = [1,2,[3,4,5,f(6,7)]]
@testex m[2] == 2
@testex m[0] == List
@testex m[3] == [3,4,5,f(6,7)]
@ex m[1] = "cat"
@testex m == ["cat",2,[3,4,5,f(6,7)]]
@ex m[3,4,0] = g
@testex m == ["cat",2,[3,4,5,g(6,7)]]
@ex m[-2] = 88
@testex m == ["cat",88,[3,4,5,g(6,7)]]
@testex m[-1,-1] == g(6,7)
@ex ClearAll(m,f,g)

@ex ClearAll(f,a,b,g,c,d,m)
@ex m = f(a,b,g(c(d,b),a(a,b(c(d)))))
@testex Position(m,f) == [[0]]
@testex Position(m,a) == [[1],[3,2,1],[3,2,0]]
@testex Position(m,b) == [[2],[3,1,2],[3,2,2,0]]
@testex Position(m,c(d)) == [[3,2,2,1]]
@ex m = f(a,b,g(c(d,b),a(a,b(c(d)))),c(d,b))
@testex Position(m, c(d,b)) == [[3,1],[4]]
@ex ClearAll(f,a,b,g,c,d,m)

# Test Span
@ex ClearAll(m)
@ex m = Range(10)
@ex m[3] = Range(10)*4
@testex m[3,4:6] == [16,20,24]
@testex m[3,1:10:2] == [4,12,20,28,36]
@ex ClearAll(m)

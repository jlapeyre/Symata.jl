
T testUserSyms

 ClearAll(a,b)
 mx = ExpandA((a+b)^3)
T  mx == Plus(Power(a,3),Times(3,Power(a,2),b),Times(3,a,Power(b,2)),Power(b,3))
T FixedQ(mx) == true
 a = 1
 mx
T FixedQ(mx) == false
 ClearAll(a)
 mx
T FixedQ(mx) == true

 ClearAll(a,b,c,d,z)
T ExpandA(1) == 1
T ExpandA(a) == a
T ExpandA(a+b) == a+b
T ExpandA(a*b) == a*b
T (ExpandA(a*(b+c)) == a*b + a*c) == True
T ExpandA(z*(b+c)) == b*z + c*z  # tests diffent branch in doexpand because z < a,b,c.
T ExpandA(3*(b+c)) == 3*b + 3*c
T ExpandA(z*(c+d)*(a+b)) == a*c*z + b*c*z + a*d*z + b*d*z
T ExpandA((a+1)^3) == 1 + 3*a + 3*(a^2) + a^3
T ExpandA((a+b)*(c+d)) == a*c + b*c + a*d + b*d
T Apply(List,ExpandA(z*a*(c+d)*(a+b))) == [(a^2)*c*z,a*b*c*z,(a^2)*d*z,a*b*d*z]
 ClearAll(a,b,c,d,z,mx)

 ClearAll(m,f,g)
 m = [1,2,[3,4,5,f(6,7)]]
T m[2] == 2
T m[0] == List
T m[3] == [3,4,5,f(6,7)]
 m[1] = "cat"
T m == ["cat",2,[3,4,5,f(6,7)]]
 m[3,4,0] = g
T m == ["cat",2,[3,4,5,g(6,7)]]
 m[-2] = 88
T m == ["cat",88,[3,4,5,g(6,7)]]
T m[-1,-1] == g(6,7)
 ClearAll(m,f,g)

 ClearAll(f,a,b,g,c,d,m)
 m = f(a,b,g(c(d,b),a(a,b(c(d)))))
T Position(m,f) == [[0]]
T Position(m,a) == [[1],[3,2,1],[3,2,0]]
T Position(m,b) == [[2],[3,1,2],[3,2,2,0]]
T Position(m,c(d)) == [[3,2,2,1]]
 m = f(a,b,g(c(d,b),a(a,b(c(d)))),c(d,b))
T Position(m, c(d,b)) == [[3,1],[4]]
 ClearAll(f,a,b,g,c,d,m)

#### Span
 ClearAll(m)
 m = Range(10)
 m[3] = Range(10)*4
T m[3,4:6] == [16,20,24]
T m[3,1:10:2] == [4,12,20,28,36]
ClearAll(m)

#### ReleaseHold

T ReleaseHold(Hold()) == Sequence()
T ReleaseHold(HoldForm()) == Sequence()
T ReleaseHold(HoldPattern()) == Sequence()
T ReleaseHold(HoldComplete()) == Sequence()
T ReleaseHold(Hold(a)) == a
T List(ReleaseHold(Hold(a,b))) == [a,b]
T ReleaseHold(f(a,b)) == f(a,b)

ClearAll(a,b,f)

T testUserSyms

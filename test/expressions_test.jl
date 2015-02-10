using Base.Test

@ex Clear(a,b)
@ex mx = Expand((a+b)^3)
@testex  mx == Plus(Power(a,3),Times(3,Power(a,2),b),Times(3,a,Power(b,2)),Power(b,3))
@testex Fixed(mx) == true
@ex a = 1
@ex mx
@testex Fixed(mx) == false
@ex Clear(a)
@ex mx
@testex Fixed(mx) == true

@ex Clear(a,b,c,d,z)
@testex Expand(1) == 1
@testex Expand(a) == a
@testex Expand(a+b) == a+b
@testex Expand(a*b) == a*b
@testex Expand(a*(b+c)) == a*b + a*c
@testex Expand(z*(b+c)) == b*z + c*z  # tests diffent branch in doexpand because z < a,b,c.
@testex Expand(3*(b+c)) == 3*b + 3*c
@testex Expand(z*(c+d)*(a+b)) == a*c*z + b*c*z + a*d*z + b*d*z
@testex Expand((a+1)^3) == 1 + 3*a + 3*(a^2) + a^3
@testex Expand((a+b)*(c+d)) == a*c + b*c + a*d + b*d
@testex Apply(List,Expand(z*a*(c+d)*(a+b))) == [(a^2)*c*z,a*b*c*z,(a^2)*d*z,a*b*d*z]
@ex ClearAll(a,b,c,d,z,mx)

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
@ex Clear(a,b,c,d)
@ex Expand((a+1)^3)
@testex Expand((a+b)*(c+d)) == a*c + b*c + a*d + b*d

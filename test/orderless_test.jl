using Base.Test

@testex 0 == 0
@testex -0  == 0
@testex 1  == 1
@testex -1  == -1
@testex 1 - 1  == 0
@testex 1 + 1  == 2
@ex Clear(a)
@testex a  == a
@testex 0 + a  == a
@testex a + 0  == a
@testex a - 0  == a
@testex 1 * a  == a
@testex 0 * a  == 0
@testex a * 1  == a
@testex a * 0  == 0
@testex a * -0  == 0
@testex -0 * a  == 0
# fixed bug
@testex a + a * a - a * a == a
# fixed bug for this one
@testex Apply(List, zb + za + 7 + 2 * x^r  + 2 + a + c + x^3 + x^2) ==  [9,a,c,x^2,x^3,2*x^r,za,zb]
@testex Apply(List, z^7 + 2 * z^6 + 5*z^3 + 10 *z  + 1) == [1,10*z,5*z^3,2*z^6,z^7]
@testex Apply(List, a * a + 1 / ((z + y) * (z + y)) ) == [a ^ 2,(y + z) ^ -2]
@testex Apply(List,a + z + 10*a^2 + 2 * z^2) == [a,10*a^2,z,2*z^2]
@testex (a+b)/(a+b) == 1
@testex (a + b)^2/(a+b) == a + b
@testex (a^2)^2 == a^4
@testex (a^t)^2 == a^(2*t)
@testex (a^2)^t == a^(2*t)
@testex Part((a^t)^z,2) == z
@testex Apply(List,( y + z  + 1) * ( y + z)) == [(y + z),1 + y + z]  # bug fix

# Blank's are less than non-Blanks
@testex Apply(List, a_ + x) == List(x,a_)
@testex Apply(List, a_h + x) == List(x,a_h)
@testex Apply(List, a__ + x) == List(x,a__)
@testex Apply(List, a__h + x) == List(x,a__h)
@testex Apply(List, a_ + _x) == List(_x,a_)
@testex Apply(List, x_ + _x) == List(_x,x_)
@testex Apply(List, _ + __) == List(_,__)
@testex Apply(List, a + _x) == List(a,_x)

@ex ClearAll(f,x)
@testex Apply(List, x + f(x_)) == List(x,f(x_))
@testex Apply(List, _ + f(x_)) == List(_,f(x_))
@testex Apply(List, x_ + f(x)) == List(f(x),x_)
@testex Apply(List, x_ + f(x) + _) == List(_,f(x),x_)
@testex Apply(List, x_y + f(x) + _ + _y) == List(_,_y,f(x),x_y)
@ex ClearAll(f,x)

# bug present in 422b42d34e4c0c6e0f377c85d1d2e78b9c1bbb19 and earlier
@ex ClearAll(m,a,b,c)
@ex m = a*b + c
@ex -m      # must not change the value of m
@testex m == a*b + c
@ex ClearAll(m,a,b,c)


## Test do_canon_power!
@ex Clear(a,b,m)
@ex m = (2*a*b)^3
@testex Head(m) == Times
@testex Apply(List,m) == List(8,a^3,b^3)

@ex m = Expand((2*a+2*b)^BI(4))
@testex Apply(List,m) == [(16*(a^4)),(64*(a^3)*b),(96*(a^2)*(b^2)),(64*a*(b^3)),16*(b^4)]

@ex m = Expand((2*a+2*b)^4)
@testex Apply(List,m) == [(16*(a^4)),(64*(a^3)*b),(96*(a^2)*(b^2)),(64*a*(b^3)),16*(b^4)]

# bug through commit e9c6803de51dab446a1f3e9bcc8d1e8cf19941d3
# Causes infinite evaluation loop, due to ill-defined order.
@ex m = (Expand((x+1)^2))^2
@ex m * (m+1)

@ex ClearAll(a,A,Z,z)

@testex Apply(List,a*Z) == [a,Z]
@testex Apply(List,A*z) == [A,z]
@testex Apply(List,A*a) == [a,A]

@ex ClearAll(a,b,m,t,x,r,z,za,zb,c,h,y,z,A,Z)

#@testex Apply(List, a___ + x) == List(x,a___)  # BlankNullSequence not implemented

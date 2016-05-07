
T testUserSyms

   ClearAll(a,c,z,za,zg,x,y,r,t,h,A,Z)
T 0 == 0
T -0  == 0
T 1  == 1
T -1  == -1
T 1 - 1  == 0
T 1 + 1  == 2
 Clear(a)
T a  == a
T 0 + a  == a
T a + 0  == a
T a - 0  == a
T 1 * a  == a
T 0 * a  == 0
T a * 1  == a
T a * 0  == 0
T a * -0  == 0
T -0 * a  == 0
# fixed bug
T a + a * a - a * a == a
# fixed bug for this one

#### Test canonical ordering, sorting

T Apply(List, zb + za + 7 + 2 * x^r  + 2 + a + c + x^3 + x^2) ==  [9,a,c,x^2,x^3,2*x^r,za,zb]
T Apply(List, z^7 + 2 * z^6 + 5*z^3 + 10 *z  + 1) == [1,10*z,5*z^3,2*z^6,z^7]
T Apply(List, a * a + 1 / ((z + y) * (z + y)) ) == [a ^ 2,(y + z) ^ -2]
T Apply(List,a + z + 10*a^2 + 2 * z^2) == [a,10*a^2,z,2*z^2]
T (a+b)/(a+b) == 1
T (a + b)^2/(a+b) == a + b
T (a^2)^2 == a^4
T (a^t)^2 == a^(2*t)
T (a^2)^t == a^(2*t)
T Part((a^t)^z,2) == z
T Apply(List,( y + z  + 1) * ( y + z)) == [(y + z),1 + y + z]  # bug fix

# FIXME: this should return true.
# T Apply(List, a + b*a + b) == [a,b,a*b]


# Blank's are less than non-Blanks
T Apply(List, a_ + x) == List(x,a_)
T Apply(List, a_h + x) == List(x,a_h)
T Apply(List, a__ + x) == List(x,a__)
T Apply(List, a__h + x) == List(x,a__h)
T Apply(List, a_ + _x) == List(_x,a_)
T Apply(List, x_ + _x) == List(_x,x_)
T Apply(List, _ + __) == List(_,__)
T Apply(List, a + _x) == List(a,_x)

 ClearAll(f,x)
T Apply(List, x + f(x_)) == List(x,f(x_))
T Apply(List, _ + f(x_)) == List(_,f(x_))
T Apply(List, x_ + f(x)) == List(f(x),x_)
T Apply(List, x_ + f(x) + _) == List(_,f(x),x_)
T Apply(List, x_y + f(x) + _ + _y) == List(_,_y,f(x),x_y)
 ClearAll(f,x)

# bug present in 422b42d34e4c0c6e0f377c85d1d2e78b9c1bbb19 and earlier
 ClearAll(m,a,b,c)
 m = a*b + c
 -m      # must not change the value of m
T m == a*b + c
 ClearAll(m,a,b,c)


#### Test do_canon_power!

 Clear(a,b,m)
 m = (2*a*b)^3
T Head(m) == Times
T Apply(List,m) == List(8,a^3,b^3)
 Clear(a,b,m)

 m = ExpandA((2*a+2*b)^BI(4))
T Apply(List,m) == [(16*(a^4)),(64*(a^3)*b),(96*(a^2)*(b^2)),(64*a*(b^3)),16*(b^4)]

 m = ExpandA((2*a+2*b)^4)
T Apply(List,m) == [(16*(a^4)),(64*(a^3)*b),(96*(a^2)*(b^2)),(64*a*(b^3)),16*(b^4)]

# bug through commit e9c6803de51dab446a1f3e9bcc8d1e8cf19941d3
# Causes infinite evaluation loop, due to ill-defined order.
 m = (ExpandA((x+1)^2))^2
 m * (m+1)

 ClearAll(a,A,Z,z)

T Apply(List,a*Z) == [a,Z]
T Apply(List,A*z) == [A,z]
T Apply(List,A*a) == [a,A]

T Apply(List,(x^2)*(1//2) + x) == [x,(1//2) * (x ^ 2)]

 ClearAll(a,b,m,t,x,r,z,za,zb,c,h,y,z,A,Z)

#### Float contagion

T Head(3 * π) == Times
T Head(3.0 * π) == Float64


#T Apply(List, a___ + x) == List(x,a___)  # BlankNullSequence not implemented

T testUserSyms


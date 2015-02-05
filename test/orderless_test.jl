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
@testex (a^t)^z == a^(t*z)
@testex Apply(List,( y + z  + 1) * ( y + z)) == [(y + z),1 + y + z]  # bug fix

# Blank's are less than non-Blanks
@testex Apply(List, a_ + x) == List(x,a_)
@testex Apply(List, a_h + x) == List(x,a_h)
@testex Apply(List, a__ + x) == List(x,a__)
@testex Apply(List, a__h + x) == List(x,a__h)
@testex Apply(List, a_ + _x) == List(_x,a_)
@testex Apply(List, _ + __) == List(_,__)
@testex Apply(List, a + _x) == List(a,_x)

## Test do_canon_power!
@ex Clear(a,b,m)
@ex m = (2*a*b)^3
@testex Head(m) == Times
@testex Apply(List,m) == List(8,a^3,b^3)

#@testex Apply(List, a___ + x) == List(x,a___)  # BlankNullSequence not implemented


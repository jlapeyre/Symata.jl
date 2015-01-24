# fixed bug for this one
@testex Apply(List, zb + za + 7 + 2 * x^r  + 2 + a + c + x^3 + x^2) ==  [9,2*a,x^2,x^3,2*x^r,za,zb]
@testex Apply(List,z^7 + 2 * z^6 + 5*z^3 + 10 *z  + 1) == [1,10*z,5*z^3,2*z^6,z^7]
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


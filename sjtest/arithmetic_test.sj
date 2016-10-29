T Head(1//1) == Int64
T Head(3//1) == Int64

 ClearAll(a,b,ar)
T Head(Re(a)) == Re
T Re(I*a)[2] == Im(a)
T Im(I*a) == Re(a)
T Im(I*a*b) == Re(a*b)
T Re(I*a*b) == -Im(a*b)
T Re(2*a) == 2*Re(a)
T Im(2*a) == 2*Im(a)

T 1/0 == DirectedInfinity()
T 1//0 == DirectedInfinity()
T 1/DirectedInfinity() == 0
T DirectedInfinity()^(2) == DirectedInfinity()
T DirectedInfinity()^(-2) == 0
T DirectedInfinity() * a * 0 == Indeterminate
T ComplexInfinity^(-2) == 0
T Infinity^(-2) == 0
T  Apply(Times, [DirectedInfinity(),a,a,a,0]) == Indeterminate
T  0 * Indeterminate == Indeterminate
T  0 * Infinity == Indeterminate
T  0 * ComplexInfinity == Indeterminate
T  1/0 == ComplexInfinity
T  a * 3 * ComplexInfinity == ComplexInfinity

T :( mpow(1,0) == 1 )
T :( mpow(1,1) == 1 )
# Do we want the following ? Or better 1
T :( mpow(1,-1) == 1//1 )
T :( mpow(2,1//2) == mxpr(:Power, 2, 1//2) )
T :( mpow(2,1//2) == mxpr(:Power, 2, 1//2) )
# Fix these too.
T :( mpow(2,-1//2) == mxpr(:Power, 2, -1//2) )
T :( mpow(4,1//2) == 2 )
T :( mpow(4,-1//2) == 1//2 )
T :( mpow(7^3,1//2) == mxpr(:Times, 7, mxpr(:Power, 7, 1//2)) )
T :(  mpow(8*5,1//2) == mxpr(:Times, 2, mxpr(:Power, 2, 1//2), mxpr(:Power, 5, 1//2)) )
T :( mpow(9, 2//3) == mxpr(:Times, 3, mxpr(:Power, 3, 1//3)) )
T :( mpow(-9,2//3) == mxpr(:Times, 3, mxpr(:Power, 3, 1//3), mxpr(:Power,-1,2//3)))

T Sqrt(-1) == I      # fixes bug in mpow{T<:Integer, V<:Integer}(x::T,y::Rational{V})
T Sqrt(-1)^2 == -1   # same bug

 ClearAll(a,b)

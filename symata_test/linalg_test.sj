ClearAll(mat,imat,idmat,a,b,c)

mat = [[1,2], [3,4]]
imat = Inverse(mat)
T imat == [[-2,1],[3/2,-1/2]]

T Dot([1,2,3], [a,b,c]) == a + 2b + 3c

idmat = IdentityMatrix(5)
T idmat == [[1,0,0,0,0],[0,1,0,0,0],[0,0,1,0,0],[0,0,0,1,0],[0,0,0,0,1]]
T Dot(idmat, [1,2,3,4,5]) == [1,2,3,4,5]
T Dot([[a,b], [c,d]] , [e,f]) == [a*e + b*f,c*e + d*f]

T Transpose([[1,2,c], [3,4,5]]) == [[1,3],[2,4],[c,5]]
T Tr(IdentityMatrix(3)) == 3

T ZeroMatrix(3,2) == [[0,0],[0,0],[0,0]]

Transpose( [ [a,b], [c,d]] ) == [[a,c],[b,d]]

Eigenvalues( [ [a,b], [c,d]] ) == [[(1/2)*a + (1/2)*d + (-1/2)*((a^2 + 4b*c - 2a*d + d^2)^(1/2)),1],[(1/2)*a + (1/2)*d + (1/2)*((a^2 + 4b*c - 2a*d + d^2)^(1/2)),1]]

## FIXME: disabled this because
## 1. I commented out the artithmetic methods in sympy.jl becuase they conflict
##    with new methods in PyCall
## 2. Inverse(mat) returns a List of python objects that are not translated
##    by ToSymata.  Another new thing, no doubt
# mat = [[a,b],[c,d]]
# T Simplify(Dot(mat,Inverse(mat))) == IdentityMatrix(2)

ClearAll(mat,imat,idmat,a,b,c,f,e,d)

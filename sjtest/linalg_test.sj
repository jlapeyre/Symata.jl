ClearAll(mat,imat,a,b,c)

mat = [[1,2], [3,4]]
imat = Inverse(mat)
T imat == [[-2,1],[3/2,-1/2]]

T Dot([1,2,3], [a,b,c]) == a + 2b + 3c

idmat = IdentityMatrix(5)
T idmat == [[1,0,0,0,0],[0,1,0,0,0],[0,0,1,0,0],[0,0,0,1,0],[0,0,0,0,1]]
T Dot(idmat, [1,2,3,4,5]) == [1,2,3,4,5]
T Transpose([[1,2,c], [3,4,5]]) == [[1,3],[2,4],[c,5]]

ClearAll(mat,imat)


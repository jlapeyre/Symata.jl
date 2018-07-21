# http://stackoverflow.com/questions/6470625/mathematica-table-function?noredirect=1&lq=1

T (Function(x , x[2,i]) ./ (Map(List, Thread(HoldPattern(i) => Range(5))))) == [x -> x[2,1],x -> x[2,2],x -> x[2,3],x -> x[2,4],x -> x[2,5]]


## This is just held here so it can go in the stackexchange notebook
# C = [a,[[a1,[a12,b12,c12]],[b2,[a22,b22,c22]],[c3,[a32,b32,c32,d32]]]];
# Replace(C , [x_,[y__]] => [x,y], [2])

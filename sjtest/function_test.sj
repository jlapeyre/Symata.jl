T Function(x,x^2)(3) == 9
T Function(x,x^2)(y) == y^2
T Function(x,y)(z) == y
T Function(x,3)(z) == 3
T Function([u,v],3)(z) == 3
T Function([u,v],x)(z) == x
T Function([x,y],x+y)(2,3) == 5
T Function([x,y],x+y)(u,v) == u+v
T Array(Function([x,y],x^2+y^2), [3,3]) == [[2,5,10],[5,8,13],[10,13,18]]

T Map(Function(x,x^2), [1,2,c]) == [1,4,c^2]

## Syntax
T ([x,y] -> x+y)(2,3) == 5
T ( x -> x^2)(a) == a^2
T Function(x,x) == x -> x
T Function(x,x^2) == x -> x^2
T Function(x,(3,x^2)) == x -> (3,x^2)
T Function([x,y],(3,x^2+y)) == [x,y] -> (3,x^2+y)

Apply(ClearAll,UserSyms())

T Function(x,x^2)(3) == 9
T Function(x,x^2)(y) == y^2
T Function(x,y)(z) == y
T Function(x,3)(z) == 3
T Function([u,v],3)(z) == 3
T Function([u,v],x)(z) == x
T Function([x,y],x+y)(2,3) == 5
T Function([x,y],x+y)(u,v) == u+v
T Array(Function([x,y],x^2+y^2), [3,3]) == [[2,5,10],[5,8,13],[10,13,18]]

# Syntax
T ([x,y] -> x+y)(2,3) == 5
T ( x -> x^2)(a) == a^2


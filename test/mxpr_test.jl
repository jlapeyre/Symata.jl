using Base.Test

## SetDelay for SJSym
@ex Clear(a,b,c)
@testex a == a
@test  @ex(a == b) == false
@testex  a != b
@testex  a == a != b
@testex  (a != a == b) == false
@ex (a = 1)
@ex (b = a)
@ex (c := a)
@ex (a = 2)
@test @ex(b) == 1
@test @ex(c) == 2
@ex Clear(a,b,res)
@ex res = a * b
@ex a = 1
@testex res == b   # test fix for bug in commit 6565
@ex Clear(a,b,c,g)
@ex a = g
@testex a(b) == g(b)
@ex Clear(a,g)

# Test non-Symbol Head for Mxpr
@ex ClearAll(a,c,f,b)
@ex      a = f(c)     
@ex      b = ReplaceAll(a, f => 4)  # Parser will not allow us to construct 4(c)
@testex  Head(b) == 4
@testex  Head(Apply(5.0,[1,2,3])) == 5.0
@ex ClearAll(a,c,f,b)

@ex ClearAll(g,h,x,m,y)
@ex m = [g,h]
@ex g(x_) := x^2
@ex h(x_) := x^3
@testex m[1](y) == y^2
@testex m[2](y) == y^3
@ex ClearAll(g,h,x,m,y)

# Test replacement
@ex ClearAll(a,b,c,g)

@ex cossinrule = Cos(x_)^2 + Sin(x_)^2 => 1

# Does not match
@test @ex(Replace( Cos(a+b)^2 + Sin(a+c)^2, cossinrule)) == @ex(Cos(a+b)^2 + Sin(a+c)^2)

# Does not match
@test @ex(Replace( Cos(a+b)^2 + Sin(a+b)^2, cossinrule)) == 1

# One level down. Does not match
@testex Replace( g(Cos(a+b)^2 + Sin(a+b)^2), cossinrule) == g(Cos(a + b)^2 + Sin(a + b)^2)

# Match at every level
@testex ReplaceAll( g(Cos(a+b)^2 + Sin(a+b)^2), cossinrule) == g(1)

@ex ClearAll(cossinrule)

@testex Replace( a , a => 1) == 1
@testex Replace( b , a => 1) == b
#@ex ClearAll(a,b)

## Set a Julia variable
@ex(SetJ(a,"cat"))
@test Main.a == "cat"

## Test compound expression

@ex Clear(a)
@ex res = ( a = 3, 4)
@testex a == 3
@testex res == 4
@ex Clear(a,res)

@ex ClearAll(f,a,b)
@ex f(x_) := ( a = 1, x + 1 )
@testex   f(b) == b + 1
@ex ClearAll(f,a,b)

## Special rule

@testex Cos(ACos(x)) == x
@testex Attributes(Plus)  == [Flat,Listable,NumericFunction,OneIdentity,Orderless,Protected]

## Orderless

@ex ClearAll(f,a,c,z)
@testex Apply(List,f(z,c,a)) == [z,c,a]
@ex ClearAll(f,a,c,z)

## Listable

@ex ClearAll(a,b,c,d,e,f,g,y,z)
@ex SetAttributes(f,Listable)
@testex f([a,b,c])  == [f(a),f(b),f(c)]
@testex f([a,b,c],d) == [f(a,d),f(b,d),f(c,d)]
@testex f(z,[a,b,c],d) == [f(z,a,d),f(z,b,d),f(z,c,d)]
@testex f([a,b,c],[d,e,f]) == [f(a,d),f(b,e),f(c,f)]
@testex f([a,b,c],[d,e,f],g) == [f(a,d,g),f(b,e,g),f(c,f,g)]
@testex f([a,b,c],z,[d,e,f]) == [f(a,z,d),f(b,z,e),f(c,z,f)]
@testex f(y,[a,b,c],z,[d,e,f]) == [f(y,a,z,d),f(y,b,z,e),f(y,c,z,f)]
@ex ClearAll(f,a,b,c,d,e,f,g,x,y,z,res)

## For
@ex m = [0,0,0]
@ex For(i=1, i <= 3, i = i + 1, m[i] = i + 1)
@ex res = ([2,3,4] == m)
@test symval(:res) == true
@ex ClearAll(m,i,res)

@ex ClearAll(f)
@ex f(x_) := Module([], (If(x < 10, x*f(x+1), x),))
@testex f(1) == 3628800
@testex f(9) == 90
@testex f(10) == 10
@testex f("dog") == "dog"
@ex ClearAll(f,x)

## While

@ex ClearAll(i)
@ex i = 0
@ex While(i < 5, i = i + 1)
@testex i == 5
@ex ClearAll(i)

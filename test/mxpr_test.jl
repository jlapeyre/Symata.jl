using Base.Test

@ex If( Length(UserSyms()) > 0 ,  Println("**********", UserSyms()))
@testex Length(UserSyms()) == 0

# Fails in 0.5.0-dev+3385, probably because the parser has changed
# ==(2,2) is an expression with Head '=='
@testex [1,2,3][2] == 2

## Head of Mxpr is Julia function

SJulia.@ex ClearAll(f,g,x,c)
SJulia.@ex       g = :( fftest(x) = x^2 ) # g is SJulia Symbol bound to Julia Function
# Creates Mxpr with head of type Function.
# The apprule for head Function is to call it on the args
SJulia.@testex   g(3) == 9
# The following works because we have defined ^ for Symbols in Julia
SJulia.@testex  g(c) == c^2

@ex ClearAll(f,g,x,c)

## SetDelay for SJSym
SJulia.@ex Clear(a,b,c)
@testex a == a
@testex Apply(List, a < a) == [a,<,a]
@testex Apply(List, a > a) == [a,>,a]
@testex Apply(List, a < b) == [a,<,b]
@testex Apply(List, a > b) == [a,>,b]
@testex  (a == b) != False
@testex  (a != b) != True
@testex  (a != b) != False
@testex (1 < 2) == True
@testex (1 < 2 < 3) == True
@testex (1 < 3 < 2) == False
# This should perhaps evaluate to  a != b
SJulia.@testex  (a == a != b) != True
@testex  (a != a == b) == False
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
SJulia.@ex(SetJ(a,"cat"))
SJulia.@test SJulia.a == "cat"

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

@testex Cos(ArcCos(x)) == x
@testex Attributes(Plus)  == [Flat,Listable,NumericFunction,OneIdentity,Orderless,Protected]

## Orderless

@ex ClearAll(f,a,c,z)
@testex Apply(List,f(z,c,a)) == [z,c,a]
@ex ClearAll(f,a,c,z,g,res,x)

@ex If( Length(UserSyms()) > 0 ,  Println("**********", UserSyms()))
@testex Length(UserSyms()) == 0

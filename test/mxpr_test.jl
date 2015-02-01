using Base.Test

## These are a few tests while we reorganize the code.

## SetDelay for SJSym
@ex Clear(a,b,c)
@testex a == a
@test  @ex(a == b) == false
@testex  a != b
@testex  a == a != b
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
@ex Clear(a,b,c)

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

@ex ClearAll(f,a,b)
@ex f(x_) := ( a = 1, x + 1 )
@testex   f(b) == b + 1
@ex ClearAll(f,a,b)

## Special rule

#@testex Cos(ACos(x)) == x
@testex Cos(ACos(x)) == x

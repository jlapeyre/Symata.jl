# Fails in 0.5.0-dev+3385, probably because the parser has changed
T ==(2,2)  #  is an expression with Head '=='
T [1,2,3][2] == 2

## Head of Mxpr is Julia function

 ClearAll(f,g,x,c)
# We use an anonymous function to avoid the redefinition warning if this test
# is run more than once.
       g = :( (x) -> mpow(x,2) ) # g is SJulia Symbol bound to Julia Function
# Creates Mxpr with head of type Function.
# The apprule for head Function is to call it on the args
T   g(3) == 9
T  g(c) == c^2

 ClearAll(f,g,x,c)

## SetDelay for SJSym
 Clear(a,b,c)
T a == a

## Remove these, I think
# T  (a == b) != False
# T  (a != b) != True
# T  (a != b) != False
T Head(a == b) == Equal
T Head(a != b) == Unequal

T (1 < 2) == True
T (1 < 2 < 3) == True
T (1 < 3 < 2) == False
T  Head(a == a != b) == Unequal
T  (a != a == b) == False
 (a = 1)
 (b = a)
 (c := a)
 (a = 2)
T (b) == 1
T (c) == 2
 Clear(a,b,res)
 res = a * b
 a = 1
T res == b   # test fix for bug in commit 6565
 Clear(a,b,c,g)
 a = g
T a(b) == g(b)
 Clear(a,g)

# Test non-Symbol Head for Mxpr
 ClearAll(a,c,f,b)
      a = f(c)
      b = ReplaceAll(a, f => 4)  # Parser will not allow us to construct 4(c)
T  Head(b) == 4
T  Head(Apply(5.0,[1,2,3])) == 5.0
 ClearAll(a,c,f,b)

 ClearAll(g,h,x,m,y)
 m = [g,h]
 g(x_) := x^2
 h(x_) := x^3
T m[1](y) == y^2
T m[2](y) == y^3
 ClearAll(g,h,x,m,y)

# Test replacement
 ClearAll(a,b,c,g)

 cossinrule = Cos(x_)^2 + Sin(x_)^2 => 1

# Does not match
T (Replace( Cos(a+b)^2 + Sin(a+c)^2, cossinrule)) == (Cos(a+b)^2 + Sin(a+c)^2)

# Does not match
T (Replace( Cos(a+b)^2 + Sin(a+b)^2, cossinrule)) == 1

# One level down. Does not match
T Replace( g(Cos(a+b)^2 + Sin(a+b)^2), cossinrule) == g(Cos(a + b)^2 + Sin(a + b)^2)

# Match at every level
T ReplaceAll( g(Cos(a+b)^2 + Sin(a+b)^2), cossinrule) == g(1)

 ClearAll(cossinrule)

T Replace( a , a => 1) == 1
T Replace( b , a => 1) == b
# ClearAll(a,b)

## Set a Julia variable
(SetJ(a,"cat"))
T :( Main.a == "cat" )

## Test compound expression

 Clear(a)
 res = ( a = 3, 4)
T a == 3
T res == 4
 Clear(a,res)

 ClearAll(f,a,b)
 f(x_) := ( a = 1, x + 1 )
T   f(b) == b + 1
 ClearAll(f,a,b)

## Special rule

T Cos(ArcCos(x)) == x
T Attributes(Plus)  == [Flat,Listable,NumericFunction,OneIdentity,Orderless,Protected]

## Orderless

 ClearAll(f,a,c,z)
T Apply(List,f(z,c,a)) == [z,c,a]
 ClearAll(f,a,c,z,g,res,x)

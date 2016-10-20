#### Fixed point evaluation

# Test that timestamp of symbols that an expression depends on are
# tracked and used correctly. This means, in particular that getting
# one part of a large expression that is 'fixed' say the part m[1] of
# m, requires evaluating neither m nor m1, but merely retrieving m[1].

T testUserSyms

 m = ExpandA((a+b)^2)
T Syms(m) == HoldForm([a,b])
 m = ExpandA((a*c + b*d)^2)
# Following depends on whether we allow protected symbols in free symbol list
# T Syms(m) == HoldForm([a,b,c,d])
T Syms(m) == HoldForm([Times,a,b,c,d])
T FixedQ(m)
 a = 1
 Length(m)
T FixedQ(m) == false
 Clear(a)
 Length(m)
T FixedQ(m)
 m = Range(10) # depends on no symbols, should never be dirty.
T DirtyQ(m) == false
 Clear(m)

## More testing of dirty symbols

# Note that this fails for Sin, because it is done by SymPy

 Clear(a)
 m = Cos(3*a)
 a = Pi
T m == -1

# Sin is done atm by sympy. we call mergeargs once in sympy.jl
# We will probably need recursive call for deeper expressions, like Integrate with conditions.
 Clear(a)
 m = Sin(Pi*a)
 a = 1/6
T m == 1/2

 ClearAll(a,b,c,d,m)

#### Listable

 ClearAll(a,b,c,d,e,f,g,y,z)
 SetAttributes(f,Listable)
T f([a,b,c])  == [f(a),f(b),f(c)]
T f([a,b,c],d) == [f(a,d),f(b,d),f(c,d)]
T f(z,[a,b,c],d) == [f(z,a,d),f(z,b,d),f(z,c,d)]
T f([a,b,c],[d,e,f]) == [f(a,d),f(b,e),f(c,f)]
T f([a,b,c],[d,e,f],g) == [f(a,d,g),f(b,e,g),f(c,f,g)]
T f([a,b,c],z,[d,e,f]) == [f(a,z,d),f(b,z,e),f(c,z,f)]
T f(y,[a,b,c],z,[d,e,f]) == [f(y,a,z,d),f(y,b,z,e),f(y,c,z,f)]
 ClearAll(f,a,b,c,d,e,f,g,x,y,z,res)

#### HoldAll

f(x_) := Hold(x)
T f(2^2) == Hold(4)
SetAttributes(f, HoldAll)
T f(2^2) == Hold(2^2)

ClearAll(f,x)


#### NHoldFirst

SetAttributes(f,NHoldFirst)
SetAttributes(g,NHoldRest)
SetAttributes(h,NHoldAll)

T  Args(Map( :( (x) -> typeof(x) <: Integer ) , N(f(1,2,3)))) == [True,False,False]
T  Args(Map( :( (x) -> typeof(x) <: Integer ) , N(g(1,2,3)))) == [False,True,True]
T  Args(Map( :( (x) -> typeof(x) <: Integer ) , N(h(1,2,3)))) == [True,True,True]

 ClearAll(f,g,h,x)

#### Flat

SetAttributes(g,Flat)
T g(g(x)) == g(x)
T [f(f(x)), f(g(g(1,2),3))] == [f(f(x)),f(g(1,2,3))]

ClearAll(f,g,x)

T testUserSyms

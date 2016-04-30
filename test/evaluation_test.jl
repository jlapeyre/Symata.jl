#### Fixed point evaluation

@testex Length(UserSyms()) == 0
# Test that timestamp of symbols that an expression depends on are
# tracked and used correctly. This means, in particular that getting
# one part of a large expression that is 'fixed' say the part m[1] of
# m, requires evaluating neither m nor m1, but merely retrieving m[1].

@ex Clear(m,a,b,c,d)
@ex m = ExpandA((a+b)^2)
@testex Syms(m) == HoldForm([a,b])
@ex m = ExpandA((a*c + b*d)^2)
# Following depends on whether we allow protected symbols in free symbol list
# @testex Syms(m) == HoldForm([a,b,c,d])
@testex Syms(m) == HoldForm([Times,a,b,c,d])
@testex Fixed(m)
@ex a = 1
@ex Length(m)
@testex Fixed(m) == false
@ex Clear(a)
@ex Length(m)
@testex Fixed(m)
@ex m = Range(10) # depends on no symbols, should never be dirty.
@testex DirtyQ(m) == false
@ex Clear(m)

## More testing of dirty symbols

# Note that this fails for Sin, because it is done by SymPy

@ex Clear(a)
@ex m = Cos(3*a)
@ex a = Pi
@testex m == -1

# Sin is done atm by sympy. we call mergeargs once in sympy.jl
# We will probably need recursive call for deeper expressions, like Integrate with conditions.
@ex Clear(a)
@ex m = Sin(Pi*a)
@ex a = 1/6
@testex m == 1/2

@ex ClearAll(a,b,c,d,m)

#### Check Listable

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

#### NHoldFirst

@ex SetAttributes(f,NHoldFirst)
@ex SetAttributes(g,NHoldRest)
@ex SetAttributes(h,NHoldAll)

@testex  Args(Map( :( (x) -> typeof(x) <: Integer ) , N(f(1,2,3)))) == [True,False,False]
@testex  Args(Map( :( (x) -> typeof(x) <: Integer ) , N(g(1,2,3)))) == [False,True,True]
@testex  Args(Map( :( (x) -> typeof(x) <: Integer ) , N(h(1,2,3)))) == [True,True,True]

@ex ClearAll(f,g,h,x)

@testex Length(UserSyms()) == 0

#### Fixed point evaluation

# Test that timestamp of symbols that an expression depends on are
# tracked and used correctly. This means, in particular that getting
# one part of a large expression that is 'fixed' say the part m[1] of
# m, requires evaluating neither m nor m1, but merely retrieving m[1].

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
 ClearAll(a)
 Length(m)
T FixedQ(m)
 m = Range(10) # depends on no symbols, should never be dirty.
T DirtyQ(m) == false
 ClearAll(m)

## More testing of dirty symbols

# Note that this fails for Sin, because it is done by SymPy

 ClearAll(a)
 m = Cos(3*a)
 a = Pi
T m == -1

# Sin is done atm by sympy. we call mergeargs once in sympy.jl
# We will probably need recursive call for deeper expressions, like Integrate with conditions.
 ClearAll(a)
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

### Hold vs. HoldComplete

T ToString(HoldComplete(1+1 , Evaluate(2+3),Sequence(a,b))) == "HoldComplete((1 + 1),Evaluate(2 + 3),Sequence(a,b))"
T ToString(Hold(1+1 , Evaluate(2+3),Sequence(a,b))) == "Hold((1 + 1),5,a,b)"
T HoldComplete(f(1 + 2)) ./ ( f(x_) :> g(x) ) == HoldComplete(g(1 + 2))

### Unevaluated

## Symata Unevaluated is not exactly the same as Mma. Maybe it is worth it at some point to make it so.

T Length(Unevaluated(1+2+3)) == 3
T ToString(Hold(Evaluate(Unevaluated(1+2)))) == "Hold(Unevaluated(1 + 2))"
ClearAll(f)
T f(Unevaluated(1+2)) == f(3)
SetAttributes(f,HoldAll)
## NOTE: Mma says that Unevaluated remains in a held function. In fact, in Mma
## In fact, without HoldAll, it still remains.
## Mma give true for following.
## T (f(Unevaluated(1 + 2))[1]) == Unevaluated
T ToString(f(Unevaluated(1 + 2))) == "f(Unevaluated(1 + 2))"
ClearAll(f)

T Length(Unevaluated(Sequence(1,2,b))) == 3
f(x_) := g(x)
f(Unevaluated(1+1)) == g(2)

SetAttributes(symlength, HoldAll)
symlength(s_Symbol) := StringLength(SymbolName(Unevaluated(s)))
aaaaa = 1
T symlength(aaaaa) == 5

#### NHoldFirst

SetAttributes(f,NHoldFirst)
SetAttributes(g,NHoldRest)
SetAttributes(h,NHoldAll)

T  Args(Map( J( (x) -> typeof(x) <: Integer ) , N(f(1,2,3)))) == [True,False,False]
T  Args(Map( J( (x) -> typeof(x) <: Integer ) , N(g(1,2,3)))) == [False,True,True]
T  Args(Map( J( (x) -> typeof(x) <: Integer ) , N(h(1,2,3)))) == [True,True,True]

 ClearAll(f,g,h,x)

### Flat

SetAttributes(g,Flat)
T g(g(x)) == g(x)
T [f(f(x)), f(g(g(1,2),3))] == [f(f(x)),f(g(1,2,3))]

T Depth(g(g(x))) == 2
ClearAttributes(g,Flat)
T Depth(g(g(x))) == 3


ClearAll(f,g,x)

### SequenceHold

T g(a,b,Sequence(c,d)) == g(a,b,c,d)
SetAttributes(g, SequenceHold)
T Length(g(a,b,Sequence(c,d))) == 3

T ReplaceAll([w,w,w], w => Sequence(x,y)) == [x,y,x,y,x,y]

T ReplaceAll(f([[x,y],[u,[v,w]]]),  List => Sequence) == f(x,y,u,v,w)

Apply(ClearAll,UserSyms())

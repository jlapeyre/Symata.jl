Apply(ClearAll, UserSyms())

### Pack

a = Pack([1,2,3])

T  If( BigIntInput() , J( eltype(Symata.symval(:a)) == BigInt ), J( eltype(Symata.symval(:a)) == Int ))

func = J((x,y) -> sum(u -> u[1]^2*u[2]^3, zip(margs(x),margs(y))))

#T J( eltype(Symata.symval(:a)) == Int )

a = Pack([1,2,3.0])
T J( eltype(Symata.symval(:a)) == Real )

a = Pack([1,2,3.0, "cat"])
T J( eltype(Symata.symval(:a)) == Any )

 J(Core.eval(Main, :(b = 1 )))

T JVar(b) == 1

T Length(Unpack( J( [1, 2, 3] ))) == 3
T Head(Unpack( J( [1, 2, 3] ))) == List

ClearAll(a,x)

### J

# We are in Test module or something ?
T J( Symata.Cos )(1) == Cos(1)
T J( Symata.Cos )(1.0) == Cos(1.0)
T J( Symata.Cos )(1.0) == J( cos(1.0) )
T J( Symata.Cos(:a) ) == Cos(a)

func = J((x,y) -> sum(u -> u[1]^2/u[2]^3, zip(margs(x),margs(y))))
T Abs(func(Range(10.0),Range(10.0)) - 2.9289682539682538) < 10^(-8)

## Test that + in sum method has a base method that works on Mxpr.
T Head(func([a+b, c+d],[u+v,y+z])) == Plus

## Nov 2016. Add methods for * and + on symbols, etc.
## We do not import the functions from Main.
T Head(J( Symata.:+(:c,:d) )) == Plus
T Head(J( Symata.:*(:c,:d) )) == Times

T J(3) == 3
T J(3 + 2) == 5
T J( x = 1 ) == 1   ## x = 1 is rewritten from call with kw, to = 
T J( x ) == 1

### Compile

T  Compile([x], Cos(x) )(3) == Cos(3)
T  Compile([x], Cos(x) )(0.0) == 1.0
T  Compile([x], Cos(x) )(a) == Cos(a)
T  Compile([x], Exp(x)*Cos(x)*x )(1.0) == J( exp(1.0) * cos(1.0) *1.0 )

### SymataCall

Apply(ClearAll, UserSyms())

  f = SymataCall([x,y], y*x^2/m)
T f(p,z) == m^(-1)*p^2*z
  f = SymataCall(x, x^2)
T f(a) == a^2

Apply(ClearAll, UserSyms())

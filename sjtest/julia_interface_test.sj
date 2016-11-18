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

 J( eval(Main, :(b = 1 )))

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

### Compile

T  Compile( Cos(x) )(3) == Cos(3)
T  Compile( Cos(x) )(0.0) == 1.0
T  Compile( Cos(x) )(a) == Cos(a)

T  Compile( Exp(x)*Cos(x)*x )(1.0) == J( exp(1.0) * cos(1.0) *1.0 )

Apply(ClearAll, UserSyms())



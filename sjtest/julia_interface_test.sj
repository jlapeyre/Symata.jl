### Pack

a = Pack([1,2,3])

T  If( BigIntInput() , :( eltype(Symata.symval(:a)) == BigInt ), :( eltype(Symata.symval(:a)) == Int ))

#T :( eltype(Symata.symval(:a)) == Int )

a = Pack([1,2,3.0])
T :( eltype(Symata.symval(:a)) == Real )

a = Pack([1,2,3.0, "cat"])
T :( eltype(Symata.symval(:a)) == Any )

:(eval(Main, :( b = 1 )))

T JVar(b) == 1

T Length(Unpack( :( [1, 2, 3] ))) == 3
T Head(Unpack( :( [1, 2, 3] ))) == List

ClearAll(a,x)

# We are in Test module or something ?
T :( Symata.Cos )(1) == Cos(1)
T :( Symata.Cos )(1.0) == Cos(1.0)
T :( Symata.Cos )(1.0) == :( cos(1.0) )
T :( Symata.Cos(:a) ) == Cos(a)

T  Compile( Cos(x) )(3) == Cos(3)
T  Compile( Cos(x) )(0.0) == 1.0
T  Compile( Cos(x) )(a) == Cos(a)

T  Compile( Exp(x)*Cos(x)*x )(1.0) == :( exp(1.0) * cos(1.0) *1.0 )

ClearAll(a)


